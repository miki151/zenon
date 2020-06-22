#include "context.h"
#include "ast.h"
#include "type.h"
#include "identifier_type.h"

Context Context::withParent(const Context& c) {
  Context ret(c.typeRegistry);
  ret.parentStates = c.parentStates;
  ret.parentStates.push_back(c.state);
  return ret;
}

Context Context::withParent(vector<const Context*> parents) {
  Context ret(parents[0]->typeRegistry);
  for (auto context : parents) {
    append(ret.parentStates, context->parentStates);
    ret.parentStates.push_back(context->state);
  }
  return ret;
}

void Context::merge(const Context& context) {
  /*for (auto& t : context.getAllTypes()) {
    auto other = getType(t->getName(false));
    CHECK(!other || other.get() == t) << other->getName();
  }*/
  for (auto& s : context.parentStates)
    if (!contains(parentStates, s) && state != s)
      parentStates.push_back(s);
  if (!contains(parentStates, context.state) && state != context.state)
    parentStates.push_back(context.state);
}

Context::Context(TypeRegistry* t) : typeRegistry(t), state(shared<State>()) {
}

void Context::State::merge(const Context::State& o) {
  for (auto& f : o.functions) {
    if (!functions.count(f.first))
      functions.insert(f);
  }
}

void Context::deepCopyFrom(const Context& c) {
  CHECK(parentStates.empty());
  *state = *c.state;
  for (auto s : c.parentStates)
    state->merge(*s);
}

bool Context::areParamsEquivalent(FunctionType f1, FunctionType f2) const {
  if (f1.templateParams.size() != f2.templateParams.size() || f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.templateParams.size(); ++i) {
    auto tParam = f2.templateParams[i];
    ErrorBuffer errors;
    f2 = replaceInFunction(std::move(f2), std::move(tParam), f1.templateParams[i], errors);
    if (!errors.empty())
      return false;
  }
  return f1.requirements == f2.requirements && f1.params == f2.params;
}

optional<vector<SType>> Context::getTemplateParams() const {
  for (auto& state : getReversedStates())
    if (!state->templateParams.empty())
      return state->templateParams;
  return none;
}

void Context::setTemplated(vector<SType> v) {
  state->templateParams = v;
}

static SType getCapturedType(SType input, LambdaCaptureType type) {
  switch (type) {
    case LambdaCaptureType::REFERENCE:
      return convertReferenceToPointer(input);
    case LambdaCaptureType::COPY:
    case LambdaCaptureType::IMPLICIT_COPY:
    case LambdaCaptureType::MOVE:
      return input->removeReference();
  }
}

WithErrorLine<vector<LambdaCapture>> Context::setLambda(LambdaCaptureInfo* info) {
  vector<LambdaCapture> ret;
  for (auto& capture : info->captures) {
    if (auto type = getTypeOfVariable(capture.name)) {
      auto underlying = type->get()->removeReference();
      if (capture.type == LambdaCaptureType::IMPLICIT_COPY && !canConvert(*type, underlying))
        return capture.codeLoc.getError("Variable " + capture.name + " of type " +
            quote(underlying->getName()) + " can't be captured by implicit copy");
      if (capture.type == LambdaCaptureType::COPY) {
        if (auto f = getCopyFunction(*this, capture.codeLoc, underlying))
          TRY(f->get()->getDefinition()->addInstance(this, *f));
        else
          return capture.codeLoc.getError("No copy function defined for type " + quote(underlying->getName())+ "\n" + f.get_error().error);
      }
      ret.push_back(LambdaCapture{capture.name, getCapturedType(*type, capture.type), capture.type});
    } else
      return capture.codeLoc.getError(type.get_error());
  }
  state->lambdaInfo = info;
  return std::move(ret);
}

bool Context::isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific,
    vector<FunctionType> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, specific->type.params,
      vector<CodeLoc>(specific->type.params.size(), CodeLoc()), existing)) {
    if (specific->type.retVal == inst.get()->type.retVal)
      return true;
  }
  return false;
}

WithError<vector<SFunctionInfo>> Context::getRequiredFunctions(const Concept& required,
    vector<FunctionType> existing) const {
  vector<SFunctionInfo> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        auto getFunction = [&]() -> optional<SFunctionInfo> {
//          std::cout << "Looking for " << function->prettyString() << std::endl;
          for (auto& myFun : getFunctions(overloads.first)) {
//            std::cout << "Considering " << myFun->prettyString() << std::endl;
            if (isGeneralization(myFun, function, existing))
              return myFun->getWithoutRequirements();
          }
          if (overloads.first == "invoke"s)
            if (!function->type.params.empty())
              if (auto lambda = function->type.params[0]->removePointer().dynamicCast<LambdaType>())
                if (isGeneralization(lambda->functionInfo.get(), function, existing))
                  return lambda->functionInfo->getWithoutRequirements();
          return none;
        };
        if (auto res = getFunction())
          ret.push_back(*res);
        else
          return "Required function not implemented: " + function->prettyString() +
              ", required by concept: " + quote(required.getName());
      }
  }
  return ret;
}

Context Context::withStates(TypeRegistry* t, ConstStates states) {
  Context ret(t);
  ret.parentStates = std::move(states);
  return ret;
}

WithError<SType> Context::getTypeOfVariable(const string& id) const {
  LambdaCaptureInfo* lambdaInfo = nullptr;
  for (auto& state : getReversedStates()) {
    if (state->vars.count(id)) {
      if (lambdaInfo) {
        if (auto info = lambdaInfo->find(id)) {
          switch (info->type) {
            case LambdaCaptureType::REFERENCE:
              return state->vars.at(id).type;
            case LambdaCaptureType::MOVE:
            case LambdaCaptureType::COPY:
            case LambdaCaptureType::IMPLICIT_COPY:
              return (SType) ReferenceType::get(state->vars.at(id).type->removeReference());
          }
        } else {
          if (auto captureType = lambdaInfo->defaultCapture) {
            auto& varInfo = state->vars.at(id);
            lambdaInfo->captures.push_back(LambdaCaptureInfo::Var{id, varInfo.declarationLoc, *captureType, false});
            lambdaInfo->implicitCaptures.push_back(LambdaCapture{id, getCapturedType(varInfo.type, *captureType), *captureType});
            return varInfo.type;
          } else
            return "Variable " + quote(id) + " is not captured by lambda";
        }
      } else
        return state->vars.at(id).type;
    }
    if (state->lambdaInfo)
      lambdaInfo = state->lambdaInfo;
  }
  for (auto& state : getReversedStates())
    if (state->unexpandedVariablePack && state->unexpandedVariablePack->first == id)
      return "Parameter pack must be unpacked using the '...' operator before being used"s;
  return "Variable not found: " + id;
}

bool Context::isCapturedVariable(const string& id) const {
  for (auto& state : getReversedStates()) {
    if (state->vars.count(id))
      return false;
    if (state->lambdaInfo)
      return true;
  }
  fail();
}

void Context::addVariable(const string& ident, SType t, CodeLoc codeLoc) {
  state->vars.insert({ident, VariableInfo{t, codeLoc}});
  state->varsList.push_back(ident);
}

void Context::State::print() const {
  for (auto& varName : varsList) {
    auto& var = vars.at(varName);
    cout << "Variable " << quote(varName) << " of type " << var.type->getName() << "\n";
  }
  for (auto& function : functions) {
    cout << "Function " << quote(toString(function.first)) << " overloads: \n";
    for (auto& overload : function.second)
      cout << overload->prettyString() << "\n";
  }
  for (auto& type : types)
    cout << "Type: " << (!fullyDefinedTypes.at(type.second.get()) ? "(incomplete) " : "") <<
        type.second->getName() << "\n";
}

void Context::print() const {
  for (auto& state : getReversedStates())
    state->print();
}

vector<SType> Context::getConversions(SType type) const {
  vector<SType> ret = {type};
  if (type->isMetaType() && type != BuiltinType::ANY_TYPE)
    ret.push_back(BuiltinType::ANY_TYPE);
  auto underlying = type->removeReference();
  if (underlying == BuiltinType::INT)
    ret.push_back(BuiltinType::DOUBLE);
  if (auto ptr = underlying.dynamicCast<MutablePointerType>())
    ret.push_back(PointerType::get(ptr->underlying));
  if (underlying != BuiltinType::NULL_TYPE)
    ret.push_back(OptionalType::get(underlying));
  return ret;
}

JustError<string> Context::canConvert(SType from, SType to) const {
  unique_ptr<Expression> expr;
  return canConvert(std::move(from), std::move(to), expr);
}

JustError<string> Context::canConvert(SType from, SType to, unique_ptr<Expression>& expr) const {
  if (from == to)
    return success;
  auto fromUnderlying = from->removeReference();
  auto toUnderlying = to->removeReference();
  if (fromUnderlying == toUnderlying && toUnderlying != to &&
      (!from.dynamicCast<ReferenceType>() || !to.dynamicCast<MutableReferenceType>()))
    return success;
  if (from != fromUnderlying && to->removeReference() == fromUnderlying && fromUnderlying->isBuiltinCopyable(*this, expr))
    return success;
  if (contains(getConversions(from), to) ||
      (from == BuiltinType::NULL_TYPE && to.dynamicCast<OptionalType>() &&
          to.dynamicCast<OptionalType>()->underlying != BuiltinType::NULL_TYPE))
    return success;
  return "Can't convert type " + quote(from->getName()) + " to type " + quote(to->getName());
}

optional<int> Context::getLoopId() const {
  for (auto state : getReversedStates())
    if (auto id = state->loopId)
      return id;
  return none;
}

int Context::setIsInLoop() {
  static int loopCount = 0;
  ++loopCount;
  state->loopId = loopCount;
  return loopCount;
}

void Context::replace(SType from, SType to, ErrorBuffer& errors) {
  CHECK(state->varsList.empty());
  /*for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to, errors);
  }*/
  for (auto& function : state->functions) {
    for (auto& overload : function.second) {
      //std::cout << "Replaced " << overload->prettyString() << std::endl;
      overload = replaceInFunction(overload, from, to, errors);
      //std::cout << "To " << overload->prettyString() << std::endl;
    }
  }
  for (auto& type : state->types)
    type.second = type.second->replace(from, to, errors);
}

void Context::expand(SType from, vector<SType> to, ErrorBuffer& errors) {
  CHECK(state->varsList.empty());
  /*for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to, errors);
  }*/
  for (auto& function : state->functions) {
    for (auto& overload : function.second)
      if (overload->type.params.back() == from) {
        auto type = overload->type;
        //std::cout << "Expanded " << overload->prettyString() << std::endl;
        if (type.variadicParams) {
          type.params.pop_back();
          for (auto& t : to)
            type.params.push_back(t);
          type.variadicParams = false;
        }
        overload = FunctionInfo::getInstance(overload->id, std::move(type), overload);
        //std::cout << "To " << overload->prettyString() << std::endl;
      }
  }
  CHECK(state->types.empty());
  /*for (auto& type : state->types)
    type.second = type.second->replace(from, to, errors);*/
}

ReturnTypeChecker* Context::getReturnTypeChecker() const {
  for (auto& state : getReversedStates())
    if (state->returnTypeChecker)
      return state->returnTypeChecker;
  return nullptr;
}

void Context::addReturnTypeChecker(ReturnTypeChecker* c) {
  CHECK(!state->returnTypeChecker);
  state->returnTypeChecker = c;
}

vector<shared_ptr<const Context::State>> Context::getReversedStates() const {
  vector<shared_ptr<const Context::State>> ret { state };
  for (auto& state : reverse(parentStates))
    ret.push_back(state);
  return ret;
}

void Context::addExpandedTypePack(const string& name, vector<SType> t) {
  state->expandedTypePack = make_pair(std::move(name), std::move(t));
}

void Context::addUnexpandedTypePack(string name, SType t) {
  state->unexpandedTypePack = make_pair(std::move(name), std::move(t));
}

void Context::addUnexpandedVariablePack(string name, SType t) {
  state->unexpandedVariablePack = make_pair(std::move(name), std::move(t));
}

void Context::addExpandedVariablePack(const string& name, vector<SType> t) {
  state->expandedVariablePack = make_pair(std::move(name), std::move(t));
}

WithError<pair<string, vector<SType>>> Context::getExpandedVariablePack() const {
  for (auto& state : getReversedStates())
    if (state->expandedVariablePack)
      return *state->expandedVariablePack;
  return "No variable pack is present in this context"s;
}

optional<pair<string, vector<SType>>> Context::getExpandedTypePack() const {
  for (auto& state : getReversedStates())
    if (state->expandedTypePack)
      return state->expandedTypePack;
  return none;
}

optional<pair<string, SType> > Context::getUnexpandedTypePack() const {
  for (auto& state : getReversedStates()) {
    if (state->expandedTypePack)
      return none;
    if (state->unexpandedTypePack)
      return state->unexpandedTypePack;
  }
  return none;
}

optional<pair<string, SType> > Context::getUnexpandedVariablePack() const {
  for (auto& state : getReversedStates()) {
    if (state->expandedVariablePack)
      return none;
    if (state->unexpandedVariablePack)
      return state->unexpandedVariablePack;
  }
  return none;
}

void Context::addType(const string& name, SType t, bool fullyDefined) {
  CHECK(!getType(name));
  state->types.insert({name, t});
  state->fullyDefinedTypes.insert({t.get(), fullyDefined});
}

void Context::addSubstitution(SubstitutionInfo info) {
  state->substitutions.push_back(std::move(info));
}

vector<Context::SubstitutionInfo> Context::getSubstitutions() const {
  vector<SubstitutionInfo> ret;
  for (auto& state : getReversedStates())
    ret.append(state->substitutions);
  return ret;
}

WithErrorLine<vector<SType>> Context::getTypeList(const vector<TemplateParameterInfo>& ids, bool variadic) const {
  vector<SType> params;
  for (int i = 0; i < ids.size(); ++i) {
    auto getType = [id = ids[i]] (const Context& context, bool variadic) {
      return id.visit(
          [&](const IdentifierInfo& id) -> WithErrorLine<SType> {
            return context.getTypeFromString(id, none); },
          [&](const shared_ptr<Expression>& expr) -> WithErrorLine<SType> {
            if (auto value = expr->eval(context))
              return value->value;
            else
              return expr->codeLoc.getError("Can't evaluate expression at compile-time");
          }
      );
    };
    if (variadic && i == ids.size() - 1) {
      if (auto pack = getExpandedTypePack()) {
        for (auto& type : pack->second) {
          auto elemContext = withParent(*this);
          if (!elemContext.getType(pack->first))
            elemContext.addType(pack->first, type);
          params.push_back(TRY(getType(elemContext, true)));
        }
      }
      else if (auto pack = getUnexpandedTypePack())
        params.push_back(pack->second);
    } else
      params.push_back(TRY(getType(*this, false)));
  }
  return params;
}

void Context::addConcept(const string& name, SConcept i) {
  state->concepts.insert({name, i});
  if (i->canCreateConceptType()) {
    ErrorBuffer errors;
    for (auto& fun : i->getContext().getAllFunctions()) {
      auto typeParams = i->getParams().getSubsequence(1);
      CHECK(!!addFunction(addTemplateParams(
            replaceInFunction(fun, i->getParams()[0], ConceptType::get(i, typeParams, i->isVariadic()), errors),
            typeParams, i->isVariadic())));
    }
    CHECK(errors.empty());
  }
}

nullable<SConcept> Context::getConcept(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->concepts.count(name))
      return state->concepts.at(name);
  return nullptr;

}

nullable<SType> Context::getType(const string& s) const {
  for (auto& state : getReversedStates())
    if (state->types.count(s))
      return state->types.at(s);
  return nullptr;
}

bool Context::isFullyDefined(const Type* t) const {
  bool was = true;
  for (auto& state : getReversedStates())
    if (auto v = getValueMaybe(state->fullyDefinedTypes, t)) {
      if (*v)
        return true;
      else
        was = false;
    }
  return was;
}

void Context::setFullyDefined(const Type* t, bool s) {
  state->fullyDefinedTypes[t] = s;
}

vector<SType> Context::getAllTypes() const {
  vector<SType> ret;
  for (auto& state : getReversedStates())
    for (auto& type : state->types)
      ret.push_back(type.second);
  return ret;
}

JustError<string> Context::addImplicitFunction(FunctionId id, FunctionType type) {
  return addFunction(FunctionInfo::getImplicit(std::move(id), std::move(type)));
}

vector<SFunctionInfo> Context::getFunctions(FunctionId name) const {
  vector<SFunctionInfo> ret;
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

vector<SFunctionInfo> Context::getAllFunctions() const {
  vector<SFunctionInfo> ret;
  for (auto& state : getReversedStates())
    for (auto& overloadSet : state->functions)
      for (auto& fun : overloadSet.second)
        ret.push_back(fun);
  return ret;
}

nullable<SType> Context::getVariable(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name).type;
  return nullptr;
}

WithErrorLine<SType> Context::getTypeFromString(IdentifierInfo id, optional<bool> typePack) const {
  if (id.parts.size() != 1)
    return id.codeLoc.getError("Bad type identifier: " + id.prettyString());
  auto name = id.parts[0].name;
  nullable<SType> topType;
  WithErrorLine<SType> ret = [&]() -> WithErrorLine<SType> {
    if (auto concept = getConcept(name)) {
      if (auto res = concept->canCreateConceptType(); !res)
        return id.codeLoc.getError("Can't create a concept type from concept " + quote(concept->getName()) + ":\n" + res.get_error().toString());
      return SType(ConceptType::get(concept.get(), TRY(getTypeList(id.parts[0].templateArguments, typePack.value_or(false))),
          typePack.value_or(false)));
    } else
      topType = getType(name);
    if (!topType)
      return id.codeLoc.getError("Type not found: " + quote(name));
    if (typePack) {
      auto activePack = getUnexpandedTypePack();
      bool match = activePack && topType.get() == activePack->second;
      if (!*typePack && match)
        return id.codeLoc.getError("Type parameter pack " + quote(name) + " must be unpacked using the '...' operator");
      if (*typePack && !match)
        return id.codeLoc.getError("Type " + quote(name) + " is not a type parameter pack");
    }
    bool variadicParams = id.parts[0].variadic;
    auto templateArgs = TRY(getTypeList(id.parts[0].templateArguments, variadicParams));
    if (variadicParams)
      return id.codeLoc.getError("Type " + quote(name) + " is not a variadic template");
    return topType->instantiate(*this, templateArgs, id.codeLoc);
  }();
  if (ret)
    for (auto& elem : id.typeOperator) {
      elem.visit(
          [&](IdentifierInfo::PointerType type) {
            switch (type) {
              case IdentifierInfo::CONST:
                *ret = PointerType::get(*ret);
                break;
              case IdentifierInfo::MUTABLE:
                *ret = MutablePointerType::get(*ret);
                break;
            }
          },
          [&](const IdentifierInfo::ArraySize& size) {
            if (auto type = size.expr->eval(*this)) {
              if (auto value = type->value.dynamicCast<CompileTimeValue>())
                if (value.get()->getType() == BuiltinType::INT) {
                  if (auto sizeInt = value->value.getValueMaybe<int>())
                    if (*sizeInt < 0) {
                      ret = size.expr->codeLoc.getError("Array size must be non-negative");
                      return;
                    }
                  *ret = ArrayType::get(*ret, value);
                  return;
                }
              ret = size.expr->codeLoc.getError("Inappropriate type of array size: " + quote(type->value->getName()));
            } else
              ret = size.expr->codeLoc.getError("Can't evaluate array size expression at compile-time"s);
          },
          [&](IdentifierInfo::Slice) {
            *ret = SliceType::get(*ret);
          },
          [&](IdentifierInfo::Optional) {
            *ret = OptionalType::get(*ret);
          }
      );
      if (!ret)
        return ret;
    }
  return ret;
}

JustError<string> Context::checkNameConflict(const string& name, const string& type) const {
  TRY(checkNameConflictExcludingFunctions(name, type));
  auto desc = type + " " + quote(name);
  if (!getFunctions(name).empty())
    return desc + " conflicts with existing function";
  return success;
}

JustError<std::string> Context::checkNameConflictExcludingFunctions(const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  if (getType(name))
    return desc + " conflicts with an existing type";
  if (getVariable(name))
    return desc + " conflicts with an existing variable or function";
  return success;
}

JustError<string> Context::addFunction(SFunctionInfo info) {
  auto& overloads = state->functions[info->id];
  for (auto& fun : overloads)
    if ((!info->id.contains<ConstructorTag>() || fun->type.retVal == info->type.retVal) &&
        areParamsEquivalent(fun->type, info->type) &&
        fun->type.generatedConstructor == info->type.generatedConstructor)
      return "Can't overload " + info->prettyString() + " with the same argument types."s;
  overloads.push_back(info);
  return success;
}

vector<SFunctionInfo> Context::getConstructorsFor(const SType& type) const {
  vector<SFunctionInfo> ret;
  for (auto& fun : getFunctions(ConstructorTag{})) {
    if (fun->type.retVal == type) {
      ret.push_back(fun);
    } else
    if (auto structType = fun->type.retVal.dynamicCast<StructType>())
      if (auto origStruct = type.dynamicCast<StructType>())
        if (structType->parent.get() == origStruct->parent.get()) {
          ret.push_back(fun);
        }
  }
  return ret;
}

WithError<IdentifierType> Context::getIdentifierType(const IdentifierInfo& id) const {
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts[0], id.codeLoc))) {
      if (auto ret = type.get()->getStaticContext().getIdentifierType(id.getWithoutFirstPart())) {
        ret->parts = concat({IdentifierType::Part{{type.get()}}}, ret->parts);
        return ret;
      }
    }
    return "Type not found: " + id.prettyString();
  } else {
    string name = id.parts[0].name;
    if (auto type = getType(name))
      // Should this also instantiate the type if there are template arguments?
      return IdentifierType(type.get());
    else
      return IdentifierType(name);
  }
}

vector<SFunctionInfo> Context::getFunctionTemplate(IdentifierType id) const {
  vector<SFunctionInfo> ret;
  if (id.parts.size() > 1)
    return (*id.parts[0].name.getReferenceMaybe<SType>())->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
  else
    return id.parts[0].name.visit(
          [&](const SType& type) { return getConstructorsFor(type); },
          [&](const string& name) { return getFunctions(name);}
    );
}

WithEvalError<SType> Context::BuiltInFunctionInfo::invokeFunction(const string& id, CodeLoc loc, vector<SType> args,
    vector<CodeLoc> argLoc) const {
  CHECK(argTypes.size() == args.size());
  for (auto t : args)
    if (!t->getMangledName()) {
      return SType(CompileTimeValue::get(
          CompileTimeValue::TemplateFunctionCall{id, args,  returnType, loc, argLoc, *this}));
    }
  if (auto res = fun(args))
    return *res;
  else
    return EvalError::withError(res.get_error());
}

WithEvalError<SType> Context::invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const {
  for (auto& state : getReversedStates())
    if (auto f = getReferenceMaybe(state->builtInFunctions, id))
      return f->invokeFunction(id, loc, args, argLoc);
  return EvalError::noEval();
}

void Context::addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction fun) {
  CHECK(!state->builtInFunctions.count(id));
  CHECK(addImplicitFunction(id, FunctionType(returnType, argTypes, {})));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{std::move(argTypes), returnType, std::move(fun)}));
}

nullable<SFunctionInfo> Context::getBuiltinOperator(Operator op, vector<SType> argTypes) const {
  auto functionType = [&] () -> optional<FunctionType> {
    switch (op) {
      case Operator::GET_ADDRESS:
        if (argTypes.size() == 1) {
          if (auto pointerType = convertReferenceToPointerStrict(argTypes[0]))
            return FunctionType(pointerType.get(), {argTypes[0]}, {});
          else // this codegens a call to the op_get_address function, which returns the address of a temporary object
            return FunctionType(PointerType::get(argTypes[0]), {argTypes[0]}, {});
        }
        break;
      case Operator::POINTER_DEREFERENCE:
        if (argTypes.size() == 1) {
          if (auto referenceType = convertPointerToReferenceStrict(argTypes[0]->removeReference()))
            return FunctionType(referenceType.get(), {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::ASSIGNMENT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->removeReference()))
          if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionType(BuiltinType::VOID, {argTypes[0], referenceType->underlying}, {}).setBuiltin();
        break;
      case Operator::SUBSCRIPT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], BuiltinType::INT))
          if (auto pack = argTypes[0].dynamicCast<VariablePack>())
            return FunctionType(pack->packType, {argTypes[0]->removeReference(), argTypes[1]->removeReference()}, {}).setBuiltin();
        break;
      default:
        break;
    }
    return none;
  }();
  if (functionType)
    return FunctionInfo::getImplicit(op, *functionType);
  return nullptr;
}

vector<SFunctionInfo> Context::getOperatorType(Operator op) const {
  return getFunctions(op);
}
