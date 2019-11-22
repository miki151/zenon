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
  if (f1.requirements != f2.requirements)
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i].type != f2.params[i].type)
      return false;
  return true;
}

bool Context::isTemplated() const {
  for (auto& state : getReversedStates())
    if (state->templated)
      return true;
  return false;
}

void Context::setTemplated() {
  state->templated = true;
}

WithErrorLine<vector<LambdaCapture>> Context::setLambda(vector<LambdaCaptureInfo> captures) {
  vector<LambdaCapture> ret;
  for (auto& capture : captures) {
    if (auto type = getTypeOfVariable(capture.name)) {
      if (capture.type == LambdaCaptureType::IMPLICIT_COPY && !canConvert(*type, type->get()->getUnderlying()))
        return capture.codeLoc.getError("Variable " + capture.name + " can't be captured by implicit copy");
      ret.push_back(LambdaCapture{capture.name, *type});
      if (capture.type == LambdaCaptureType::IMPLICIT_COPY)
        ret.back().type = ret.back().type->getUnderlying();
    } else
      return capture.codeLoc.getError(type.get_error());
  }
  state->lambdaInfo = LambdaInfo { std::move(captures) };
  return std::move(ret);
}

bool Context::isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific,
    vector<FunctionType> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, transform(specific->type.params, [](const auto& param) { return param.type; }),
      vector<CodeLoc>(specific->type.params.size(), CodeLoc()), existing)) {
    return specific->type.retVal == inst.get()->type.retVal;
  }
  else
    return false;
}

WithError<vector<SFunctionInfo>> Context::getRequiredFunctions(const Concept& required, vector<FunctionType> existing) const {
  vector<SFunctionInfo> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        auto getFunction = [&]() -> optional<SFunctionInfo> {
          for (auto& myFun : getFunctions(overloads.first))
            if (isGeneralization(myFun, function, existing))
              return myFun->getWithoutRequirements();
          if (overloads.first == "invoke"s)
            if (!function->type.params.empty())
              if (auto lambda = function->type.params[0].type->removePointer().dynamicCast<LambdaType>())
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
  optional<LambdaInfo> lambdaInfo;
  for (auto& state : getReversedStates()) {
    if (state->vars.count(id)) {
      if (lambdaInfo) {
        if (auto info = lambdaInfo->find(id)) {
          switch (info->type) {
            case LambdaCaptureType::REFERENCE:
              return state->vars.at(id);
            case LambdaCaptureType::IMPLICIT_COPY:
              return state->vars.at(id)->getUnderlying();
          }
        } else
          return "Variable " + quote(id) + " is not captured by lambda";
      } else
        return state->vars.at(id);
    }
    if (state->lambdaInfo)
      lambdaInfo = state->lambdaInfo;
  }
  for (auto& state : getReversedStates())
    if (state->variablePack && state->variablePack->name == id)
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

void Context::addVariable(const string& ident, SType t) {
  state->vars.insert({ident, t});
  state->varsList.push_back(ident);
}

void Context::addVariablePack(const string& ident, SType t) {
  state->variablePack = VariablePackInfo{ident, t};
}

const optional<Context::VariablePackInfo>& Context::getVariablePack() const {
  for (auto& state : getReversedStates())
    if (state->variablePack)
      return state->variablePack;
  return state->variablePack;
}

void Context::expandVariablePack(const vector<string>& vars) {
  auto& pack = *getVariablePack();
  for (auto& v : vars)
    addVariable(v, pack.type);
  state->variablePack = pack;
  state->variablePack->wasExpanded = true;
}

void Context::State::print() const {
  for (auto& varName : varsList) {
    auto& var = vars.at(varName);
    cout << "Variable " << quote(varName) << " of type " << var->getName() << "\n";
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
  if (type->getUnderlying()->isBuiltinCopyable(*this) && type != type->getUnderlying())
    ret.push_back(type->getUnderlying());
  if (type->getUnderlying() == ArithmeticType::INT)
    ret.push_back(ArithmeticType::DOUBLE);
  if (auto ptr = type->getUnderlying().dynamicCast<MutablePointerType>())
    ret.push_back(PointerType::get(ptr->underlying));
  if (type->getUnderlying() != ArithmeticType::NULL_TYPE)
    ret.push_back(OptionalType::get(type->getUnderlying()));
  return ret;
}

bool Context::canConvert(SType from, SType to) const {
  return contains(getConversions(from), to) ||
      (from == ArithmeticType::NULL_TYPE && to.dynamicCast<OptionalType>() &&
          to.dynamicCast<OptionalType>()->underlying != ArithmeticType::NULL_TYPE);
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
      if (overload->type.params.back().type == from) {
        auto type = overload->type;
        //std::cout << "Expanded " << overload->prettyString() << std::endl;
        if (type.variadicParams) {
          type.params.pop_back();
          for (auto& t : to)
            type.params.push_back(FunctionType::Param(none, t));
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

void Context::addType(const string& name, SType t, bool fullyDefined, bool typePack) {
  CHECK(!getType(name));
  state->types.insert({name, t});
  state->fullyDefinedTypes.insert({t.get(), fullyDefined});
  if (typePack)
    state->typePack = t;
}

WithErrorLine<vector<SType>> Context::getTypeList(const vector<TemplateParameterInfo>& ids, bool variadic) const {
  vector<SType> params;
  for (int i = 0; i < ids.size(); ++i) {
    auto type = ids[i].visit(
        [&](const IdentifierInfo& id) -> WithErrorLine<SType> {
          return getTypeFromString(id,  i == ids.size() - 1 && variadic); },
        [&](const shared_ptr<Expression>& expr) -> WithErrorLine<SType> {
          if (auto value = expr->eval(*this))
            return value->value;
          else
            return expr->codeLoc.getError("Can't evaluate expression at compile-time");
        }
    );
    if (type)
      params.push_back(*type);
    else
      return type.get_error();
  }
  return params;
}

void Context::addConcept(const string& name, shared_ptr<Concept> i) {
  state->concepts.insert({name, i});
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

nullable<SType> Context::getTypePack() const {
  for (auto& state : getReversedStates())
    if (auto t = state->typePack)
      return t.get();
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

optional<string> Context::addImplicitFunction(FunctionId id, FunctionType type) {
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
      return state->vars.at(name);
  return nullptr;
}

WithErrorLine<SType> Context::getTypeFromString(IdentifierInfo id, optional<bool> typePack) const {
  if (id.parts.size() != 1)
    return id.codeLoc.getError("Bad type identifier: " + id.prettyString());
  auto name = id.parts.at(0).name;
  auto topType = getType(name);
  if (!topType)
    return id.codeLoc.getError("Type not found: " + quote(name));
  if (typePack) {
    if (!*typePack && topType == getTypePack())
      return id.codeLoc.getError("Type parameter pack " + quote(name) + " must be unpacked using the '...' operator");
    if (*typePack && getTypePack() != topType.get())
      return id.codeLoc.getError("Type " + quote(name) + " is not a type parameter pack");
  }
  bool variadicParams = id.parts.at(0).variadic;
  auto templateArgs = getTypeList(id.parts.at(0).templateArguments, variadicParams);
  if (!templateArgs)
    return templateArgs.get_error();
  if (variadicParams)
    return id.codeLoc.getError("Type " + quote(name) + " is not a variadic template");
  WithErrorLine<SType> ret = topType->instantiate(*this, *templateArgs, id.codeLoc);
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
                if (value.get()->getType() == ArithmeticType::INT) {
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

optional<string> Context::checkNameConflict(const string& name, const string& type) const {
  if (auto err = checkNameConflictExcludingFunctions(name, type))
    return err;
  auto desc = type + " " + quote(name);
  if (!getFunctions(name).empty())
    return desc + " conflicts with existing function";
  return none;
}

optional<string> Context::checkNameConflictExcludingFunctions(const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  if (getType(name))
    return desc + " conflicts with an existing type";
  if (getVariable(name))
    return desc + " conflicts with an existing variable or function";
  return none;
}

optional<string> Context::addFunction(SFunctionInfo info) {
  auto& overloads = state->functions[info->id];
  for (auto& fun : overloads)
    if (areParamsEquivalent(fun->type, info->type) &&
        (!info->id.contains<ConstructorTag>() || fun->type.retVal == info->type.retVal) &&
        fun->type.generatedConstructor == info->type.generatedConstructor)
      return "Can't overload " + info->prettyString() + " with the same argument types."s;
  overloads.push_back(info);
  return none;
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
  vector<FunctionInfo> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0), id.codeLoc))) {
      if (auto ret = type.get()->getStaticContext().getIdentifierType(id.getWithoutFirstPart())) {
        ret->parts = concat({IdentifierType::Part{{type.get()}}}, ret->parts);
        return ret;
      }
    }
    return "Type not found: " + id.prettyString();
  } else {
    string name = id.parts.at(0).name;
    if (auto type = getType(name))
      // Should this also instantiate the type if there are template arguments?
      return IdentifierType(type.get());
    else
      return IdentifierType(name);
  }
}

WithError<vector<SFunctionInfo>> Context::getFunctionTemplate(IdentifierType id) const {
  vector<SFunctionInfo> ret;
  if (id.parts.size() > 1)
    return (*id.parts[0].name.getReferenceMaybe<SType>())->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
  else
    return id.parts.at(0).name.visit(
          [&](const SType& type) { return getConstructorsFor(type); },
          [&](const string& name) { return getFunctions(name);}
    );
}

WithErrorLine<SFunctionInfo> Context::instantiateFunctionTemplate(CodeLoc codeLoc, SFunctionInfo templateType,
    vector<SType> templateParams, vector<SType> argTypes, vector<CodeLoc> argLoc) const {
  return instantiateFunction(*this, templateType, codeLoc, templateParams, argTypes, argLoc, {});
}

nullable<SType> Context::BuiltInFunctionInfo::invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const {
  if (argTypes.size() != args.size())
    return nullptr;
    //loc.error("Expected " + to_string(argTypes.size()) + " arguments to built-in function " + quote(id));
  for (int i = 0; i < args.size(); ++i)
    if (argTypes[i] != args[i]->getType())
      return nullptr;
      /*argLoc[i].error("Expected argument of type " + quote(argTypes[i]->getName()) + ", got "
          + quote(args[i]->getName()) + " of type " + quote(args[i]->getType()->getName()));*/
  for (auto t : args)
    if (!t->getMangledName()) {
      return SType(CompileTimeValue::get(
          CompileTimeValue::TemplateFunctionCall{id, args,  returnType, loc, argLoc, *this}));
    }
  return fun(args).addCodeLoc(loc).get();
}

nullable<SType> Context::invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const {
  for (auto& state : getReversedStates())
    if (auto f = getReferenceMaybe(state->builtInFunctions, id))
      return f->invokeFunction(id, loc, args, argLoc);
  return nullptr;
}

void Context::addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction fun) {
  CHECK(!state->builtInFunctions.count(id));
  CHECK(!addImplicitFunction(id, FunctionType(returnType,
      transform(argTypes, [](const auto& p){ return FunctionType::Param(p); }), {})));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{std::move(argTypes), returnType, std::move(fun)}));
}

nullable<SFunctionInfo> Context::getBuiltinOperator(Operator op, vector<SType> argTypes) const {
  auto functionType = [&] () -> optional<FunctionType> {
    switch (op) {
      case Operator::GET_ADDRESS:
        if (argTypes.size() == 1) {
          if (auto referenceType = argTypes[0].dynamicCast<ReferenceType>())
            return FunctionType(PointerType::get(referenceType->underlying), {argTypes[0]}, {}).setBuiltin();
          else if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionType(MutablePointerType::get(referenceType->underlying), {argTypes[0]}, {}).setBuiltin();
          else // this codegens a call to the op_get_address function, which returns the address of a temporary object
            return FunctionType(PointerType::get(argTypes[0]), {argTypes[0]}, {});
        }
        break;
      case Operator::POINTER_DEREFERENCE:
        if (argTypes.size() == 1) {
          if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<PointerType>())
            return FunctionType(ReferenceType::get(pointerType->underlying), {argTypes[0]}, {}).setBuiltin();
          else if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<MutablePointerType>())
            return FunctionType(MutableReferenceType::get(pointerType->underlying), {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::ASSIGNMENT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->getUnderlying()))
          if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionType(ArithmeticType::VOID, {argTypes[0], referenceType->underlying}, {}).setBuiltin();
        break;
      case Operator::SUBSCRIPT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], ArithmeticType::INT))
          if (auto pack = argTypes[0].dynamicCast<VariablePack>())
            return FunctionType(pack->packType, {argTypes[0]->getUnderlying(), argTypes[0]->getUnderlying()}, {}).setBuiltin();
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

bool Context::canDefaultInitialize(SType type) const {
  if (auto s = type.dynamicCast<StructType>())
    type = s->parent.get();
  for (auto& f : getConstructorsFor(type))
    if (f->type.params.empty())
      return true;
  return false;
}

const LambdaCaptureInfo* Context::LambdaInfo::find(const string& var) const {
  for (auto& elem : captures)
    if (elem.name == var)
      return &elem;
  return nullptr;
}
