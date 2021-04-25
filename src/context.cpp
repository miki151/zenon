#include "context.h"
#include "ast.h"
#include "type.h"
#include "type_registry.h"


struct StateIterator {
  const Context* context;
  int index;
  decltype(auto) operator ++() {
    ++index;
    return *this;
  }
  const Context::State* operator* () const {
    if (index == 0)
      return context->state.get();
    return context->parentStates[context->parentStates.size() - index].get();
  }
  bool operator != (const StateIterator& o) {
    return index != o.index;
  }
};

struct StateContainer {
  const Context* context;
  StateIterator begin() {
    return StateIterator { context, 0 };
  }

  StateIterator end() {
    return StateIterator { context, context->parentStates.size() + 1 };
  }
};

Context Context::getChild(bool isTopLevel) const {
  Context ret(typeRegistry, isTopLevel);
  ret.parentStates = parentStates;
  ret.parentStates.push_back(state);
  return ret;
}

Context Context::getTopLevel() const {
  Context ret(typeRegistry, false);
  for (auto& s : parentStates)
    if (s->isTopLevel)
      ret.parentStates.push_back(s);
  if (state->isTopLevel)
    ret.parentStates.push_back(state);
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

Context::Context(TypeRegistry* t, bool isTopLevel) : typeRegistry(t), state(shared<State>()) {
  state->isTopLevel = isTopLevel;
}

void Context::State::merge(const Context::State& o) {
  for (auto& f : o.functions)
    if (!functions.count(f.first))
      functions.insert(f);
}

void Context::deepCopyFrom(const Context& c) {
  CHECK(parentStates.empty());
  *state = *c.state;
  for (auto s : c.parentStates)
    state->merge(*s);
}

bool Context::areParamsEquivalent(FunctionSignature f1, FunctionSignature f2) const {
  if (f1.templateParams.size() != f2.templateParams.size() || f1.params.size() != f2.params.size())
    return false;
  auto origParams = f2.templateParams;
  for (int i = 0; i < f1.templateParams.size(); ++i) {
    auto tParam = f2.templateParams[i];
    ErrorBuffer errors;
    f2 = replaceInFunction(*this, std::move(f2), std::move(tParam), f1.templateParams[i], errors, origParams);
    if (!errors.empty())
      return false;
  }
  return f1.requirements == f2.requirements && f1.params == f2.params;
}

optional<vector<SType>> Context::getTemplateParams() const {
  for (auto state : getReversedStates())
    if (!state->templateParams.empty())
      return state->templateParams;
  return none;
}

void Context::setTemplated(vector<SType> v) {
  state->templateParams = v;
}

void Context::setTemplateInstance() {
  state->isTemplateInstance = true;
}

bool Context::isTemplateInstance() const {
  for (auto state : getReversedStates())
    if (state->isTemplateInstance)
      return true;
  return false;
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

void Context::setLambda(LambdaCaptureInfo* info) {
  state->lambdaInfo = info;
}

nullable<SFunctionInfo> Context::isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific,
    vector<FunctionSignature> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = isGeneralizationWithoutReturnType(general, specific, existing)) {
    if (specific->type.retVal == inst.get()->type.retVal)
      return inst;
  }
  return nullptr;
}

nullable<SFunctionInfo> Context::isGeneralizationWithoutReturnType(const SFunctionInfo& general,
    const SFunctionInfo& specific, vector<FunctionSignature> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, specific->type.params,
      vector<CodeLoc>(specific->type.params.size(), CodeLoc()), existing))
    return *inst;
  return nullptr;
}

WithError<vector<SFunctionInfo>> Context::getRequiredFunctions(const Concept& required,
    vector<FunctionSignature> existing) const {
  vector<SFunctionInfo> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        auto getFunction = [&]() -> optional<SFunctionInfo> {
          for (auto& overload : getSpecialOverloads(overloads.first, function->type.params))
            if (isGeneralization(overload, function, existing))
              return overload;
          for (auto& myFun : getFunctions(overloads.first))
            if (!myFun->isConceptTypeFunction())
              if (auto gen = isGeneralization(myFun, function, existing))
                return gen.get();
          return none;
        };
        if (auto res = getFunction())
          ret.push_back(*res);
        else if (function->id == AttributeTag{})
          return "Type " + quote(function->type.params[0]->getName()) + " is missing attribute: "
              + quote(function->type.params[1]->getName());
        else
          return "Required function not implemented: " + function->prettyString() +
              ", required by concept: " + quote(required.getName());
      }
  }
  return ret;
}

WithError<SType> Context::getTypeOfVariable(const string& id) const {
  vector<LambdaCaptureInfo*> lambdas;
  bool makeConstReference = false;
  for (auto state : getReversedStates()) {
    if (auto varInfo = getReferenceMaybe(state->vars, id)) {
      if (!varInfo->global)
        for (auto lambdaInfo : lambdas) {
          if (auto info = lambdaInfo->find(id)) {
            if (info->type != LambdaCaptureType::REFERENCE)
              makeConstReference = true;
          } else
          if (auto captureType = lambdaInfo->defaultCapture) {
            auto& varInfo = state->vars.at(id);
            lambdaInfo->captures.push_back(LambdaCaptureInfo::Var{id, varInfo.declarationLoc, *captureType, false});
            lambdaInfo->implicitCaptures.push_back(LambdaCapture{id, getCapturedType(varInfo.type, *captureType), *captureType});
          } else
            return "Variable " + quote(id) + " is not captured by lambda";
        }
      return makeConstReference
          ? ReferenceType::get(varInfo->type->removeReference())
          : varInfo->type;
    }
    if (state->lambdaInfo)
      lambdas.push_back(state->lambdaInfo);
  }
  for (auto state : getReversedStates())
    if (state->unexpandedVariablePack && state->unexpandedVariablePack->first == id)
      return "Variable pack must be unpacked using the '...' operator"s;
  return "Variable not found: " + id;
}

bool Context::isCapturedVariable(const string& id) const {
  for (auto state : getReversedStates()) {
    if (state->vars.count(id))
      return false;
    if (state->lambdaInfo)
      return true;
  }
  fail();
}

void Context::addVariable(const string& ident, SType t, CodeLoc codeLoc, bool global) {
  state->vars.insert({ident, VariableInfo{t, codeLoc, global}});
  state->varsList.push_back(ident);
}

void Context::setShadowId(const string& oldId, const string& newId) {
  state->shadowIds.insert({oldId, newId});
}

optional<string> Context::getShadowId(const string& id) const {
  for (auto state : getReversedStates())
    if (auto newId = getReferenceMaybe(state->shadowIds, id))
      return *newId;
  return none;
}

void Context::setNonMovable(const string& variable) {
  state->nonMovableVars.insert(variable);
}

bool Context::isNonMovable(const string& variable) const {
  for (auto state : getReversedStates())
    if (state->nonMovableVars.count(variable))
      return true;
  return false;
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
    cout << "Type: " << type.second->getName() << "\n";
}

void Context::print() const {
  for (auto state : getReversedStates())
    state->print();
}

SType Context::getSliceType(SType underlying) const {
  return *getType("slice_t")->instantiate(*this, {std::move(underlying)}, CodeLoc());
}

SType Context::getMutableSliceType(SType underlying) const {
  return *getType("mutable_slice_t")->instantiate(*this, {std::move(underlying)}, CodeLoc());
}

vector<SType> Context::getConversions(SType type, SType to) const {
  vector<SType> ret = {type};
  if (type->isMetaType() && type != BuiltinType::ANY_TYPE)
    ret.push_back(BuiltinType::ANY_TYPE);
  ret.push_back(BuiltinType::ANYTHING);
  auto underlying = type->removeReference();
  if (underlying == BuiltinType::INT)
    ret.push_back(BuiltinType::DOUBLE);
  if (auto ptr = underlying.dynamicCast<MutablePointerType>())
    ret.push_back(PointerType::get(ptr->underlying));
  if (auto t = underlying.dynamicCast<StructType>())
    if (t->parent && t->parent.get() == getType("mutable_slice_t").get())
      ret.push_back(getSliceType(t->templateParams[0]));
  if (underlying != BuiltinType::NULL_TYPE)
    ret.push_back(OptionalType::get(underlying));
  if (underlying->isPointer() &&
      to->isPointer() &&
      !underlying->removePointer().dynamicCast<ConceptType>() &&
      to->removePointer().dynamicCast<ConceptType>() &&
      (to.dynamicCast<PointerType>() || underlying.dynamicCast<MutablePointerType>()))
    ret.push_back(to);
  if (!underlying.dynamicCast<ConceptType>() &&
      to.dynamicCast<ConceptType>() &&
      (!type->isReference() || type->isBuiltinCopyable(*this)))
    ret.push_back(to);
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
  if (from != fromUnderlying && to->removeReference() == fromUnderlying)
    return fromUnderlying->isBuiltinCopyable(*this, expr);
  if (fromUnderlying->isPointer() && to->isPointer() && !fromUnderlying->removePointer().dynamicCast<ConceptType>())
    if (auto conceptType = to->removePointer().dynamicCast<ConceptType>()) {
      auto concept = conceptType->getConceptFor(fromUnderlying->removePointer());
      if (to.dynamicCast<MutablePointerType>() && !fromUnderlying.dynamicCast<MutablePointerType>())
        return "Cannot cast value of type " + quote(fromUnderlying->getName()) + " to a mutable pointer";
      auto functions = TRY(getRequiredFunctionsForConceptType(*this, *concept, CodeLoc()));
      for (auto& fun : functions)
        CHECK(!!fun->addInstance(*this));
      concept->def->addFatPointer({fromUnderlying->removePointer(), functions});
      concept->def->addConceptType(conceptType);
      if (expr) {
        auto loc = expr->codeLoc;
        expr = unique<FatPointerConversion>(loc, functions, to, fromUnderlying, std::move(expr), conceptType);
        auto err = expr->getTypeImpl(*this);
        CHECK(!!err) << err.get_error();
      }
      return success;
    }
  if (auto conceptType = to.dynamicCast<ConceptType>()) {
    auto concept = conceptType->getConceptFor(fromUnderlying);
    if (from->isReference()) {
      TRY(canConvert(from, fromUnderlying, expr));
      if (expr)
        CHECK(!!::getType(*this, expr));
    }
    auto functions = TRY(getRequiredFunctionsForConceptType(*this, *concept, CodeLoc()));
    for (auto& fun : functions)
      CHECK(!!fun->addInstance(*this));
    concept->def->addFatPointer({fromUnderlying, functions});
    concept->def->addConceptType(conceptType);
    if (expr) {
      auto loc = expr->codeLoc;
      expr = unique<FatPointerConversion>(loc, functions, to, fromUnderlying, std::move(expr), conceptType);
      auto err = expr->getTypeImpl(*this);
      CHECK(!!err) << err.get_error();
    }
    return success;
  }
  if (auto structType = toUnderlying.dynamicCast<StructType>())
    if (!structType->alternatives.empty()) {
      const StructType::Variable* alternative = nullptr;
      for (auto& alt : structType->alternatives)
        if (alt.type == fromUnderlying) {
          if (!!alternative)
            return "Multiple matching alternatives found in " + quote(toUnderlying->getName()) +
                ": " + quote(alternative->name) + ", " + quote(alt.name);
          alternative = &alt;
        }
      if (alternative) {
        if (from != fromUnderlying)
          TRY(fromUnderlying->isBuiltinCopyable(*this, expr));
        if (expr) {
          auto codeLoc = expr->codeLoc;
          auto call = [&] () -> unique_ptr<Expression> {
            if (alternative->type == BuiltinType::VOID) {
              // This is not needed once there is a generic union constructor for each alternative
              auto call = unique<FunctionCall>(IdentifierInfo("bogus", codeLoc), false);
              call->functionInfo = structType->staticContext.getFunctions(alternative->name).getOnlyElement();
              return unique<StatementExpression>(codeLoc,
                  makeVec<unique_ptr<Statement>>(unique<ExpressionStatement>(std::move(expr))),
                  cast<Expression>(std::move(call)));
            } else {
              auto call = unique<FunctionCall>(IdentifierInfo("bogus", codeLoc), std::move(expr), false);
              call->functionInfo = structType->staticContext.getFunctions(alternative->name).getOnlyElement();
              return call;
            }
          }();
          expr = std::move(call);
          auto err = expr->getTypeImpl(*this);
          CHECK(!!err) << err.get_error();
        }
        return success;
      }
    }
  if (contains(getConversions(from, to), to) ||
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

void Context::setIsInBranch() {
  state->isBranch = true;
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
      overload = replaceInFunction(*this, overload, from, to, errors);
      //std::cout << "To " << overload->prettyString() << std::endl;
    }
  }
  state->typesSet.clear();
  for (auto& type : state->types) {
    type.second = type.second->replace(*this, from, to, errors);
    state->typesSet.insert(type.second.get());
  }
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
  for (auto state : getReversedStates())
    if (state->returnTypeChecker)
      return state->returnTypeChecker;
  return nullptr;
}

void Context::addReturnTypeChecker(ReturnTypeChecker* c) {
  CHECK(!state->returnTypeChecker);
  state->returnTypeChecker = c;
}

StateContainer Context::getReversedStates() const {
  return StateContainer{this};
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
  for (auto state : getReversedStates())
    if (state->expandedVariablePack)
      return *state->expandedVariablePack;
  return "No variable pack is present in this context"s;
}

optional<pair<string, vector<SType>>> Context::getExpandedTypePack() const {
  for (auto state : getReversedStates())
    if (state->expandedTypePack)
      return state->expandedTypePack;
  return none;
}

optional<pair<string, SType> > Context::getUnexpandedTypePack() const {
  for (auto state : getReversedStates()) {
    if (state->expandedTypePack)
      return none;
    if (state->unexpandedTypePack)
      return state->unexpandedTypePack;
  }
  return none;
}

optional<pair<string, SType> > Context::getUnexpandedVariablePack() const {
  for (auto state : getReversedStates()) {
    if (state->expandedVariablePack)
      return none;
    if (state->unexpandedVariablePack)
      return state->unexpandedVariablePack;
  }
  return none;
}

void Context::addType(const string& name, SType t) {
  //CHECK(!getType(name) || !isFullyDefined(getType(name).get().get())) << name;
  state->types.insert({name, t});
  state->typesSet.insert(t.get());
}

void Context::addSubstitution(SubstitutionInfo info) {
  state->substitutions.push_back(std::move(info));
}

vector<Context::SubstitutionInfo> Context::getSubstitutions() const {
  vector<SubstitutionInfo> ret;
  for (auto state : getReversedStates())
    ret.append(state->substitutions);
  return ret;
}

void Context::setAttribute(SType t, SType attr, vector<SType> templateParams) {
  CHECK(addFunction(FunctionInfo::getImplicit(AttributeTag{},
      FunctionSignature(BuiltinType::VOID, {t, attr}, std::move(templateParams)))));
}

void Context::setStructMembers(SType t, vector<SType> members, vector<SType> templateParams) {
  CHECK(addFunction(FunctionInfo::getImplicit(StructMembersTag{},
      FunctionSignature(BuiltinType::VOID, concat({std::move(t)}, std::move(members)), std::move(templateParams)))));
}

WithErrorLine<vector<SType>> Context::getTypeList(const vector<TemplateParameterInfo>& ids, bool variadic) const {
  vector<SType> params;
  for (int i = 0; i < ids.size(); ++i) {
    auto getType = [id = ids[i]] (const Context& context, bool variadic) {
      return id.visit(
          [&](const IdentifierInfo& id) -> WithErrorLine<SType> {
            return TRY(context.getTypeFromString(id, none))->removeValueReference();
          },
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
          auto elemContext = getChild();
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
          replaceInFunction(*this, fun, i->getParams()[0], ConceptType::get(i, typeParams, i->isVariadic()), errors),
          typeParams, i->isVariadic())));
    }
    CHECK(errors.empty());
  }
}

nullable<SConcept> Context::getConcept(const string& name) const {
  for (auto state : getReversedStates())
    if (state->concepts.count(name))
      return state->concepts.at(name);
  return nullptr;

}

nullable<SType> Context::getType(const string& s) const {
  bool isLoopOrIf = false;
  for (auto state : getReversedStates()) {
    if (auto ret = getReferenceMaybe(state->types, s))
      return isLoopOrIf ? (*ret)->removeValueReference() : *ret;
    if (state->loopId || state->isBranch)
      isLoopOrIf = true;
  }
  if (typeRegistry)
    return typeRegistry->getType(s);
  return nullptr;
}

bool Context::isFullyDefined(const Type* t) const {
  if (auto s = dynamic_cast<const StructType*>(t)) {
    // If it's a struct type then we only look at the top level because
    // an incomplete struct could also be used to instantiate a template
    // and we'd get a false positive.
    auto parent = s->parent.get().get();
    for (auto state : getReversedStates())
      if (state->isTopLevel && state->typesSet.count(parent))
        return true;
    return false;
  }
  for (auto state : getReversedStates())
    if (state->typesSet.count(t))
      return true;
  return false;
}

vector<SType> Context::getAllTypes() const {
  vector<SType> ret;
  for (auto state : getReversedStates())
    for (auto& type : state->types)
      ret.push_back(type.second);
  return ret;
}

JustError<string> Context::addImplicitFunction(FunctionId id, FunctionSignature type) {
  return addFunction(FunctionInfo::getImplicit(std::move(id), std::move(type)));
}

vector<SFunctionInfo> Context::getFunctions(FunctionId name) const {
  vector<SFunctionInfo> ret;
  for (auto state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

vector<SFunctionInfo> Context::getAllFunctions() const {
  vector<SFunctionInfo> ret;
  for (auto state : getReversedStates())
    for (auto& overloadSet : state->functions)
      for (auto& fun : overloadSet.second)
        ret.push_back(fun);
  return ret;
}

nullable<SType> Context::getVariable(const string& name) const {
  for (auto state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name).type;
  return nullptr;
}

WithErrorLine<SType> Context::getTypeFromString(IdentifierInfo id, optional<bool> typePack) const {
  if (id.parts.size() > 1)
    return id.codeLoc.getError("Bad type identifier: " + id.prettyString());
  nullable<SType> topType;
  WithErrorLine<SType> ret = [&]() -> WithErrorLine<SType> {
    if (id.typeExpression) {
      if (auto res = id.typeExpression->eval(*this))
        return res->value;
      else {
        return id.typeExpression->codeLoc.getError(!res.get_error().canEval
            ? "Cannot evaulate expression." : res.get_error().error);
      }
    }
    auto name = id.parts[0].name;
    if (auto id = getShadowId(name))
      name = *id;
    if (auto concept = getConcept(name)) {
      if (auto res = concept->canCreateConceptType(); !res)
        return id.codeLoc.getError("Can't create a concept type from concept " + quote(concept->getName()) + ":\n" + res.get_error().toString());
      bool variadic = typePack.value_or(false);
      auto templateParams = TRY(getTypeList(id.parts[0].templateArguments, typePack.value_or(false)));
      if (concept->getParams().size() - 1 > templateParams.size() ||
          ((!concept->isVariadic() || variadic) && templateParams.size() != concept->getParams().size() - 1))
        return id.codeLoc.getError("Wrong number of template parameters to concept type " + quote(concept->getName()));
      auto type = ConceptType::get(concept.get(), std::move(templateParams), variadic);
      concept->def->addConceptType(type);
      return SType(std::move(type));
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
    return topType->instantiate(getTopLevel(), templateArgs, id.codeLoc);
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
          [&](IdentifierInfo::Optional) {
            *ret = OptionalType::get(*ret);
          },
          [&](const IdentifierInfo& id) {
            auto errorType = getTypeFromString(id);
            if (auto expected = getType("expected"))
              ret = expected->instantiate(getTopLevel(), {*ret, errorType.get()}, id.codeLoc);
            else
              ret = id.codeLoc.getError("The template type \"expected\" is not available in this context. "
                  "Try importing \"std/expected.znn\"");
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
  if (auto t = getType(name))
    if (isFullyDefined(t.get().get()))
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

WithError<vector<SFunctionInfo>> Context::getFunctionTemplate(IdentifierInfo id) const {
  vector<SFunctionInfo> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts[0], id.codeLoc)))
      return (*type)->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
    else
      return "Type not found: " + id.prettyString();
  } else {
    string funName = id.parts[0].name;
    if (auto type = getType(funName))
      ret = getConstructorsFor(type.get());
    else
      ret = getFunctions(funName);
  }
  return ret;
}

WithEvalError<SType> Context::BuiltInFunctionInfo::invokeFunction(const Context& context, const string& id, CodeLoc loc,
    vector<SType> args, vector<CodeLoc> argLoc) const {
  CHECK(argTypes.size() == args.size());
  for (auto t : args)
    if (!t->getMangledName()) {
      return SType(CompileTimeValue::get(
          CompileTimeValue::TemplateFunctionCall{id, args,  returnType, loc, argLoc, *this}));
    }
  if (auto res = fun(context, args))
    return *res;
  else
    return EvalError::withError(res.get_error());
}

WithEvalError<SType> Context::invokeFunction(const string& id, CodeLoc loc, vector<SType> args,
    vector<CodeLoc> argLoc) const {
  for (auto state : getReversedStates())
    if (auto f = getReferenceMaybe(state->builtInFunctions, id))
      return f->invokeFunction(*this, id, loc, args, argLoc);
  return EvalError::noEval();
}

void Context::addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction fun) {
  CHECK(!state->builtInFunctions.count(id));
  CHECK(addImplicitFunction(id, FunctionSignature(returnType, argTypes, {})));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{std::move(argTypes), returnType, std::move(fun)}));
}

nullable<SFunctionInfo> Context::getBuiltinOperator(Operator op, vector<SType> argTypes) const {
  auto functionType = [&] () -> optional<FunctionSignature> {
    switch (op) {
      case Operator::GET_ADDRESS:
        if (argTypes.size() == 1) {
          if (auto pointerType = convertReferenceToPointerStrict(argTypes[0]))
            return FunctionSignature(pointerType.get(), {argTypes[0]}, {});
          else // this codegens a call to the op_get_address function, which returns the address of a temporary object
            return FunctionSignature(PointerType::get(argTypes[0]), {argTypes[0]}, {});
        }
        break;
      case Operator::POINTER_DEREFERENCE:
        if (argTypes.size() == 1) {
          if (auto referenceType = convertPointerToReferenceStrict(argTypes[0]->removeReference()))
            return FunctionSignature(referenceType.get(), {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::ASSIGNMENT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->removeReference()))
          if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionSignature(BuiltinType::VOID, {argTypes[0], referenceType->underlying}, {}).setBuiltin();
        break;
      case Operator::SUBSCRIPT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], BuiltinType::INT))
          if (auto pack = argTypes[0].dynamicCast<VariablePack>())
            return FunctionSignature(pack->packType, {argTypes[0]->removeReference(), argTypes[1]->removeReference()}, {}).setBuiltin();
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
