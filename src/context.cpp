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

optional<vector<Type*>> Context::getTemplateParams() const {
  for (auto state : getReversedStates())
    if (!state->templateParams.empty())
      return state->templateParams;
  return none;
}

void Context::setTemplated(vector<Type*> v) {
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

static Type* getCapturedType(Type* input, LambdaCaptureType type) {
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

FunctionInfo* Context::isGeneralization(FunctionInfo* general, FunctionInfo* specific,
    vector<FunctionSignature> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = isGeneralizationWithoutReturnType(general, specific, existing)) {
    if (specific->type.retVal == inst->type.retVal)
      return inst;
  }
  return nullptr;
}

FunctionInfo* Context::isGeneralizationWithoutReturnType(FunctionInfo* general,
    FunctionInfo* specific, vector<FunctionSignature> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, specific->type.params,
      vector<CodeLoc>(specific->type.params.size(), CodeLoc()), existing))
    return *inst;
  return nullptr;
}

WithError<vector<FunctionInfo*>> Context::getRequiredFunctions(const Concept& required,
    vector<FunctionSignature> existing) const {
  vector<FunctionInfo*> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        auto getFunction = [&]() -> optional<FunctionInfo*> {
          for (auto& overload : getSpecialOverloads(overloads.first, function->type.params))
            if (isGeneralization(overload, function, existing))
              return overload;
          for (auto& myFun : getFunctions(overloads.first, false))
            if (auto gen = isGeneralization(myFun, function, existing))
              return gen;
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

WithError<Type*> Context::getTypeOfVariable(const string& id) const {
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

void Context::addVariable(const string& ident, Type* t, CodeLoc codeLoc, bool global) {
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

Type* Context::getSliceType(Type* underlying) const {
  return *getType("slice_t")->instantiate(*this, {std::move(underlying)}, CodeLoc());
}

Type* Context::getMutableSliceType(Type* underlying) const {
  return *getType("mutable_slice_t")->instantiate(*this, {std::move(underlying)}, CodeLoc());
}

vector<Type*> Context::getConversions(Type* type, Type* to, bool withConcepts) const {
  vector<Type*> ret = {type};
  if (type->isMetaType() && type != BuiltinType::ANY_TYPE)
    ret.push_back(BuiltinType::ANY_TYPE);
  ret.push_back(BuiltinType::ANYTHING);
  auto underlying = type->removeReference();
  if (underlying == BuiltinType::INT)
    ret.push_back(BuiltinType::DOUBLE);
  if (auto ptr = dynamic_cast<MutablePointerType*>(underlying))
    ret.push_back(PointerType::get(ptr->underlying));
  if (auto t = dynamic_cast<StructType*>(underlying))
    if (t->parent && getType("mutable_slice_t") == t->parent)
      ret.push_back(getSliceType(t->templateParams[0]));
  if (underlying != BuiltinType::NULL_TYPE)
    ret.push_back(OptionalType::get(underlying));
  if (withConcepts) {
    if (underlying->isPointer() &&
        to->isPointer() &&
        !dynamic_cast<ConceptType*>(underlying->removePointer()) &&
        dynamic_cast<ConceptType*>(to->removePointer()) &&
        (dynamic_cast<PointerType*>(to) || dynamic_cast<MutablePointerType*>(underlying)))
      ret.push_back(to);
  }
  return ret;
}

JustError<string> Context::canConvert(Type* from, Type* to) const {
  unique_ptr<Expression> expr;
  return canConvert(std::move(from), std::move(to), expr);
}

JustError<string> Context::canConvert(Type* from, Type* to, unique_ptr<Expression>& expr) const {
  if (from == to)
    return success;
  auto fromUnderlying = from->removeReference();
  auto toUnderlying = to->removeReference();
  if (fromUnderlying == toUnderlying && toUnderlying != to &&
      (!dynamic_cast<ReferenceType*>(from) || !dynamic_cast<MutableReferenceType*>(to)))
    return success;
  if (from != fromUnderlying && to->removeReference() == fromUnderlying)
    return fromUnderlying->isBuiltinCopyable(*this, expr);
  if (fromUnderlying->isPointer() && to->isPointer() && !dynamic_cast<ConceptType*>(fromUnderlying->removePointer()))
    if (auto conceptType = dynamic_cast<ConceptType*>(to->removePointer())) {
      auto concept = conceptType->getConceptFor(fromUnderlying->removePointer());
      if (dynamic_cast<MutablePointerType*>(to) && !dynamic_cast<MutablePointerType*>(fromUnderlying))
        return "Cannot cast value of type " + quote(fromUnderlying->getName()) + " to a mutable pointer";
      auto functions = TRY(getRequiredFunctionsForConceptType(*this, *concept, CodeLoc()));
      for (auto& fun : functions)
        CHECK(!!fun->addInstance(*this));
      if (expr) {
        auto loc = expr->codeLoc;
        expr = unique<FatPointerConversion>(loc, functions, to, fromUnderlying, std::move(expr), conceptType);
        auto err = expr->getTypeImpl(*this);
        CHECK(!!err) << err.get_error();
      }
      return success;
    }
  if (auto structType = dynamic_cast<StructType*>(toUnderlying))
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
              call->functionInfo = structType->staticContext.getFunctions(alternative->name, false).getOnlyElement();
              return unique<StatementExpression>(codeLoc,
                  makeVec<unique_ptr<Statement>>(unique<ExpressionStatement>(std::move(expr))),
                  cast<Expression>(std::move(call)));
            } else {
              auto call = unique<FunctionCall>(IdentifierInfo("bogus", codeLoc), std::move(expr), false);
              call->functionInfo = structType->staticContext.getFunctions(alternative->name, false).getOnlyElement();
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
  if (contains(getConversions(from, to, true), to) ||
      (from == BuiltinType::NULL_TYPE && dynamic_cast<OptionalType*>(to) &&
          dynamic_cast<OptionalType*>(to)->underlying != BuiltinType::NULL_TYPE))
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

void Context::replace(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  CHECK(state->varsList.empty());
  /*for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to, errors);
  }*/
  for (auto& function : state->functions) {
    for (auto& overload : function.second) {
      //std::cout << "Replaced " << overload->prettyString() << std::endl;
      overload = replaceInFunction(context, overload, from, to, errors);
      //std::cout << "To " << overload->prettyString() << std::endl;
    }
  }
  state->typesSet.clear();
  for (auto& type : state->types) {
    type.second = type.second->replace(context, from, to, errors);
    state->typesSet.insert(type.second);
  }
}

void Context::expand(Type* from, vector<Type*> to, ErrorBuffer& errors) {
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

void Context::addExpandedTypePack(const string& name, vector<Type*> t) {
  state->expandedTypePack = make_pair(std::move(name), std::move(t));
}

void Context::addUnexpandedTypePack(string name, Type* t) {
  state->unexpandedTypePack = make_pair(std::move(name), std::move(t));
}

void Context::addUnexpandedVariablePack(string name, Type* t) {
  state->unexpandedVariablePack = make_pair(std::move(name), std::move(t));
}

void Context::addExpandedVariablePack(const string& name, vector<Type*> t) {
  state->expandedVariablePack = make_pair(std::move(name), std::move(t));
}

WithError<pair<string, vector<Type*>>> Context::getExpandedVariablePack() const {
  for (auto state : getReversedStates())
    if (state->expandedVariablePack)
      return *state->expandedVariablePack;
  return "No variable pack is present in this context"s;
}

optional<pair<string, vector<Type*>>> Context::getExpandedTypePack() const {
  for (auto state : getReversedStates())
    if (state->expandedTypePack)
      return state->expandedTypePack;
  return none;
}

optional<pair<string, Type*> > Context::getUnexpandedTypePack() const {
  for (auto state : getReversedStates()) {
    if (state->expandedTypePack)
      return none;
    if (state->unexpandedTypePack)
      return state->unexpandedTypePack;
  }
  return none;
}

optional<pair<string, Type*> > Context::getUnexpandedVariablePack() const {
  for (auto state : getReversedStates()) {
    if (state->expandedVariablePack)
      return none;
    if (state->unexpandedVariablePack)
      return state->unexpandedVariablePack;
  }
  return none;
}

void Context::addType(const string& name, Type* t) {
  //CHECK(!getType(name) || !isFullyDefined(getType(name).get().get())) << name;
  state->types.insert({name, t});
}

void Context::setTypeFullyDefined(Type* t) {
  state->typesSet.insert(t);
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

void Context::setAttribute(Type* t, Type* attr, vector<Type*> templateParams) {
  CHECK(addFunction(FunctionInfo::getImplicit(AttributeTag{},
      FunctionSignature(BuiltinType::VOID, {t, attr}, std::move(templateParams)))));
}

void Context::setStructMembers(Type* t, vector<Type*> members, vector<Type*> templateParams) {
  CHECK(addFunction(FunctionInfo::getImplicit(StructMembersTag{},
      FunctionSignature(BuiltinType::VOID, concat({std::move(t)}, std::move(members)), std::move(templateParams)))));
}

WithErrorLine<vector<Type*>> Context::getTypeList(const vector<TemplateParameterInfo>& ids, bool variadic) const {
  vector<Type*> params;
  for (int i = 0; i < ids.size(); ++i) {
    auto getType = [id = ids[i]] (const Context& context, bool variadic) {
      return id.visit(
          [&](const IdentifierInfo& id) -> WithErrorLine<Type*> {
            return TRY(context.getTypeFromString(id, none))->removeValueReference();
          },
          [&](const shared_ptr<Expression>& expr) -> WithErrorLine<Type*> {
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
      auto signature = replaceInFunction(*this, fun->type, i->getParams()[0], ConceptType::get(i, typeParams, i->isVariadic()),
          errors, fun->type.templateParams);
      CHECK(!!signature.concept);
      CHECK(!!addFunction(addTemplateParams(
          FunctionInfo::getImplicit(fun->id, std::move(signature)),
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

Type* Context::getType(const string& s) const {
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
    auto parent = s->parent;
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

vector<Type*> Context::getAllTypes() const {
  vector<Type*> ret;
  for (auto state : getReversedStates())
    for (auto& type : state->types)
      ret.push_back(type.second);
  return ret;
}

JustError<string> Context::addImplicitFunction(FunctionId id, FunctionSignature type) {
  return addFunction(FunctionInfo::getImplicit(std::move(id), std::move(type)));
}

vector<FunctionInfo*> Context::getFunctions(FunctionId name, bool compileTime) const {
  vector<FunctionInfo*> ret;
  for (auto state : getReversedStates()) {
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
    if (compileTime)
      if (auto id = name.getValueMaybe<string>())
        if (auto fun = getReferenceMaybe(state->builtInFunctions, *id))
          ret.push_back(fun->functionInfo);
  }
  return ret;
}

vector<FunctionInfo*> Context::getAllFunctions() const {
  vector<FunctionInfo*> ret;
  for (auto state : getReversedStates())
    for (auto& overloadSet : state->functions)
      for (auto& fun : overloadSet.second)
        ret.push_back(fun);
  return ret;
}

Type* Context::getVariable(const string& name) const {
  for (auto state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name).type;
  return nullptr;
}

WithErrorLine<Type*> Context::getTypeFromString(IdentifierInfo id, optional<bool> typePack) const {
  if (id.parts.size() > 1)
    return id.codeLoc.getError("Bad type identifier: " + id.prettyString());
  Type* topType = nullptr;
  WithErrorLine<Type*> ret = [&]() -> WithErrorLine<Type*> {
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
      auto templateParams = TRY(getTypeList(id.parts[0].templateArguments, variadic));
      if (concept->getParams().size() - 2 > templateParams.size() ||
          ((!concept->isVariadic() || variadic) && templateParams.size() != concept->getParams().size() - 1))
        return id.codeLoc.getError("Wrong number of template parameters to concept type " + quote(concept->getName()));
      auto type = ConceptType::get(concept.get(), std::move(templateParams), variadic);
      concept->def->addConceptType(type);
      return (Type*)type;
    } else
      topType = getType(name);
    if (!topType)
      return id.codeLoc.getError("Type not found: " + quote(name));
    if (typePack) {
      auto activePack = getUnexpandedTypePack();
      bool match = activePack && topType == activePack->second;
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
              if (auto value = dynamic_cast<CompileTimeValue*>(type->value))
                if (value->getType() == BuiltinType::INT) {
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
  if (!getFunctions(name, true).empty())
    return desc + " conflicts with existing function";
  return success;
}

JustError<std::string> Context::checkNameConflictExcludingFunctions(const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  if (auto t = getType(name))
    if (isFullyDefined(t))
      return desc + " conflicts with an existing type";
  if (getVariable(name))
    return desc + " conflicts with an existing variable or function";
  return success;
}

JustError<string> Context::addFunction(FunctionInfo* info) {
  auto& overloads = state->functions[info->id];
  for (auto& fun : overloads)
    if ((!info->id.contains<ConstructorTag>() || fun->type.retVal == info->type.retVal) &&
        areParamsEquivalent(fun->type, info->type) &&
        fun->type.generatedConstructor == info->type.generatedConstructor)
      return "Can't overload " + info->prettyString() + " with the same argument types."s;
  overloads.push_back(info);
  return success;
}

vector<FunctionInfo*> Context::getConstructorsFor(Type* type) const {
  vector<FunctionInfo*> ret;
  for (auto& fun : getFunctions(ConstructorTag{}, false)) {
    if (fun->type.retVal == type) {
      ret.push_back(fun);
    } else
    if (auto structType = dynamic_cast<StructType*>(fun->type.retVal))
      if (auto origStruct = dynamic_cast<StructType*>(type))
        if (structType->parent == origStruct->parent) {
          ret.push_back(fun);
        }
  }
  return ret;
}

WithError<vector<FunctionInfo*>> Context::getFunctionTemplate(IdentifierInfo id, bool compileTimeArgs) const {
  vector<FunctionInfo*> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts[0], id.codeLoc)))
      return (*type)->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart(), compileTimeArgs);
    else
      return "Type not found: " + id.prettyString();
  } else {
    string funName = id.parts[0].name;
    if (auto type = getType(funName))
      ret = getConstructorsFor(type);
    else
      ret = getFunctions(funName, compileTimeArgs);
  }
  return ret;
}

WithEvalError<Type*> Context::BuiltInFunctionInfo::invokeFunction(const Context& context, const string& id, CodeLoc loc,
    vector<Type*> args, vector<CodeLoc> argLoc) const {
  CHECK(functionInfo->type.params.size() == args.size());
  for (auto t : args)
    if (!t->getMangledName()) {
      return (Type*)CompileTimeValue::get(
          CompileTimeValue::TemplateFunctionCall{id, args,  functionInfo->type.retVal, loc, argLoc, *this});
    }
  if (auto res = fun(context, args))
    return *res;
  else
    return EvalError::withError(res.get_error());
}

WithEvalError<Type*> Context::invokeFunction(const string& id, CodeLoc loc, vector<Type*> args,
    vector<CodeLoc> argLoc) const {
  for (auto state : getReversedStates())
    if (auto f = getReferenceMaybe(state->builtInFunctions, id))
      return f->invokeFunction(*this, id, loc, args, argLoc);
  return EvalError::noEval();
}

void Context::addBuiltInFunction(const string& id, Type* returnType, vector<Type*> argTypes, BuiltInFunction fun) {
  CHECK(!state->builtInFunctions.count(id));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{
      FunctionInfo::getImplicit(id, FunctionSignature(returnType, argTypes, {})), std::move(fun)}));
}

FunctionInfo* Context::getBuiltinOperator(Operator op, vector<Type*> argTypes) const {
  auto functionType = [&] () -> optional<FunctionSignature> {
    switch (op) {
      case Operator::GET_ADDRESS:
        if (argTypes.size() == 1) {
          if (auto pointerType = convertReferenceToPointerStrict(argTypes[0]))
            return FunctionSignature(pointerType, {argTypes[0]}, {});
          else // this codegens a call to the op_get_address function, which returns the address of a temporary object
            return FunctionSignature(PointerType::get(argTypes[0]), {argTypes[0]}, {});
        }
        break;
      case Operator::POINTER_DEREFERENCE:
        if (argTypes.size() == 1) {
          if (auto referenceType = convertPointerToReferenceStrict(argTypes[0]->removeReference()))
            return FunctionSignature(referenceType, {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::ASSIGNMENT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->removeReference()))
          if (auto referenceType = dynamic_cast<MutableReferenceType*>(argTypes[0]))
            return FunctionSignature(BuiltinType::VOID, {argTypes[0], referenceType->underlying}, {}).setBuiltin();
        break;
      case Operator::SUBSCRIPT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], BuiltinType::INT))
          if (auto pack = dynamic_cast<VariablePack*>(argTypes[0]))
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

vector<FunctionInfo*> Context::getOperatorType(Operator op) const {
  return getFunctions(op, false);
}
