#include "context.h"
#include "ast.h"
#include "type.h"


Context Context::withParent(const Context& c) {
  Context ret;
  ret.parentStates = c.parentStates;
  ret.parentStates.push_back(c.state);
  return ret;
}

Context Context::withParent(vector<Context*> parents) {
  Context ret;
  for (auto context : parents) {
    append(ret.parentStates, context->parentStates);
    ret.parentStates.push_back(context->state);
  }
  return ret;
}

void Context::merge(const Context& context) {
  append(parentStates, context.parentStates);
  parentStates.push_back(context.state);
}

void Context::mergeAndCollapse(const Context& context) {
  for (auto& s : context.getReversedStates())
    state->merge(*s);
}

Context::Context() : state(shared<State>()) {
}

void Context::State::merge(const Context::State& o) {
  for (auto& f : o.functions) {
    CHECK(!functions.count(f.first));
    functions.insert(f);
  }
}

void Context::deepCopyFrom(const Context& c) {
  CHECK(parentStates.empty());
  *state = *c.state;
  for (auto s : c.parentStates)
    state->merge(*s);
}

static string getFunctionIdName(const FunctionId& id) {
  return id.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return "operator "s + getString(op); },
      [&](SType type) { return type->getName(); }
  );
}

static string getFunctionNameForError(const FunctionType& function) {
  string typePrefix;
  if (function.parentType)
    typePrefix = function.parentType->getName() + "::";
  return function.retVal->getName() + " " + typePrefix + getFunctionIdName(function.name) + "(" +
      combine(transform(function.params, [](const auto& t) { return t.type->getName(); }), ", ") + ")";
}

bool Context::areParamsEquivalent(const FunctionType& f1, const FunctionType& f2) const {
  auto checkFun = [this](const FunctionType& f1, const FunctionType& f2) {
    return !!instantiateFunction(*this, f1, CodeLoc(), {}, transform(f2.params, [](const auto& param) { return param.type; }),
        vector<CodeLoc>(f2.params.size(), CodeLoc()), {});
  };
  return checkFun(f1, f2) && checkFun(f2, f1);
}

bool Context::isGeneralization(const FunctionType& general, const FunctionType& specific,
    vector<FunctionType> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (!specific.name.contains<SType>() && general.name != specific.name)
    return false;
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, transform(specific.params, [](const auto& param) { return param.type; }),
      vector<CodeLoc>(specific.params.size(), CodeLoc()), existing)) {
    return specific.name == inst->name && specific.retVal == inst->retVal;
  }
  else
    return false;
}

static bool isBuiltinCopyConstructor(const FunctionType& f) {
  if (auto type = f.name.getValueMaybe<SType>())
    return f.retVal == *type && f.params.size() == 1 && f.params[0].type == PointerType::get(*type)
        && (*type)->isBuiltinCopyable();
  return false;
}

vector<FunctionType> Context::getMissingFunctions(const Context& required, vector<FunctionType> existing) const {
  //cout << "Looking for missing functions in:\n";
  //required.print();
  vector<FunctionType> ret;
  for (auto otherState : required.getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        if (isBuiltinCopyConstructor(function))
          continue;
        //cout << "Looking for function: " << getFunctionNameForError(function) << "\n";
        //print();
        bool found = false;
        for (auto& myFun : getAllFunctions())
          if (isGeneralization(myFun, function, existing)) {
            found = true;
            break;
          }
        if (!found)
          ret.push_back(function);
      }
  }
  return ret;
}

WithError<SType> Context::getTypeOfVariable(const string& id) const {
  for (auto& state : getReversedStates()) {
    if (contains(state->movedVars, id))
      return "Variable has been moved: " + id;
    if (state->vars.count(id))
      return state->vars.at(id);
  }
  return "Variable not found: " + id;
}

optional<string> Context::setVariableAsMoved(const string& id) {
  for (auto& state : getReversedStates())
    if (state->vars.count(id)) {
      state->movedVars.push_back(id);
      return none;
    }
  return "Variable not found: " + id;
}

void Context::addVariable(const string& ident, SType t) {
  state->vars.insert({ident, t});
  state->varsList.push_back(ident);
}

const vector<string>& Context::getBottomLevelVariables() const {
  return state->varsList;
}

void Context::State::print() const {
  for (auto& varName : varsList) {
    auto& var = vars.at(varName);
    cout << "Variable " << quote(varName) << " of type " << var->getName() << "\n";
  }
  for (auto& function : functions) {
    cout << "Function " << quote(getFunctionIdName(function.first)) << " overloads: \n";
    for (auto& overload : function.second)
      cout << getFunctionNameForError(overload) << "\n";
  }
  for (auto& type : types)
    cout << "Type: " << type.second->getName() << "\n";
}

void Context::print() const {
  for (auto& state : getReversedStates())
    state->print();
}

vector<SType> Context::getConversions(SType type) const {
  vector<SType> ret = {type};
  if (canCopyConstruct(type->getUnderlying()) && type != type->getUnderlying())
    ret.push_back(type->getUnderlying());
  if (type->getUnderlying() == ArithmeticType::INT)
    ret.push_back(ArithmeticType::DOUBLE);
  if (auto ptr = type->getUnderlying().dynamicCast<MutablePointerType>())
    ret.push_back(PointerType::get(ptr->underlying));
  return ret;
}

bool Context::canConvert(SType from, SType to) const {
  return contains(getConversions(from), to);
}

void Context::replace(SType from, SType to) {
  for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to);
  }
  for (auto& function : copyOf(state->functions)) {
    if (auto constructorName = function.first.getValueMaybe<SType>())
      if (*constructorName == from) {
        state->functions.erase(function.first);
        state->functions.insert({to, function.second});
      }
    }
  for (auto& function : state->functions) {
    for (auto& overload : function.second)
      replaceInFunction(overload, from, to);
  }
  for (auto& type : state->types)
    type.second = type.second->replace(from, to);
}

const Context::State& Context::getTopState() const {
  if (parentStates.empty())
    return *state;
  else
    return *parentStates.front();
}

vector<shared_ptr<const Context::State>> Context::getReversedStates() const {
  vector<shared_ptr<const Context::State>> ret { state };
  for (auto& state : reverse(parentStates))
    ret.push_back(state);
  return ret;
}

nullable<SType> Context::getReturnType() const {
  for (auto& state : getReversedStates())
    if (state->returnType)
      return state->returnType;
  return nullptr;
}

void Context::setReturnType(SType t) {
  CHECK(!getReturnType()) << "Attempted to overwrite return type";
  state->returnType = t;
}

void Context::addType(const string& name, SType t) {
  CHECK(!getTypeFromString(IdentifierInfo(name)));
  state->types.insert({name, t});
}

vector<SType> Context::getTypeList(const vector<IdentifierInfo>& ids) const {
  vector<SType> params;
  for (auto& id : ids)
    params.push_back(getTypeFromString(id).get(id.codeLoc));
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

vector<FunctionType> Context::getFunctions(FunctionId name) const {
  vector<FunctionType> ret;
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

vector<FunctionType> Context::getAllFunctions() const {
  vector<FunctionType> ret;
  for (auto& state : getReversedStates())
    for (auto& fun : state->functions)
      append(ret, fun.second);
  return ret;
}

nullable<SType> Context::getVariable(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name);
  return nullptr;
}

WithError<SType> Context::getTypeFromString(IdentifierInfo id) const {
  id.codeLoc.check(id.parts.size() == 1, "Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  auto topType = getType(name);
  if (!topType)
    return "Type not found: " + quote(name);
  auto ret = topType->instantiate(*this, getTypeList(id.parts.at(0).templateArguments));
  if (ret && id.pointerType)
    switch (*id.pointerType) {
      case IdentifierInfo::CONST:
        *ret = PointerType::get(*ret);
        break;
      case IdentifierInfo::MUTABLE:
        *ret = MutablePointerType::get(*ret);
        break;
    }
  return ret;
}

void Context::checkNameConflict(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!getType(name), desc + " conflicts with an existing type");
  loc.check(!getVariable(name), desc + " conflicts with an existing variable or function");
  loc.check(getFunctions(name).empty(), desc + " conflicts with existing function");
}

void Context::checkNameConflictExcludingFunctions(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!getType(name), desc + " conflicts with an existing type");
  loc.check(!getVariable(name), desc + " conflicts with an existing variable or function");
}

optional<string> Context::addFunction(FunctionType f) {
  auto& overloads = state->functions[f.name];
  for (auto& fun : overloads)
    if (areParamsEquivalent(fun, f))
      return "Can't overload " + quote(getFunctionNameForError(f)) + " with the same argument types."s;
  overloads.push_back(f);
  return none;
}

WithError<vector<FunctionType>> Context::getFunctionTemplate(IdentifierInfo id) const {
  vector<FunctionType> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0))))
      return (*type)->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
    else
      return "Type not found: " + id.toString();
  } else {
    string funName = id.parts.at(0).name;
    append(ret, getFunctions(funName));
    if (auto type = getType(funName))
      append(ret, getFunctions(type.get()));
  }
  return ret;
}

void Context::pushImport(const string& name, size_t contentHash) {
  CHECK(parentStates.empty());
  state->imports.push_back(name);
  state->allImports.push_back(name);
  state->importHashes.push_back(contentHash);
  state->allImportHashes.insert(contentHash);
}

void Context::popImport() {
  state->imports.pop_back();
  state->importHashes.pop_back();
}

bool Context::wasEverImported(size_t contentHash) {
  return getTopState().allImportHashes.count(contentHash);
}

bool Context::isCurrentlyImported(size_t contentHash) {
  return contains(getTopState().importHashes, contentHash);
}

const vector<string>& Context::getCurrentImports() const {
  return getTopState().imports;
}

const vector<string>& Context::getAllImports() const {
  return getTopState().allImports;
}

WithErrorLine<FunctionType> Context::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType,
    IdentifierInfo id, vector<SType> argTypes, vector<CodeLoc> argLoc) const {
  auto templateArgNames = id.parts.back().templateArguments;
  auto templateArgs = getTypeList(templateArgNames);
  auto ret = instantiateFunction(*this, templateType, codeLoc, templateArgs, argTypes, argLoc, {});
  return ret;
}

vector<FunctionType> Context::getOperatorType(Operator op) const {
  return getFunctions(op);
}

FunctionId Context::getFunctionId(const FunctionName& name) const {
  return name.visit(
      [this](ConstructorId id) -> FunctionId { return getType(id.name).get(); },
      [](const string& name) -> FunctionId { return name; },
      [](Operator op) -> FunctionId { return op; }
  );
}

bool Context::canConstructWith(SType type, vector<SType> argsRef) const {
//  cout << "Trying to construct " << type->getName() << " with " << combine(transform(argsRef, [](const auto& t) { return t->getName(); }), ", ") << "\n";
//  print();
  auto args = transform(argsRef, [](const auto& arg) { return arg->getUnderlying();});
  if (type->isBuiltinCopyable() && argsRef.size() == 1 && argsRef[0] == PointerType::get(type))
    return true;
  if (args.size() == 1 && args[0] == type)
    return true;
  vector<SType> templateParams;
  if (auto s = type.dynamicCast<StructType>()) {
    type = s->parent.get();
    templateParams = s->templateParams;
  }
  for (auto f : getFunctions(type))
    if (instantiateFunction(*this, f, CodeLoc(), templateParams, args, vector<CodeLoc>(args.size())))
      return true;
  return false;
}

bool Context::canCopyConstruct(SType t) const {
  return canConstructWith(t, {PointerType::get(t)});
}

optional<std::string> Context::addCopyConstructorFor(SType type, const vector<SType>& templateParams) {
  auto constructor = FunctionType(type, FunctionCallType::CONSTRUCTOR, type, {{PointerType::get(type)}}, templateParams);
  constructor.parentType = type;
  return addFunction(constructor);
}
