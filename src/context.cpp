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

static bool areParamsEquivalent(const FunctionType& f1, const FunctionType& f2) {
  if (f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i].type != f2.params[i].type)
      return false;
  return true;
}

static bool areEquivalent(const FunctionType& f1, const FunctionType& f2) {
  return f1.retVal == f2.retVal && areParamsEquivalent(f1, f2);
}

vector<FunctionType> Context::getMissingFunctions(const Context& required) const {
  vector<FunctionType> ret;
  for (auto otherState : required.getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        bool found = false;
        for (auto& myFun : getFunctions(overloads.first))
          if (areEquivalent(myFun, function)) {
            found = true;
            break;
          }
        if (!found)
          ret.push_back(function);
      }
  }
  return ret;
}

nullable<SType> Context::getTypeOfVariable(const string& s) const {
  return getVariable(s);
}

void Context::addVariable(const string& ident, SType t) {
  state->vars.insert({ident, t});
  state->varsList.push_back(ident);
}

const vector<string>& Context::getBottomLevelVariables() const {
  return state->varsList;
}

void Context::replace(SType from, SType to) {
  for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to);
  }
  for (auto& function : state->functions)
    for (auto& overload : function.second)
      replaceInFunction(overload, from, to);
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
    if (auto type = getTypeFromString(id))
      params.push_back(type.get());
    else
      id.codeLoc.error("Unrecognized type: " + quote(id.toString()));
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

vector<FunctionType> Context::getFunctions(FunctionName name) const {
  vector<FunctionType> ret;
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

nullable<SType> Context::getVariable(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name);
  return nullptr;
}

nullable<SType> Context::getTypeFromString(IdentifierInfo id) const {
  id.codeLoc.check(id.parts.size() == 1, "Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  auto topType = getType(name);
  if (!topType)
    return nullptr;
  auto ret = topType->instantiate(id.codeLoc, getTypeList(id.parts.at(0).templateArguments));
  if (ret && id.pointer)
    ret = PointerType::get(ret.get());
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

static string getFunctionNameForError(const FunctionType& function) {
  string typePrefix;
  if (function.parentType)
    typePrefix = function.parentType->getName() + "::";
  return typePrefix + function.name.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return "operator "s + getString(op); },
      [&](ConstructorId) { return function.parentType->getName(false); }
  );
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
      return type->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
    else
      return "Type not found: " + id.toString();
  } else {
    string funName = id.parts.at(0).name;
    append(ret, getFunctions(funName));
    if (auto type = getType(funName))
      append(ret, type->getStaticContext().getFunctions(ConstructorId{}));
  }
  return ret;
}

void Context::pushImport(const string& name) {
  CHECK(parentStates.empty());
  state->imports.push_back(name);
  state->allImports.push_back(name);
}

void Context::popImport() {
  state->imports.pop_back();
}

const vector<string>& Context::getImports() const {
  return getTopState().imports;
}

const vector<string>& Context::getAllImports() const {
  return getTopState().allImports;
}

WithErrorLine<FunctionType> Context::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType, IdentifierInfo id, vector<SType> argTypes,
    vector<CodeLoc> argLoc) const {
  auto templateArgNames = id.parts.back().templateArguments;
  auto templateArgs = getTypeList(templateArgNames);
  return instantiateFunction(templateType, codeLoc, templateArgs, argTypes, argLoc);
}

vector<FunctionType> Context::getOperatorType(Operator op) const {
  return getFunctions(op);
}

vector<FunctionType> Context::getConstructorType() const {
  return getFunctions(ConstructorId{});
}
