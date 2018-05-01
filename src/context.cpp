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

static bool areEquivalent(const FunctionType& f1, const FunctionType& f2) {
  if (f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i].type != f2.params[i].type)
      return false;
  return f1.retVal == f2.retVal;
}

vector<FunctionType> Context::getMissingFunctions(const Context& required) const {
  vector<FunctionType> ret;
  for (auto otherState : required.getReversedStates()) {
    for (auto& function : otherState->functions) {
      auto myFun = getFunction(function.first);
      if (!myFun || !areEquivalent(*myFun, function.second))
        ret.push_back(function.second);
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
    replaceInFunction(function.second, from, to);
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

const FunctionType* Context::getFunction(FunctionName name) const {
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      return &state->functions.at(name);
  return nullptr;
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
  loc.check(!getFunction(name), desc + " conflicts with existing function");
}

void Context::addFunction(FunctionType f) {
//  INFO << "Inserting function " << id;
  CHECK(!getFunction(f.name));
  state->functions.insert(make_pair(f.name, f));
}

WithError<FunctionType> Context::getFunctionTemplate(IdentifierInfo id) const {
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0)))) {
      auto ret = type->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
      if (ret)
        ret->parentType = type.get();
      return ret;
    }
    return "Type not found: " + id.toString();
  } else {
    string funName = id.parts.at(0).name;
    if (auto fun = getFunction(funName))
      return *fun;
  }
  return "Function not found: " + quote(id.toString());
}

WithError<vector<string>> Context::getFunctionParamNames(IdentifierInfo id) const {
  auto fun = getFunctionTemplate(id);
  if (fun)
    return transform(fun->params, [](const FunctionType::Param& p) { return p.name; });
  else
    return fun.get_error();
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

optional<FunctionType> Context::getOperatorType(Operator op) const {
  if (auto fun = getFunction(op))
    return *fun;
  else
    return none;
}
