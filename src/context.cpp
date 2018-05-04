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
  auto checkFun = [](const FunctionType& f1, const FunctionType& f2) {
    return !!instantiateFunction(f1, CodeLoc(), {}, transform(f2.params, [](const auto& param) { return param.type; }),
        vector<CodeLoc>(f2.params.size(), CodeLoc()));
  };
  return checkFun(f1, f2) && checkFun(f2, f1);
}

static bool isGeneralization(const FunctionType& general, const FunctionType& specific) {
  if (auto inst = instantiateFunction(general, CodeLoc(), {}, transform(specific.params, [](const auto& param) { return param.type; }),
      vector<CodeLoc>(specific.params.size(), CodeLoc())))
    return specific.retVal == inst->retVal;
  else
    return false;
}

vector<FunctionType> Context::getMissingFunctions(const Context& required) const {
  vector<FunctionType> ret;
  for (auto otherState : required.getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        bool found = false;
        for (auto& myFun : getFunctions(overloads.first))
          if (isGeneralization(myFun, function)) {
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

static string getFunctionIdName(const FunctionId& id) {
  return id.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return "operator "s + getString(op); },
      [&](SType type) { return type->getName(false); }
  );
}

static string getFunctionNameForError(const FunctionType& function) {
  string typePrefix;
  if (function.parentType)
    typePrefix = function.parentType->getName() + "::";
  return typePrefix + getFunctionIdName(function.name) + "(" +
      combine(transform(function.params, [](const auto& t) { return t.type->getName(); }), ", ") + ")";
;
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
  if (ret && id.pointer)
    *ret = PointerType::get(*ret);
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
  auto ret = instantiateFunction(templateType, codeLoc, templateArgs, argTypes, argLoc);
  if (ret)
    for (auto& concept : ret->requirements) {
      auto missing = getMissingFunctions(concept->context);
      if (!missing.empty())
        return codeLoc.getError("Required function not implemented: " +
            missing[0].toString() + ", required by concept: " + quote(concept->getName()));
    }
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
  //cout << "Trying to construct " << type->getName() << " with " << combine(transform(argsRef, [](const auto& t) { return t->getName(); }), ", ") << "\n";
  //print();
  auto args = transform(argsRef, [](const auto& arg) { return arg->getUnderlying();});
  if (args.size() == 1 && args[0] == type)
    return true;
  vector<SType> templateParams;
  if (auto s = type.dynamicCast<StructType>()) {
    type = s->parent.get();
    templateParams = s->templateParams;
  }
  for (auto f : getFunctions(type))
    if (instantiateFunction(f, CodeLoc(), templateParams, args, vector<CodeLoc>(args.size())))
      return true;
  return false;
}
