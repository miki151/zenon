#include "state.h"
#include "ast.h"
#include "type.h"

nullable<SType> State::getTypeOfVariable(const string& name) const {
  if (vars.count(name))
    return vars.at(name);
  else
    return nullptr;
}

void State::addVariable(const string& id, SType t) {
  CHECK(!vars.count(id));
  vars.insert({id, t});
}

nullable<SType> State::getReturnType() const {
  return returnType;
}

void State::setReturnType(SType t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}

void State::addType(const string& name, SType t) {
  CHECK(!types.count(name));
  types.insert({name, t});
}

vector<SType> State::getTypeList(const vector<IdentifierInfo>& ids) const {
  vector<SType> params;
  for (auto& id : ids)
    if (auto type = getTypeFromString(id))
      params.push_back(type.get());
    else
      id.codeLoc.error("Unrecognized type: " + quote(id.toString()));
  return params;
}

nullable<SType> State::getTypeFromString(IdentifierInfo id) const {
  //INFO << "Get type " << id.toString();
  id.codeLoc.check(id.parts.size() == 1, "Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  if (!types.count(name))
    return nullptr;
  auto ret = instantiate(types.at(name), getTypeList(id.parts.at(0).templateArguments));
  if (ret && id.pointer)
    ret = PointerType::get(ret.get());
  return ret;
}

void State::checkNameConflict(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!types.count(name), desc + " conflicts with an existing type");
  loc.check(!vars.count(name), desc + " conflicts with an existing variable or function");
  loc.check(!functions.count(name), desc + " conflicts with existing function");
}

void State::addFunction(variant<string, Operator> nameOrOp, FunctionType f) {
  nameOrOp.visit(
      [&](const string& id) {
        INFO << "Inserting function " << id;
        CHECK(!functions.count(id));
        functions.insert(make_pair(id, f));
      },
      [&](Operator op) {
        INFO << "Inserting operator " << getString(op);
        CHECK(!operators.count(op));
        operators.insert(make_pair(op, f));
      }
  );
}

FunctionType State::getFunctionTemplate(CodeLoc codeLoc, IdentifierInfo id) const {
  string funName = id.parts.at(0).name;
  if (id.parts.size() == 2) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0)))) {
      INFO << "Looking for static method in type " << getName(type.get());
      if (auto fun = getStaticMethod(*type, id.parts.at(1).name)) {
        fun->parentType = type.get();
        return *fun;
      } else
        id.codeLoc.error("Static method not found: " + id.toString());
    } else
      id.codeLoc.error("Type not found: " + id.toString());
  } else {
    id.codeLoc.check(id.parts.size() == 1, "Bad function identifier: " + id.toString());
    if (functions.count(funName))
      return functions.at(funName);
  }
  codeLoc.error("Function not found: " + quote(funName));
}

vector<string> State::getFunctionParamNames(CodeLoc codeLoc, IdentifierInfo id) const {
  auto fun = getFunctionTemplate(codeLoc, id);
  return transform(fun.params, [](const FunctionType::Param& p) { return p.name; });
}

void State::pushImport(const string& name) {
  imports.push_back(name);
  allImports.push_back(name);
}

void State::popImport() {
  imports.pop_back();
}

const vector<string>& State::getImports() const {
  return imports;
}

const vector<string>& State::getAllImports() const {
  return allImports;
}

FunctionType State::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType, IdentifierInfo id, vector<SType> argTypes,
    vector<CodeLoc> argLoc) const {
  auto templateArgNames = id.parts.back().templateArguments;
  auto templateArgs = getTypeList(templateArgNames);
  instantiateFunction(templateType, codeLoc, templateArgs, argTypes, argLoc);
  INFO << "Function " << id.toString() << " return type " << getName(templateType.retVal);
  return templateType;
}

optional<FunctionType> State::getOperatorType(Operator op) const {
  if (operators.count(op))
    return operators.at(op);
  else
    return none;
}
