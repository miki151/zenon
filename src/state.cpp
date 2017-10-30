#include "state.h"
#include "ast.h"
#include "type.h"

optional<Type> State::getTypeOfVariable(const IdentifierInfo& id) const {
  id.codeLoc.check(id.parts.size() == 1 && id.parts.at(0).templateParams.empty(),
      "Bad variable identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  if (vars.count(name))
    return vars.at(name);
  else
    return none;
}

void State::addVariable(const string& id, Type t) {
  vars[id] = t;
}

const optional<Type>& State::getReturnType() const {
  return returnType;
}

void State::setReturnType(Type t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}

void State::addType(const string& name, Type t) {
  types[name] = t;
}

vector<Type> State::getTypeList(const vector<IdentifierInfo>& ids) const {
  vector<Type> params;
  for (auto& id : ids)
    if (auto type = getTypeFromString(id))
      params.push_back(*type);
    else
      id.codeLoc.error("Unrecognized type: " + quote(id.toString()));
  return params;
}

optional<Type> State::getTypeFromString(IdentifierInfo id) const {
  //INFO << "Get type " << id.toString();
  id.codeLoc.check(id.parts.size() == 1, "Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  //id.templateParams.clear();
  if (!types.count(name))
    return none;
  auto params = id.parts.at(0).templateParams;
  return instantiate(types.at(name), getTypeList(params), params);
}

bool State::typeNameExists(const string& name) const {
  return types.count(name);
}

void State::addFunction(string id, FunctionType f) {
  INFO << "Inserting function " << id;
  CHECK(!functions.count(id));
  functions.insert(make_pair(id, f));
}

FunctionType State::getFunction(CodeLoc codeLoc, IdentifierInfo idOrig) const {
  /*INFO << "Looking up function " << id.toString();
  INFO << "Functions:";
  for (auto& elem : functions)
    INFO << elem.first.toString();*/
  auto templateParamNames = idOrig.parts.at(0).templateParams;
  string funName = idOrig.parts.at(0).name;
  optional<FunctionType> ret;
  if (idOrig.parts.size() == 2) {
    if (auto type = getTypeFromString(IdentifierInfo(idOrig.parts.at(0)))) {
      INFO << "Looking for static method in type " << getName(*type);
      if (auto fun = getStaticMethod(*type, idOrig.parts.at(1).name)) {
        templateParamNames = idOrig.parts.at(1).templateParams;
        ret = *fun;
      } else
        idOrig.codeLoc.error("Static method not found: " + idOrig.toString());
    } else
      idOrig.codeLoc.error("Type not found: " + idOrig.toString());
  } else {
    idOrig.codeLoc.check(idOrig.parts.size() == 1, "Bad function identifier: " + idOrig.toString());
    if (functions.count(funName))
      ret = functions.at(funName);
  }
  codeLoc.check(!!ret, "Function not found: " + quote(funName));
  ret = instantiate(*ret, getTypeList(templateParamNames), templateParamNames);
  if (!ret)
    codeLoc.error("Can't instantiate template function " + quote(idOrig.toString()));
  INFO << "Function " << idOrig.toString() << " return type " << getName(*ret->retVal);
  return *ret;
}
