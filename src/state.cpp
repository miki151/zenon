#include "state.h"
#include "ast.h"
#include "type.h"

optional<Type> State::getTypeOfVariable(const IdentifierInfo& id) const {
  if (id.parts.size() == 2 && types.count(id.parts[0].toString())) {
    auto type = types.at(id.parts[0].toString());
    if (auto enumType = type.getReferenceMaybe<EnumType>()) {
      string element = id.parts[1].toString();
      id.codeLoc.check(contains(enumType->elements, element),
          "No element named " + quote(element) + " in enum " + quote(enumType->name));
      return Type(*enumType);
    } else
      id.codeLoc.error("Unrecognized variable or constant: " + quote(id.toString()));
  }
  id.codeLoc.check(id.parts.size() == 1 && id.parts.at(0).templateArguments.empty(),
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
  if (!types.count(name))
    return none;
  auto ret = instantiate(types.at(name), getTypeList(id.parts.at(0).templateArguments));
  if (ret && id.pointer)
    ret = Type(PointerType(*ret));
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
      INFO << "Looking for static method in type " << getName(*type);
      if (auto fun = getStaticMethod(*type, id.parts.at(1).name)) {
        fun->parentType = *type;
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
  return functions.at(funName);
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

FunctionType State::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType, IdentifierInfo id, vector<Type> argTypes,
    vector<CodeLoc> argLoc) const {
  auto templateArgNames = id.parts.back().templateArguments;
  auto templateArgs = getTypeList(templateArgNames);
  instantiate(templateType, codeLoc, templateArgs, argTypes, argLoc);
  INFO << "Function " << id.toString() << " return type " << getName(*templateType.retVal);
  return templateType;
}

optional<FunctionType> State::getOperatorType(Operator op) const {
  if (operators.count(op))
    return operators.at(op);
  else
    return none;
}
