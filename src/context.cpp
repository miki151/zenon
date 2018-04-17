#include "context.h"
#include "ast.h"
#include "type.h"

void Context::merge(const Context& context) {
  variables.merge(context.variables);
  alternatives.merge(context.alternatives);
  constants.merge(context.constants);
  for (auto& function : context.functions)
    functions.insert(function);
  for (auto& function : context.operators)
    operators.insert(function);
}

const Variables& Context::getVariables() const {
  return variables;
}

Variables& Context::getVariables() {
  return variables;
}

const Variables& Context::getAlternatives() const {
  return alternatives;
}

Variables& Context::getAlternatives() {
  return alternatives;
}

const Variables& Context::getConstants() const {
  return constants;
}

Variables& Context::getConstants() {
  return constants;
}

void Context::replace(SType from, SType to) {
  variables.replace(from, to);
  alternatives.replace(from, to);
  constants.replace(from, to);
  for (auto& function : functions)
    replaceInFunction(function.second, from, to);
  for (auto& function : operators)
    replaceInFunction(function.second, from, to);
}

nullable<SType> Context::getReturnType() const {
  return returnType;
}

void Context::setReturnType(SType t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}

void Context::addType(const string& name, SType t) {
  CHECK(!types.count(name));
  types.insert({name, t});
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

nullable<SType> Context::getTypeFromString(IdentifierInfo id) const {
  //INFO << "Get type " << id.toString();
  id.codeLoc.check(id.parts.size() == 1, "Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  if (!types.count(name))
    return nullptr;
  auto ret = types.at(name)->instantiate(getTypeList(id.parts.at(0).templateArguments));
  if (ret && id.pointer)
    ret = PointerType::get(ret.get());
  return ret;
}

void Context::checkNameConflict(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!types.count(name), desc + " conflicts with an existing type");
  loc.check(!constants.getType(name) && !alternatives.getType(name) && !variables.getType(name),
      desc + " conflicts with an existing variable or function");
  loc.check(!functions.count(name), desc + " conflicts with existing function");
}

void Context::addFunction(variant<string, Operator> nameOrOp, FunctionType f) {
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

FunctionType Context::getFunctionTemplate(CodeLoc codeLoc, IdentifierInfo id) const {
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0)))) {
      auto ret = type->getStaticContext().getFunctionTemplate(codeLoc, id.getWithoutFirstPart());
      ret.parentType = type.get();
      return ret;
    } else
      id.codeLoc.error("Type not found: " + id.toString());
  } else {
    string funName = id.parts.at(0).name;
    id.codeLoc.check(id.parts.size() == 1, "Bad function identifier: " + id.toString());
    if (functions.count(funName))
      return functions.at(funName);
  }
  codeLoc.error("Function not found: " + quote(id.toString()));
}

vector<string> Context::getFunctionParamNames(CodeLoc codeLoc, IdentifierInfo id) const {
  auto fun = getFunctionTemplate(codeLoc, id);
  return transform(fun.params, [](const FunctionType::Param& p) { return p.name; });
}

void Context::pushImport(const string& name) {
  imports.push_back(name);
  allImports.push_back(name);
}

void Context::popImport() {
  imports.pop_back();
}

const vector<string>& Context::getImports() const {
  return imports;
}

const vector<string>& Context::getAllImports() const {
  return allImports;
}

FunctionType Context::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType, IdentifierInfo id, vector<SType> argTypes,
    vector<CodeLoc> argLoc) const {
  auto templateArgNames = id.parts.back().templateArguments;
  auto templateArgs = getTypeList(templateArgNames);
  instantiateFunction(templateType, codeLoc, templateArgs, argTypes, argLoc);
  INFO << "Function " << id.toString() << " return type " << templateType.retVal->getName();
  return templateType;
}

optional<FunctionType> Context::getOperatorType(Operator op) const {
  if (operators.count(op))
    return operators.at(op);
  else
    return none;
}
