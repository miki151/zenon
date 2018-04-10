#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "type.h"
#include "identifier.h"

struct IdentifierInfo;

class State {
  public:

  optional<Type> getTypeOfVariable(const string&) const;
  void addVariable(const string& ident, Type);
  const optional<Type>& getReturnType() const;
  void setReturnType(Type);
  void addType(const string& name, Type);
  optional<Type> getTypeFromString(IdentifierInfo) const;
  void addFunction(variant<string, Operator> nameOrOp, FunctionType);
  vector<string> getFunctionParamNames(CodeLoc, IdentifierInfo) const;
  FunctionType getFunctionTemplate(CodeLoc, IdentifierInfo) const;
  FunctionType instantiateFunctionTemplate(CodeLoc, FunctionType, IdentifierInfo, vector<Type> argTypes, vector<CodeLoc> argLoc) const;
  optional<FunctionType> getOperatorType(Operator) const;
  void pushImport(const string& name);
  void popImport();
  const vector<string>& getImports() const;
  const vector<string>& getAllImports() const;
  void checkNameConflict(CodeLoc loc, const string& name, const string& type) const;

  private:
  unordered_map<string, Type> vars;
  unordered_map<string, Type> types;
  unordered_map<string, FunctionType> functions;
  map<Operator, FunctionType> operators;
  optional<Type> returnType;
  vector<Type> getTypeList(const vector<IdentifierInfo>&) const;
  vector<string> imports;
  vector<string> allImports;
};
