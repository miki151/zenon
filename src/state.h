#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "type.h"
#include "identifier.h"

struct IdentifierInfo;

class State {
  public:

  nullable<SType> getTypeOfVariable(const string&) const;
  void addVariable(const string& ident, SType);
  nullable<SType> getReturnType() const;
  void setReturnType(SType);
  void addType(const string& name, SType);
  nullable<SType> getTypeFromString(IdentifierInfo) const;
  void addFunction(variant<string, Operator> nameOrOp, FunctionType);
  vector<string> getFunctionParamNames(CodeLoc, IdentifierInfo) const;
  FunctionType getFunctionTemplate(CodeLoc, IdentifierInfo) const;
  FunctionType instantiateFunctionTemplate(CodeLoc, FunctionType, IdentifierInfo, vector<SType> argTypes, vector<CodeLoc> argLoc) const;
  optional<FunctionType> getOperatorType(Operator) const;
  void pushImport(const string& name);
  void popImport();
  const vector<string>& getImports() const;
  const vector<string>& getAllImports() const;
  void checkNameConflict(CodeLoc loc, const string& name, const string& type) const;

  private:
  unordered_map<string, SType> vars;
  unordered_map<string, SType> types;
  unordered_map<string, FunctionType> functions;
  map<Operator, FunctionType> operators;
  nullable<SType> returnType;
  vector<SType> getTypeList(const vector<IdentifierInfo>&) const;
  vector<string> imports;
  vector<string> allImports;
};
