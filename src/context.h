#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "identifier.h"
#include "variables.h"

struct IdentifierInfo;
struct Type;
struct FunctionType;

class Context {
  public:
  const Variables& getVariables() const;
  Variables& getVariables();
  const Variables& getAlternatives() const;
  Variables& getAlternatives();
  const Variables& getConstants() const;
  Variables& getConstants();
  void merge(const Context&);
  void replace(SType from, SType to);
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
  Variables variables;
  Variables alternatives;
  Variables constants;
  map<string, SType> types;
  map<string, FunctionType> functions;
  map<Operator, FunctionType> operators;
  nullable<SType> returnType;
  vector<SType> getTypeList(const vector<IdentifierInfo>&) const;
  vector<string> imports;
  vector<string> allImports;
};
