#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "type.h"
#include "identifier.h"

struct IdentifierInfo;

class State {
  public:

  optional<Type> getTypeOfVariable(const IdentifierInfo& ident) const;
  void addVariable(const string& ident, Type);
  const optional<Type>& getReturnType() const;
  void setReturnType(Type);
  void addType(const string& name, Type);
  optional<Type> getTypeFromString(IdentifierInfo) const;
  bool typeNameExists(const string&) const;
  void addFunction(string, FunctionType);
  FunctionType getFunction(CodeLoc, IdentifierInfo, vector<Type> argTypes, vector<CodeLoc> argLoc) const;
  vector<string> getFunctionParamNames(CodeLoc, IdentifierInfo) const;

  private:
  unordered_map<string, Type> vars;
  unordered_map<string, Type> types;
  unordered_map<string, FunctionType> functions;
  optional<Type> returnType;
  vector<Type> getTypeList(const vector<IdentifierInfo>&) const;
  FunctionType getFunctionTemplate(CodeLoc, IdentifierInfo) const;
};
