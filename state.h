#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "type.h"

class State {
  public:

  optional<Type> getTypeOfVariable(const string& ident) const;
  void setType(const string& ident, Type);
  const optional<Type>& getReturnType() const;
  void setReturnType(Type);
  void addType(const string& name, Type);
  optional<Type> getTypeFromString(const string& ident) const;

  private:
  unordered_map<string, Type> vars;
  unordered_map<string, Type> types;
  optional<Type> returnType;
};
