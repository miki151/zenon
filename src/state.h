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
  void setType(const IdentifierInfo& ident, Type);
  const optional<Type>& getReturnType() const;
  void setReturnType(Type);
  void addType(const string& name, Type);
  optional<Type> getTypeFromString(const string& ident) const;

  private:
  unordered_map<IdentifierInfo, Type, CustomHash<IdentifierInfo>> vars;
  unordered_map<IdentifierInfo, Type, CustomHash<IdentifierInfo>> types;
  optional<Type> returnType;
};
