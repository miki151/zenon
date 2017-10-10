#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "type.h"

class State {
  public:

  optional<Type> getType(const string& ident) const;
  void setType(const string& ident, Type);
  const optional<Type>& getReturnType() const;
  void setReturnType(Type);

  private:
  using Variables = unordered_map<string, Type>;
  Variables vars;
  optional<Type> returnType;
};
