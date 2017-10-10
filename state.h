#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"

enum class ArithmeticType {
  INT,
  BOOL,
  VOID
};

struct FunctionType {
  ArithmeticType retVal;
  vector<ArithmeticType> params;
};

using Type = variant<ArithmeticType, FunctionType>;

extern optional<ArithmeticType> getType(const string&);
extern const char* getName(ArithmeticType);

class State {
  public:

  optional<Type> getType(const string& ident) const;
  void setType(const string& ident, Type t);
  const optional<ArithmeticType>& getReturnType() const;
  void setReturnType(ArithmeticType);

  private:
  using Variables = unordered_map<string, Type>;
  Variables vars;
  optional<ArithmeticType> returnType;
};
