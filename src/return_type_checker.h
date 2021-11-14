#pragma once

#include "operator.h"

class Context;
struct Expression;

class ReturnTypeChecker {
  public:
  ReturnTypeChecker(Type* explicitReturn);
  JustError<string> addReturnStatement(const Context&, Type*, unique_ptr<Expression>& expr);
  Type* getReturnType() const;

  private:
  Type* explicitReturn = nullptr;
  Type* returnStatement = nullptr;
};
