#pragma once

#include "operator.h"

class Context;
struct Expression;

class ReturnTypeChecker {
  public:
  ReturnTypeChecker(nullable<SType> explicitReturn);
  JustError<string> addReturnStatement(const Context&, SType, unique_ptr<Expression>& expr);
  SType getReturnType() const;

  private:
  nullable<SType> explicitReturn;
  nullable<SType> returnStatement;
};
