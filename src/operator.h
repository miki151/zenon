#pragma once

#include "stdafx.h"

enum class Operator {
  LESS_THAN,
  MORE_THAN,
  EQUALS,
  PLUS,
  MINUS,
  ASSIGNMENT,
  MEMBER_ACCESS,
  MULTIPLY,
  GET_ADDRESS,
  SUBSCRIPT
};

extern optional<Operator> getOperator(const string&);
extern const char* getString(Operator);
extern vector<string> getAllOperators();


extern int getPrecedence(Operator);
extern bool isUnary(Operator);
extern bool isRightAssociative(Operator);
struct Type;
using SType = shared_ptr<Type>;
class CodeLoc;
class Context;
struct Expression;
extern SType getOperationResult(CodeLoc, Operator, const Context&, Expression& left, Expression& right);
extern SType getUnaryOperationResult(CodeLoc, Operator, SType);
