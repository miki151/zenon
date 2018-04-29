#pragma once

#include "stdafx.h"

enum class Operator {
  LESS_THAN,
  MORE_THAN,
  EQUALS,
  PLUS,
  PLUS_UNARY,
  MINUS,
  MINUS_UNARY,
  ASSIGNMENT,
  MEMBER_ACCESS,
  MULTIPLY,
  POINTER_DEREFERENCE,
  GET_ADDRESS,
  SUBSCRIPT,
  LOGICAL_NOT,
  LOGICAL_AND,
  LOGICAL_OR
};

extern optional<Operator> getOperator(const string&);
extern const char* getString(Operator);
extern vector<string> getAllOperators();


extern int getPrecedence(Operator);
extern optional<Operator> getUnary(Operator);
extern bool canOverload(Operator, int numArguments);
extern bool isRightAssociative(Operator);
struct Type;
using SType = shared_ptr<Type>;
class CodeLoc;
class Context;
struct Expression;
extern SType getOperationResult(CodeLoc, Operator, const Context&, Expression& left, Expression& right);
extern SType getUnaryOperationResult(CodeLoc, Operator, SType);
