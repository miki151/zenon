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
class CodeLoc;
class State;
class Expression;
extern Type getOperationResult(CodeLoc, Operator, const State&, Expression& left, Expression& right);
extern Type getUnaryOperationResult(CodeLoc, Operator, const Type&);
