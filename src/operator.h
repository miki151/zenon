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
  MULTIPLY
};

extern optional<Operator> getOperator(const string&);
extern const char* getString(Operator);
extern vector<string> getAllOperators();


extern int getPrecedence(Operator);
extern bool isUnary(Operator);
extern bool isRightAssociative(Operator);
