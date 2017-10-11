#pragma once

#include "stdafx.h"

enum class BinaryOperator {
  LESS_THAN,
  MORE_THAN,
  EQUALS,
  PLUS,
  MINUS,
  ASSIGNMENT,
};

extern optional<BinaryOperator> getBinaryOperator(const string&);
extern const char* getString(BinaryOperator);
extern vector<string> getAllOperators();


extern int getPrecedence(BinaryOperator);

extern bool isRightAssociative(BinaryOperator);
