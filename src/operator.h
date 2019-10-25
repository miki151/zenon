#pragma once

#include "stdafx.h"
#include "code_loc.h"

struct Type;
using SType = shared_ptr<Type>;

struct FunctionInfo;
using SFunctionInfo = shared_ptr<FunctionInfo>;

struct CompileTimeValue;
using SCompileTimeValue = shared_ptr<CompileTimeValue>;

enum class Operator {
  LESS_THAN,
  MORE_THAN,
  EQUALS,
  NOT_EQUAL,
  LESS_OR_EQUAL,
  MORE_OR_EQUAL,
  PLUS,
  PLUS_UNARY,
  MINUS,
  MINUS_UNARY,
  INCREMENT,
  INCREMENT_BY,
  DECREMENT,
  DECREMENT_BY,
  ASSIGNMENT,
  MEMBER_ACCESS,
  POINTER_MEMBER_ACCESS,
  MULTIPLY,
  MULTIPLY_BY,
  DIVIDE,
  DIVIDE_BY,
  MODULO,
  POINTER_DEREFERENCE,
  GET_ADDRESS,
  SUBSCRIPT,
  LOGICAL_NOT,
  LOGICAL_AND,
  LOGICAL_OR,
  MAYBE,
  VALUE_OR
};

extern optional<Operator> getOperator(const string&);
extern const char* getString(Operator);
extern vector<string> getAllOperators();


extern int getPrecedence(Operator);
extern optional<Operator> getUnary(Operator);
extern bool canOverload(Operator);
extern bool canOverload(Operator, int numArgs);
extern bool isUnary(Operator);
extern bool isRightAssociative(Operator);
extern nullable<SType> eval(Operator, vector<SType> args);
extern string getPrettyString(Operator, vector<SType> args);
extern const char* getCodegenName(Operator);
