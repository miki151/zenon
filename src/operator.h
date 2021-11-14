#pragma once

#include "stdafx.h"
#include "code_loc.h"

struct Type;

struct FunctionInfo;
using SFunctionInfo = shared_ptr<FunctionInfo>;

struct CompileTimeValue;
using SCompileTimeValue = CompileTimeValue*;

enum class Operator {
  LESS_THAN,
  EQUALS,
  NOT_EQUAL,
  PLUS,
  PLUS_UNARY,
  MINUS,
  MINUS_UNARY,
  INCREMENT,
  INCREMENT_BY,
  DECREMENT,
  DECREMENT_BY,
  ASSIGNMENT,
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
extern WithEvalError<Type*> eval(Operator, vector<Type*> args);
extern string getPrettyString(Operator, vector<Type*> args);
extern const char* getCodegenName(Operator);
