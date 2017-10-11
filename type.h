#pragma once

#include "stdafx.h"
#include "util.h"
#include "binary_operator.h"

struct Type;

enum class ArithmeticType {
  INT,
  BOOL,
  VOID
};

extern optional<ArithmeticType> getArithmeticType(const string&);

struct FunctionType {
  FunctionType(Type returnType, vector<Type> params);
  HeapAllocated<Type> retVal;
  vector<Type> params;
  bool operator == (const FunctionType&) const;
};

struct ReferenceType {
  ReferenceType(Type);
  HeapAllocated<Type> underlying;
  bool operator == (const ReferenceType&) const;
};

struct Type : variant<ArithmeticType, FunctionType, ReferenceType> {
  using variant::variant;
};

extern string getName(const Type&);
extern bool canAssign(const Type& to, const Type& from);
extern optional<Type> getOperationResult(BinaryOperator op, const Type& from, const Type& to);
