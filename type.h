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

struct StructType {
  StructType(string name);
  string name;
  int id;
  struct Member {
    string name;
    HeapAllocated<Type> type;
  };
  vector<Member> members;
  bool operator == (const StructType&) const;
};

struct Type : variant<ArithmeticType, FunctionType, ReferenceType, StructType> {
  using variant::variant;
};

extern string getName(const Type&);
extern bool canAssign(const Type& to, const Type& from);
extern optional<Type> getOperationResult(BinaryOperator op, const Type& from, const Type& to);
