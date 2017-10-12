#pragma once

#include "stdafx.h"
#include "util.h"
#include "binary_operator.h"
#include "code_loc.h"

struct Type;

enum class ArithmeticType {
  INT,
  BOOL,
  VOID
};

struct FunctionType {
  struct Param {
    string name;
    HeapAllocated<Type> type;
  };
  enum Target {
    TOP_LEVEL,
    CONSTRUCTOR
  };
  FunctionType(Target, Type returnType, vector<Param>);
  Target target;
  HeapAllocated<Type> retVal;
  vector<Param> params;
  int id;
  bool operator == (const FunctionType&) const;
};

struct ReferenceType {
  ReferenceType(Type);
  HeapAllocated<Type> underlying;
  bool operator == (const ReferenceType&) const;
};

struct MemberAccess {
  MemberAccess(const string& memberName);
  string memberName;
  int id;
  bool operator == (const MemberAccess&) const;
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

struct Type : variant<ArithmeticType, FunctionType, ReferenceType, StructType, MemberAccess> {
  using variant::variant;
};

extern string getName(const Type&);
extern bool canAssign(const Type& to, const Type& from);
extern Type getOperationResult(CodeLoc, BinaryOperator op, const Type& from, const Type& to);
extern bool canConvert(const Type& from, const Type& to);
extern bool requiresInitialization(const Type&);
