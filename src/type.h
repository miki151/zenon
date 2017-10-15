#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "function_call_type.h"

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
  FunctionType(FunctionCallType, Type returnType, vector<Param>);
  FunctionCallType callType;
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

struct VariantType {
  VariantType(string name);
  string name;
  int id;
  map<string, Type> types;
  bool operator == (const VariantType&) const;
};

struct Type : variant<ArithmeticType, FunctionType, ReferenceType, StructType, MemberAccess, VariantType> {
  using variant::variant;
};

extern string getName(const Type&);
extern bool canAssign(const Type& to, const Type& from);
extern Type getOperationResult(CodeLoc, Operator op, const Type& from, const Type& to);
extern bool canConvert(const Type& from, const Type& to);
extern bool requiresInitialization(const Type&);
