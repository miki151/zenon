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

struct TemplateParameter {
  TemplateParameter(string name);
  string name;
  int id;
  bool operator == (const TemplateParameter&) const;
};

struct FunctionType;

struct VariantType {
  VariantType(string name);
  string name;
  int id;
  map<string, Type> types;
  vector<Type> templateParams;
  vector<pair<string, FunctionType>> staticMethods;
  bool operator == (const VariantType&) const;
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
  vector<Type> templateParams;
  bool operator == (const StructType&) const;
};

struct Type : variant<ArithmeticType, ReferenceType, StructType, MemberAccess, VariantType, TemplateParameter> {
  using variant::variant;
};

struct FunctionType {
  struct Param {
    string name;
    HeapAllocated<Type> type;
  };
  FunctionType(FunctionCallType, Type returnType, vector<Param>, vector<Type>);
  FunctionCallType callType;
  HeapAllocated<Type> retVal;
  vector<Param> params;
  vector<Type> templateParams;
  int id;
  bool operator == (const FunctionType&) const;
};

extern string getName(const Type&);
extern bool canAssign(const Type& to, const Type& from);
extern bool canBind(const Type& to, const Type& from);
extern Type getOperationResult(CodeLoc, Operator op, const Type& from, const Type& to);
extern bool canConvert(const Type& from, const Type& to);
extern bool requiresInitialization(const Type&);
class IdentifierInfo;
extern optional<Type> instantiate(const Type&, vector<Type> templateParams);
extern void instantiate(FunctionType&, CodeLoc, vector<Type> templateArgs, vector<Type> argTypes, vector<CodeLoc> argLoc);
extern optional<FunctionType> getStaticMethod(const Type&, string name);
extern Type getUnderlying(Type);
