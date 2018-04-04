#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "function_call_type.h"

struct Type;
class State;

enum class ArithmeticType {
  INT,
  BOOL,
  VOID,
  STRING,
  CHAR,
};

struct ReferenceType {
  ReferenceType(Type);
  HeapAllocated<Type> underlying;
  bool operator == (const ReferenceType&) const;
};

struct PointerType {
  PointerType(Type);
  HeapAllocated<Type> underlying;
  bool operator == (const PointerType&) const;
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
  struct Method {
    variant<string, Operator> nameOrOp;
    HeapAllocated<FunctionType> type;
  };
  vector<Method> methods;
  vector<Type> templateParams;
  vector<pair<string, FunctionType>> staticMethods;
  bool operator == (const VariantType&) const;
  State getContext() const;
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
  struct Method {
    variant<string, Operator> nameOrOp;
    HeapAllocated<FunctionType> type;
  };
  vector<Method> methods;
  vector<Type> templateParams;
  bool operator == (const StructType&) const;
  State getContext() const;
};

struct EnumType {
  EnumType(string name, vector<string> elements);
  string name;
  int id;
  vector<string> elements;
  bool operator == (const EnumType&) const;
};

struct Type : variant<ArithmeticType, ReferenceType, PointerType, StructType, VariantType, TemplateParameter, EnumType> {
  using variant::variant;
};

struct FunctionType {
  struct Param {
    string name;
    HeapAllocated<Type> type;
  };
  FunctionType(FunctionCallType, Type returnType, vector<Param> params, vector<Type> templateParams);
  FunctionCallType callType;
  HeapAllocated<Type> retVal;
  vector<Param> params;
  vector<Type> templateParams;
  optional<Type> parentType;
  int id;
  bool operator == (const FunctionType&) const;
};

class Expression;
class State;

extern string getName(const Type&);
extern const char* getName(ArithmeticType);
extern bool canAssign(const Type& to, const Type& from);
extern bool canBind(const Type& to, const Type& from);
extern bool canConvert(const Type& from, const Type& to);
extern bool requiresInitialization(const Type&);
class IdentifierInfo;
extern optional<Type> instantiate(const Type&, vector<Type> templateParams);
extern void instantiate(FunctionType&, CodeLoc, vector<Type> templateArgs, vector<Type> argTypes, vector<CodeLoc> argLoc);
extern optional<FunctionType> getStaticMethod(const Type&, string name);
extern Type getUnderlying(Type);
