#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "function_call_type.h"

struct Type;
class State;

struct ArithmeticType {
  static SType INT;
  static SType BOOL;
  static SType VOID;
  static SType STRING;
  static SType CHAR;
};

struct ReferenceType {
  static SType get(SType);
  SType underlying;

  private:
  ReferenceType(SType);
};

struct PointerType {
  static SType get(SType);
  SType underlying;

  private:
  PointerType(SType);
};

struct TemplateParameter {
  TemplateParameter(string name);
  string name;
};

struct FunctionType;

struct StructType {
  enum Kind {
    STRUCT,
    VARIANT
  };
  static SType get(Kind, string name);
  Kind kind;
  string name;
  struct Member {
    string name;
    SType type;
  };
  nullable<SType> getMember(const string&) const;
  vector<Member> members;
  struct Method {
    variant<string, Operator> nameOrOp;
    HeapAllocated<FunctionType> type;
  };
  vector<Method> methods;
  vector<pair<string, FunctionType>> staticMethods;
  vector<SType> templateParams;
  vector<SType> instantations;
  nullable<SType> parent;
  State getContext() const;
  SType instantiate(SType self, vector<SType> templateParams);
  void updateInstantations();
  SType getInstantiated(vector<SType> templateParams);

  private:
  StructType() {}
};

struct EnumType {
  EnumType(string name, vector<string> elements);
  string name;
  vector<string> elements;
};

struct Type : variant<ArithmeticType, ReferenceType, PointerType, StructType, TemplateParameter, EnumType> {
  using variant::variant;
  Type(const Type&) = delete;
};

struct FunctionType {
  struct Param {
    string name;
    SType type;
  };
  FunctionType(FunctionCallType, SType returnType, vector<Param> params, vector<SType> templateParams);
  FunctionCallType callType;
  SType retVal;
  vector<Param> params;
  vector<SType> templateParams;
  nullable<SType> parentType;
};

struct Expression;
class State;

extern string getName(SType);
extern bool canAssign(SType to, SType from);
extern bool canBind(SType to, SType from);
extern bool canConvert(SType from, SType to);
extern bool requiresInitialization(SType);
class IdentifierInfo;
extern nullable<SType> instantiate(SType, vector<SType> templateParams);
extern void instantiateFunction(FunctionType&, CodeLoc, vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc);
extern optional<FunctionType> getStaticMethod(const Type&, string name);
extern SType getUnderlying(SType);
