#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "function_call_type.h"
#include "state.h"

class State;
struct TypeMapping;
struct FunctionType;
struct SwitchStatement;

struct Type : public owned_object<Type> {
  virtual string getName() const = 0;
  virtual SType getUnderlying();
  virtual bool canAssign(SType from) const;
  virtual bool canMap(TypeMapping&, SType argType) const;
  virtual SType replace(SType from, SType to) const;
  virtual ~Type() {}
  virtual nullable<SType> instantiate(vector<SType> templateParams) const;
  virtual optional<State> getTypeContext() const;
  virtual optional<FunctionType> getStaticMethod(const string&) const;
  virtual void handleSwitchStatement(SwitchStatement&, State&, CodeLoc) const;
  State state;
  State staticState;
};

struct ArithmeticType : public Type {
  virtual string getName() const override;
  using DefType = SType;
  static DefType INT;
  static DefType BOOL;
  static DefType VOID;
  static DefType STRING;
  static DefType CHAR;

  ArithmeticType(const char* name);
  virtual optional<State> getTypeContext() const override;

  private:
  const char* name;
};

struct ReferenceType : public Type {
  virtual string getName() const override;
  virtual SType getUnderlying() override;
  virtual bool canAssign(SType from) const override;
  virtual bool canMap(TypeMapping& mapping, SType from) const override;
  virtual SType replace(SType from, SType to) const override;
  virtual optional<State> getTypeContext() const override;
  virtual void handleSwitchStatement(SwitchStatement&, State&, CodeLoc) const override;

  static shared_ptr<ReferenceType> get(SType);
  SType underlying;
  ReferenceType(SType);
};

struct PointerType : public Type {
  virtual string getName() const override;
  virtual SType replace(SType from, SType to) const override;
  virtual bool canMap(TypeMapping&, SType argType) const override;

  static shared_ptr<PointerType> get(SType);

  SType underlying;
  PointerType(SType);
};

struct TemplateParameterType : public Type {
  virtual string getName() const override;
  virtual SType replace(SType from, SType to) const override;

  TemplateParameterType(string name, CodeLoc);
  string name;
  CodeLoc declarationLoc;
};

struct StructType : public Type {
  virtual string getName() const override;
  virtual SType replace(SType from, SType to) const override;
  virtual nullable<SType> instantiate(vector<SType> templateParams) const override;
  virtual bool canMap(TypeMapping&, SType argType) const override;
  virtual optional<FunctionType> getStaticMethod(const string&) const override;
  virtual optional<State> getTypeContext() const override;
  virtual void handleSwitchStatement(SwitchStatement&, State&, CodeLoc) const override;

  enum Kind {
    STRUCT,
    VARIANT
  };
  static shared_ptr<StructType> get(Kind, string name);
  Kind kind;
  string name;
  struct Method {
    variant<string, Operator> nameOrOp;
    HeapAllocated<FunctionType> type;
  };
  vector<Method> methods;
  vector<pair<string, FunctionType>> staticMethods;
  shared_ptr<StructType> getInstance(vector<SType> templateParams);
  vector<SType> templateParams;
  vector<shared_ptr<StructType>> instantations;
  nullable<shared_ptr<StructType>> parent;
  void updateInstantations();
  SType getInstantiated(vector<SType> templateParams);
  StructType() {}
};

struct EnumType : public Type {
  EnumType(string name, vector<string> elements);

  virtual string getName() const override;
  virtual void handleSwitchStatement(SwitchStatement&, State&, CodeLoc) const override;

  string name;
  vector<string> elements;
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

extern bool requiresInitialization(SType);
class IdentifierInfo;
extern void instantiateFunction(FunctionType&, CodeLoc, vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc);
extern bool canConvert(SType from, SType to);
