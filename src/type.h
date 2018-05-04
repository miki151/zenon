#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "function_call_type.h"
#include "context.h"
#include "function_name.h"

class Context;
struct TypeMapping;
struct FunctionType;
struct SwitchStatement;

struct Type : public owned_object<Type> {
  virtual string getName(bool withTemplateArguments = true) const = 0;
  virtual SType getUnderlying();
  virtual bool canAssign(SType from) const;
  virtual bool canMap(TypeMapping&, SType argType) const;
  virtual unique_ptr<Expression> getConversionFrom(unique_ptr<Expression>, const Context& callContext) const;
  virtual SType replace(SType from, SType to) const;
  virtual ~Type() {}
  virtual WithError<SType> instantiate(const Context&, vector<SType> templateArgs) const;
  virtual const Context& getContext() const;
  virtual const Context& getStaticContext() const;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, bool isReference) const;
  bool canConstructWith(vector<SType> args) const;
  Context context;
  Context staticContext;
};

struct ArithmeticType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  using DefType = shared_ptr<ArithmeticType>;
  static DefType INT;
  static DefType BOOL;
  static DefType VOID;
  static DefType CHAR;
  static DefType STRING;

  ArithmeticType(const string& name);

  private:
  string name;
};

struct ReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual SType getUnderlying() override;
  virtual bool canAssign(SType from) const override;
  virtual bool canMap(TypeMapping& mapping, SType from) const override;
  virtual SType replace(SType from, SType to) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, bool isReference) const override;

  static shared_ptr<ReferenceType> get(SType);
  SType underlying;
  ReferenceType(SType);
};

struct PointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual SType replace(SType from, SType to) const override;
  virtual bool canMap(TypeMapping&, SType argType) const override;

  static shared_ptr<PointerType> get(SType);

  SType underlying;
  PointerType(SType);
};

struct TemplateParameterType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;

  TemplateParameterType(string name, CodeLoc);
  string name;
  CodeLoc declarationLoc;
};

struct StructType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual SType replace(SType from, SType to) const override;
  virtual WithError<SType> instantiate(const Context&, vector<SType> templateArgs) const override;
  virtual bool canMap(TypeMapping&, SType argType) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, bool isReference) const override;
  virtual unique_ptr<Expression> getConversionFrom(unique_ptr<Expression>, const Context& callContext) const override;

  enum Kind {
    STRUCT,
    VARIANT
  };
  static shared_ptr<StructType> get(Kind, string name);
  Kind kind;
  string name;
  shared_ptr<StructType> getInstance(vector<SType> templateParams);
  vector<SType> templateParams;
  vector<shared_ptr<StructType>> instantations;
  nullable<shared_ptr<StructType>> parent;
  vector<SConcept> requirements;
  struct Alternative {
    string name;
    SType type;
  };
  vector<Alternative> alternatives;
  void updateInstantations();
  SType getInstantiated(vector<SType> templateParams);
  StructType() {}
};

struct EnumType : public Type {
  EnumType(string name, vector<string> elements);

  virtual string getName(bool withTemplateArguments = true) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, bool isReference) const override;

  string name;
  vector<string> elements;
};

struct FunctionType {
  struct Param {
    Param(string name, SType type);
    Param(SType type);
    optional<string> name;
    SType type;
  };
  FunctionType(FunctionName name, FunctionCallType, SType returnType, vector<Param> params, vector<SType> templateParams);
  FunctionName name;
  FunctionCallType callType;
  SType retVal;
  vector<Param> params;
  vector<SType> templateParams;
  vector<SConcept> requirements;
  nullable<SType> parentType;
  string toString() const;
};

struct Concept : public owned_object<Concept> {
  Concept(const string& name);
  vector<SType> params;
  Context context;
  string getName() const;
  SConcept translate(vector<SType> params) const;
  SConcept replace(SType from, SType to) const;

  private:
  string name;
};

struct Expression;
class Context;

struct IdentifierInfo;
extern WithErrorLine<FunctionType> instantiateFunction(const FunctionType&, CodeLoc, vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc);
extern bool canConvert(SType from, SType to);
extern void replaceInFunction(FunctionType&, SType from, SType to);
extern string joinTemplateParams(const vector<SType>& params);
