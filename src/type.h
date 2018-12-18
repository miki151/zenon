#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "context.h"
#include "function_name.h"

class Context;
struct TypeMapping;
struct FunctionType;
struct SwitchStatement;

struct Type : public owned_object<Type> {
  virtual string getName(bool withTemplateArguments = true) const = 0;
  virtual string getCodegenName() const;
  virtual SType getUnderlying() const;
  virtual bool canAssign(SType from) const;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const;
  SType replace(SType from, SType to) const;
  virtual bool canReplaceBy(SType) const;
  virtual SType replaceImpl(SType from, SType to) const;
  virtual SType getType() const;
  virtual ~Type() {}
  virtual WithError<SType> instantiate(const Context&, vector<SType> templateArgs) const;
  virtual Context& getStaticContext();
  enum class SwitchArgument {
    VALUE,
    REFERENCE,
    MUTABLE_REFERENCE
  };
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, SwitchArgument) const;
  virtual bool isBuiltinCopyable(const Context&) const;
  virtual SType removePointer() const;
  virtual optional<string> getSizeError() const;
  Context staticContext;
};

struct ArithmeticType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual bool isBuiltinCopyable(const Context&) const override;
  using DefType = shared_ptr<ArithmeticType>;
  static DefType INT;
  static DefType DOUBLE;
  static DefType BOOL;
  static DefType VOID;
  static DefType CHAR;
  static DefType STRING;
  static DefType ANY_TYPE;
  static DefType ENUM_TYPE;
  ArithmeticType(const string& name, optional<string> codegenName = none);

  private:
  string name;
  string codegenName;
};

struct ReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual SType getUnderlying() const override;
  virtual bool canAssign(SType from) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping& mapping, SType from) const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, SwitchArgument) const override;
  virtual SType removePointer() const override;

  static shared_ptr<ReferenceType> get(SType);
  SType underlying;
  ReferenceType(SType);
};

struct MutableReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual SType getUnderlying() const override;
  virtual bool canAssign(SType from) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping& mapping, SType from) const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, SwitchArgument) const override;
  virtual SType removePointer() const override;

  static shared_ptr<MutableReferenceType> get(SType);
  MutableReferenceType(SType);
  SType underlying;
};

struct PointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  virtual bool isBuiltinCopyable(const Context&) const override;
  virtual SType removePointer() const override;

  static shared_ptr<PointerType> get(SType);
  SType underlying;
  PointerType(SType);
};

struct MutablePointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  virtual bool isBuiltinCopyable(const Context&) const override;
  virtual SType removePointer() const override;

  static shared_ptr<MutablePointerType> get(SType);
  MutablePointerType(SType);
  SType underlying;
};

struct EnumType;

struct CompileTimeValue : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  virtual bool canReplaceBy(SType) const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual SType getType() const override;
  struct TemplateValue {
    SType type;
    string name;
    auto asTuple() const {
      return std::forward_as_tuple(name, type);
    }
    bool operator == (const TemplateValue& o) const {
      return asTuple() == o.asTuple();
    }
    bool operator < (const TemplateValue& o) const {
      return asTuple() < o.asTuple();
    }
  };
  struct EnumValue {
    shared_ptr<EnumType> type;
    int index;
    auto asTuple() const {
      return std::forward_as_tuple(type, index);
    }
    bool operator == (const EnumValue& o) const {
      return asTuple() == o.asTuple();
    }
    bool operator < (const EnumValue& o) const {
      return asTuple() < o.asTuple();
    }
  };
  struct ArrayValue {
    vector<SCompileTimeValue> values;
    SType type;
    auto asTuple() const {
      return std::forward_as_tuple(type, values);
    }
    bool operator == (const ArrayValue& o) const {
      return asTuple() == o.asTuple();
    }
    bool operator < (const ArrayValue& o) const {
      return asTuple() < o.asTuple();
    }
  };

  using Value = variant<int, bool, double, char, string, EnumValue, ArrayValue, TemplateValue>;
  static shared_ptr<CompileTimeValue> get(Value);
  static shared_ptr<CompileTimeValue> getTemplateValue(SType type, string name);
  Value value;

  CompileTimeValue(Value);
};

struct TemplateParameterType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual bool canReplaceBy(SType) const override;
  TemplateParameterType(string name, CodeLoc);
  string name;
  CodeLoc declarationLoc;
};

struct StructType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual WithError<SType> instantiate(const Context&, vector<SType> templateArgs) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, SwitchArgument) const override;
  virtual optional<string> getSizeError() const override;
  WithError<SType> getTypeOfMember(const string&) const;
  static shared_ptr<StructType> get(string name);
  string name;
  shared_ptr<StructType> getInstance(vector<SType> templateParams);
  vector<SType> templateParams;
  vector<shared_ptr<StructType>> instantations;
  nullable<shared_ptr<StructType>> parent;
  vector<SConcept> requirements;
  struct Variable {
    string name;
    SType type;
  };
  vector<Variable> members;
  vector<Variable> alternatives;
  bool incomplete = false;
  void updateInstantations();
  SType getInstantiated(vector<SType> templateParams);
  StructType() {}
};

struct EnumType : public Type {
  EnumType(string name, vector<string> elements);

  virtual string getName(bool withTemplateArguments = true) const override;
  virtual void handleSwitchStatement(SwitchStatement&, Context&, CodeLoc, SwitchArgument) const override;
  virtual bool isBuiltinCopyable(const Context&) const override;
  virtual SType getType() const override;

  string name;
  vector<string> elements;
};

struct ArrayType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  static shared_ptr<ArrayType> get(SType, SCompileTimeValue size);
  virtual bool isBuiltinCopyable(const Context&) const override;
  virtual optional<string> getSizeError() const override;
  ArrayType(SType, SCompileTimeValue size);
  SCompileTimeValue size;
  SType underlying;
};

struct SliceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(SType from, SType to) const override;
  virtual optional<string> getMappingError(const Context&, TypeMapping&, SType argType) const override;
  static shared_ptr<SliceType> get(SType);
  virtual bool isBuiltinCopyable(const Context&) const override;
  virtual optional<string> getSizeError() const override;
  struct Private {};
  SliceType(Private, SType);
  SType underlying;
};

struct FunctionType {
  struct Param {
    Param(optional<string> name, SType type);
    Param(string name, SType type);
    Param(SType type);
    optional<string> name;
    SType type;
  };
  FunctionType(SType returnType, vector<Param> params, vector<SType> templateParams);
  SType retVal;
  vector<Param> params;
  vector<SType> templateParams;
  vector<SConcept> requirements;
  nullable<SType> parentType;
  bool externalMethod = false;
  bool fromConcept = false;
  bool subscriptOpWorkaround = true;
};

struct FunctionInfo {
  FunctionId id;
  FunctionType type;
  string prettyString() const;
};

struct Concept : public owned_object<Concept> {
  Concept(const string& name);
  string getName() const;
  SConcept translate(vector<SType> params) const;
  SConcept replace(SType from, SType to) const;
  const vector<SType>& getParams() const;
  const Context& getContext() const;
  vector<SType>& modParams();
  Context& modContext();

  private:
  vector<SType> params;
  Context context;
  string name;
};

struct Expression;
class Context;

struct IdentifierInfo;
extern WithErrorLine<FunctionType> instantiateFunction(const Context& context, const FunctionType&, CodeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc, vector<FunctionType> existing = {});
extern void replaceInFunction(FunctionType&, SType from, SType to);
extern string joinTemplateParams(const vector<SType>&);
extern string joinTypeList(const vector<SType>&);
extern string joinTemplateParamsCodegen(const vector<SType>&);
extern string joinTypeListCodegen(const vector<SType>&);
