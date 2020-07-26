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
struct FunctionDefinition;
struct Accu;
struct StatementBlock;
struct Statement;
struct AttributeType;

struct Type : public owned_object<Type> {
  Type();
  virtual string getName(bool withTemplateArguments = true) const = 0;
  virtual string getCodegenName() const;
  virtual optional<string> getMangledName() const;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const;
  SType replace(const Context&, SType from, SType to, ErrorBuffer&) const;
  virtual bool canReplaceBy(SType) const;
  virtual SType transform(function<SType(const Type*)>) const;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const;
  virtual SType expand(SType pack, vector<SType> to, ErrorBuffer& errors) const;
  virtual SType getType() const;
  struct MemberInfo {
    SType type;
    string name;
  };
  virtual WithError<MemberInfo> getTypeOfMember(const SType&) const;
  virtual WithError<SType> getTypeOfMember(const string& name) const;
  virtual bool hasDestructor() const;
  void codegenDefinition(set<const Type*>& visited, Accu&) const;
  virtual ~Type() {}
  virtual WithErrorLine<SType> instantiate(const Context&, vector<SType> templateArgs, CodeLoc) const;
  Context& getStaticContext();
  enum class SwitchArgument {
    VALUE,
    REFERENCE,
    MUTABLE_REFERENCE
  };
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, SwitchArgument) const;
  virtual SType removePointer() const;
  virtual SType removeReference() const;
  bool isPointer() const;
  bool isReference() const;
  virtual JustError<string> getSizeError(const Context&) const;
  virtual bool canBeValueTemplateParam() const;
  virtual bool canDeclareVariable() const;
  virtual bool isMetaType() const;
  bool isBuiltinCopyable(const Context&, unique_ptr<Expression>&) const;
  bool isBuiltinCopyable(const Context&) const;
  Context staticContext;

  protected:
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const;
  virtual void codegenDefinitionImpl(set<const Type*>& visited, Accu&) const;
};

struct BuiltinType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual bool canBeValueTemplateParam() const override;
  virtual bool canDeclareVariable() const override;
  virtual bool isMetaType() const override;
  using DefType = shared_ptr<BuiltinType>;
  static DefType INT;
  static DefType DOUBLE;
  static DefType BOOL;
  static DefType VOID;
  static DefType CHAR;
  static DefType STRING;
  static DefType NORETURN;
  static DefType NULL_TYPE;
  static DefType ANY_TYPE;
  static DefType ENUM_TYPE;
  static DefType STRUCT_TYPE;
  static DefType UNION_TYPE;
  static DefType ATTRIBUTE_TYPE;
  BuiltinType(const string& name, optional<string> codegenName = none);

  private:
  string name;
  string codegenName;
};

struct AttributeType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual bool canBeValueTemplateParam() const override;
  virtual bool canDeclareVariable() const override;
  virtual SType getType() const override;
  static shared_ptr<AttributeType> get(const string&);
  struct Private {};
  AttributeType(Private, const string& name);

  private:
  string name;
};

struct ReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual SType removeReference() const override;
  virtual JustError<string> getMappingError(TypeMapping& mapping, SType from) const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  virtual WithError<MemberInfo> getTypeOfMember(const SType&) const override;
  virtual WithError<SType> getTypeOfMember(const string& name) const override;
  virtual SType removePointer() const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, SwitchArgument) const override;

  static shared_ptr<ReferenceType> get(SType);
  SType underlying;
  ReferenceType(SType);
};

struct MutableReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual SType removeReference() const override;
  virtual JustError<string> getMappingError(TypeMapping& mapping, SType from) const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  virtual WithError<MemberInfo> getTypeOfMember(const SType&) const override;
  virtual WithError<SType> getTypeOfMember(const string& name) const override;
  virtual SType removePointer() const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, SwitchArgument) const override;

  static shared_ptr<MutableReferenceType> get(SType);
  const SType underlying;
  struct Private {};
  MutableReferenceType(Private, SType);
};

struct PointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual SType removePointer() const override;

  static shared_ptr<PointerType> get(SType);
  const SType underlying;
  struct Private {};
  PointerType(Private, SType);
};

struct MutablePointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual SType removePointer() const override;

  static shared_ptr<MutablePointerType> get(SType);
  const SType underlying;
  struct Private {};
  MutablePointerType(Private, SType);
};

struct OptionalType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual JustError<string> getMappingError(TypeMapping& mapping, SType from) const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  void codegenDefinitionImpl(set<const Type*>& visited, Accu& accu) const override;
  virtual bool hasDestructor() const override;

  static shared_ptr<OptionalType> get(SType);
  const SType underlying;
  struct Private {};
  OptionalType(Private, SType);
};

struct EnumType;

struct CompileTimeValue : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual bool canReplaceBy(SType) const override;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const override;
  virtual SType expand(SType pack, vector<SType> to, ErrorBuffer&) const override;
  virtual SType getType() const override;
  virtual optional<string> getMangledName() const override;
  struct TemplateValue {
    SType type;
    string name;
    COMPARABLE(TemplateValue, type, name)
  };
  struct TemplateExpression {
    Operator op;
    vector<SType> args;
    SType type;
    COMPARABLE(TemplateExpression, op, args, type)
  };
  struct TemplateFunctionCall {
    string name;
    vector<SType> args;
    SType retVal;
    CodeLoc loc;
    vector<CodeLoc> argLoc;
    Context::BuiltInFunctionInfo functionInfo;
    COMPARABLE(TemplateFunctionCall, name, args)
  };
  struct EnumValue {
    shared_ptr<EnumType> type;
    int index;
    COMPARABLE(EnumValue, type, index)
  };
  struct ArrayValue {
    vector<SCompileTimeValue> values;
    SType type;
    COMPARABLE(ArrayValue, values, type)
  };
  struct ReferenceValue {
    ReferenceValue(SCompileTimeValue);
    SCompileTimeValue value;
    int id;
    COMPARABLE(ReferenceValue, id)
  };
  struct NullValue { COMPARABLE(NullValue) };
  using Value = variant<int, bool, double, char, NullValue, string, EnumValue, ArrayValue, TemplateValue, TemplateExpression,
      TemplateFunctionCall, ReferenceValue>;
  static shared_ptr<CompileTimeValue> get(Value);
  static shared_ptr<CompileTimeValue> getReference(SCompileTimeValue);
  static shared_ptr<CompileTimeValue> getTemplateValue(SType type, string name);
  Value value;

  struct Private {};
  CompileTimeValue(Private, Value);
};

struct TemplateParameterType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual bool canReplaceBy(SType) const override;
  virtual optional<string> getMangledName() const override;
  virtual SType getType() const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual WithError<MemberInfo> getTypeOfMember(const SType&) const override;
  virtual WithError<SType> getTypeOfMember(const string&) const override;
  virtual bool canBeValueTemplateParam() const override;
  TemplateParameterType(string name, CodeLoc);
  TemplateParameterType(SType type, string name, CodeLoc);
  string name;
  CodeLoc declarationLoc;
  SType type;
};

struct TemplateStructMemberType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  static shared_ptr<TemplateStructMemberType> get(SType structType, SCompileTimeValue);
  SType structType;
  SCompileTimeValue memberIndex;
  struct Private {};
  TemplateStructMemberType(Private, SType structType, SCompileTimeValue);
};

struct TemplateRequirement {
  using Base = variant<SConcept, shared_ptr<Expression>>;
  TemplateRequirement(Base base, bool variadic) : base(std::move(base)), variadic(variadic) {}
  Base base;
  bool variadic;
  COMPARABLE(TemplateRequirement, base, variadic)
};

struct StructType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const override;
  virtual SType expand(SType pack, vector<SType> to, ErrorBuffer&) const override;
  virtual WithErrorLine<SType> instantiate(const Context&, vector<SType> templateArgs, CodeLoc) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, SwitchArgument) const override;
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual void codegenDefinitionImpl(set<const Type*>& visited, Accu&) const override;
  virtual WithError<MemberInfo> getTypeOfMember(const SType&) const override;
  virtual SType getType() const override;
  virtual WithError<SType> getTypeOfMember(const string&) const override;
  virtual bool hasDestructor() const override;
  string name;
  shared_ptr<StructType> getInstance(vector<SType> templateParams);
  vector<SType> templateParams;
  vector<shared_ptr<StructType>> instances;
  nullable<shared_ptr<StructType>> parent;
  nullable<SFunctionInfo> destructor;
  vector<TemplateRequirement> requirements;
  struct Variable {
    string name;
    SType type;
  };
  vector<Variable> members;
  vector<Variable> alternatives;
  bool external = false;
  optional<CodeLoc> definition;
  void updateInstantations(const Context&);
  struct Private {};
  StructType(string name, Private);
};

struct EnumType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, SwitchArgument) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual SType getType() const override;
  virtual void codegenDefinitionImpl(set<const Type*>& visited, Accu&) const override;

  string name;
  vector<string> elements;
  bool external = false;
  optional<CodeLoc> definition;
  struct Private {};
  EnumType(string name, Private);
};

struct ArrayType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const override;
  virtual SType expand(SType pack, vector<SType> to, ErrorBuffer&) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  static shared_ptr<ArrayType> get(SType, SCompileTimeValue size);
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual void codegenDefinitionImpl(set<const Type*>& visited, Accu&) const override;
  virtual bool hasDestructor() const override;
  ArrayType(SType, SCompileTimeValue size);
  SCompileTimeValue size;
  SType underlying;
};

struct SliceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType transform(function<SType(const Type*)>) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  static shared_ptr<SliceType> get(SType);
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  struct Private {};
  SliceType(Private, SType);
  SType underlying;
};

struct VariablePack : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  VariablePack(SType, string);
  string identifier;
  SType packType;
};

struct FunctionType {
  FunctionType(SType returnType, vector<SType> params, vector<SType> templateParams);
  FunctionType setBuiltin();
  SType retVal;
  vector<SType> params;
  vector<SType> templateParams;
  vector<TemplateRequirement> requirements;
  nullable<SType> parentType;
  nullable<SConcept> concept;
  bool builtinOperator = false;
  bool generatedConstructor = false;
  bool variadicTemplate = false;
  bool variadicParams = false;
  COMPARABLE(FunctionType, retVal, params, templateParams, parentType, variadicTemplate, variadicParams, requirements, concept, builtinOperator, generatedConstructor)
};

struct FunctionInfo : public owned_object<FunctionInfo> {
  struct Private {};
  static SFunctionInfo getImplicit(FunctionId, FunctionType);
  static SFunctionInfo getInstance(FunctionId, FunctionType, SFunctionInfo parent);
  static SFunctionInfo getDefined(FunctionId, FunctionType, FunctionDefinition*);
  const FunctionId id;
  const FunctionType type;
  string prettyString() const;
  string getMangledName() const;
  bool isMainFunction() const;
  optional<string> getMangledSuffix() const;
  optional<string> getParamName(int index, const FunctionDefinition*) const;
  SFunctionInfo getWithoutRequirements() const;
  FunctionInfo(Private, FunctionId, FunctionType, nullable<SFunctionInfo> parent);
  FunctionInfo(Private, FunctionId, FunctionType, FunctionDefinition*);
  SFunctionInfo getParent() const;
  FunctionDefinition* getDefinition() const;
  JustError<ErrorLoc> addInstance(const Context&) const;

  private:
  const nullable<SFunctionInfo> parent;
  FunctionDefinition* const definition = nullptr;
};

struct LambdaType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual void codegenDefinitionImpl(set<const Type*>& visited, Accu&) const override;
  virtual bool hasDestructor() const override;
  static shared_ptr<LambdaType> get(string name, vector<SType> templateParams);
  static shared_ptr<LambdaType> get(vector<SType> templateParams);
  struct Private {};
  LambdaType(Private);
  nullable<SFunctionInfo> functionInfo;
  vector<LambdaCapture> captures;
  vector<SType> templateParams;
  vector<optional<string>> parameterNames;
  unique_ptr<Statement> body;
  unique_ptr<StatementBlock> destructor;
  vector<unique_ptr<Statement>> destructorCalls;
  private:
  string name;
};

struct ConceptDefinition;

struct Concept : public owned_object<Concept> {
  Concept(const string& name, ConceptDefinition*, Context emptyContext, bool variadic);
  string getName(bool withTemplateParams = true) const;
  SConcept translate(vector<SType> params, bool variadicParams, ErrorBuffer&) const;
  SConcept replace(const Context&, SType from, SType to, ErrorBuffer&) const;
  SConcept expand(SType from, vector<SType> newParams, ErrorBuffer& errors) const;
  const vector<SType>& getParams() const;
  bool isVariadic() const;
  JustError<ErrorLoc> canCreateConceptType() const;
  const Context& getContext() const;
  vector<SType>& modParams();
  Context& modContext();
  ConceptDefinition* def;

  private:
  vector<SType> params;
  string name;
  Context context;
  bool variadic;
};

struct ConceptType : public Type {
  static shared_ptr<ConceptType> get(SConcept, vector<SType> params, bool variadic);
  struct Private{};
  ConceptType(Private, SConcept, vector<SType> params, bool variadic);
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual SType replaceImpl(const Context&, SType from, SType to, ErrorBuffer&) const override;
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual JustError<string> getMappingError(TypeMapping&, SType argType) const override;
  virtual SType expand(SType pack, vector<SType> to, ErrorBuffer&) const override;
  virtual bool hasDestructor() const override;
  SConcept getConceptFor(const SType&) const;
  SConcept concept;
  vector<SType> params;
  bool variadic;
};

struct Expression;
class Context;

struct IdentifierInfo;
extern WithErrorLine<SFunctionInfo> instantiateFunction(const Context& context, const SFunctionInfo&, CodeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc,
    vector<FunctionType> existing = {});
extern FunctionType replaceInFunction(const Context&, FunctionType, SType from, SType to, ErrorBuffer&);
extern SFunctionInfo replaceInFunction(const Context&, const SFunctionInfo&, SType from, SType to, ErrorBuffer&);
extern SFunctionInfo addTemplateParams(const SFunctionInfo&, vector<SType> params, bool variadic);
extern string joinTemplateParams(const vector<SType>&, bool variadic = false);
extern optional<string> mangleTemplateParams(const vector<SType>&);
extern string joinTypeList(const vector<SType>&);
extern string joinTemplateParamsCodegen(const vector<SType>&);
extern string joinTypeListCodegen(const vector<SType>&);
extern string getExpandedParamName(const string& packName, int index);
void generateConversions(const Context&, const vector<SType>& paramTypes, const vector<SType>& argTypes,
    vector<unique_ptr<Expression>>&);

SType convertPointerToReference(SType);
SType convertReferenceToPointer(SType);
nullable<SType> convertPointerToReferenceStrict(SType);
nullable<SType> convertReferenceToPointerStrict(SType);
