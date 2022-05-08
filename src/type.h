#pragma once

#include "stdafx.h"
#include "util.h"
#include "operator.h"
#include "code_loc.h"
#include "context.h"
#include "function_name.h"

class Context;
struct TypeMapping;
struct FunctionSignature;
struct SwitchStatement;
struct FunctionDefinition;
struct Buffer;
struct Sections;
struct StatementBlock;
struct Statement;
struct AttributeType;
struct StructType;
struct ReferenceType;
struct MutableReferenceType;
struct PointerType;
struct MutablePointerType;
struct ConceptType;
struct OptionalType;
struct BuiltinType;
struct CompileTimeValue;
struct EnumDefinition;
struct MappingError {
  const Type* from;
  const Type* to;
  string toString() const;
};

struct Type : public owned_object<Type> {
  Type();
  virtual string getName(bool withTemplateArguments = true) const = 0;
  virtual string getCodegenName() const;
  virtual optional<string> getMangledName() const;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const;
  Type* replace(const Context&, Type* from, Type* to, ErrorBuffer&);
  virtual bool canReplaceBy(Type*) const;
  virtual Type* transform(function<Type*(Type*)>);
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&);
  virtual Type* expand(const Context&, Type* pack, vector<Type*> to, ErrorBuffer& errors);
  virtual Type* getType() const;
  virtual ReferenceType* asReferenceType()  { return nullptr; }
  virtual MutableReferenceType* asMutableReferenceType()  { return nullptr; }
  virtual PointerType* asPointerType()  { return nullptr; }
  virtual MutablePointerType* asMutablePointerType()  { return nullptr; }
  virtual StructType* asStructType() { return nullptr; }
  virtual ConceptType* asConceptType() { return nullptr; }
  virtual OptionalType* asOptionalType() { return nullptr; }
  virtual BuiltinType* asBuiltinType() { return nullptr; }
  virtual CompileTimeValue* asCompileTimeValue() { return nullptr; }
  virtual optional<CodeLoc> getDefinition() const { return none; }

  struct MemberInfo {
    Type* type;
    string name;
  };
  enum class ArgumentType {
    VALUE,
    REFERENCE,
    MUTABLE_REFERENCE
  };
  virtual WithError<Type*> getTypeOfMember(const string& name, ArgumentType = ArgumentType::VALUE) const;
  virtual optional<CodeLoc> getMemberLoc(const string& name) const; 
  virtual bool hasDestructor() const;
  virtual void codegenDefinition(Buffer*, Sections*);
  virtual ~Type() {}
  virtual WithErrorLine<Type*> instantiate(const Context&, vector<Type*> templateArgs, CodeLoc);
  Context& getStaticContext();
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, ArgumentType) const;
  virtual Type* removePointer();
  virtual Type* removeReference();
  bool isPointer();
  bool isReference();
  Type* removeValueReference();
  virtual JustError<string> getSizeError(const Context&) const;
  virtual bool canBeValueTemplateParam() const;
  virtual bool canDeclareVariable() const;
  virtual bool isMetaType() const;
  virtual WithError<Type*> convertTo(Type*);
  JustError<string> isBuiltinCopyable(const Context&, unique_ptr<Expression>&);
  bool isBuiltinCopyable(const Context&);
  Context staticContext;

  protected:
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const;
};

struct FunctionType : public Type {
  virtual string getName(bool withTemplateArguments = true) const;
  virtual string getCodegenName() const;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const;
  static FunctionType* get(const string& name, vector<FunctionInfo*> overloads);
  string name;
  vector<FunctionInfo*> overloads;
  struct Private {};
  FunctionType(Private, string name, vector<FunctionInfo*> overloads);
};

struct BuiltinType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual bool canBeValueTemplateParam() const override;
  virtual bool canDeclareVariable() const override;
  virtual bool isMetaType() const override;
  virtual BuiltinType* asBuiltinType() override { return this; }
  using DefType = BuiltinType*;
  static DefType INT;
  static DefType DOUBLE;
  static DefType LONG;
  static DefType SHORT;
  static DefType BYTE;
  static DefType BOOL;
  static DefType VOID;
  static DefType CHAR;
  static DefType STRING;
  static DefType NORETURN;
  static DefType NULL_TYPE;
  static DefType ANY_TYPE;
  static DefType ANYTHING;
  static DefType ENUM_TYPE;
  static DefType STRUCT_TYPE;
  static DefType UNION_TYPE;
  static DefType ATTRIBUTE_TYPE;
  static DefType CONCEPT_TYPE;
  BuiltinType(const string& name, bool canDeclareVar, optional<string> codegenName = none);

  private:
  string name;
  string codegenName;
  bool canDeclareVar;
};

struct AttributeType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual bool canBeValueTemplateParam() const override;
  virtual bool canDeclareVariable() const override;
  virtual Type* getType() const override;
  static AttributeType* get(const string&);
  struct Private {};
  AttributeType(Private, const string& name);

  private:
  string name;
};

struct ReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual Type* removeReference() override;
  virtual JustError<MappingError> getMappingError(TypeMapping& mapping, Type* from) const override;
  virtual Type* transform(function<Type*(Type*)>) override;
  virtual WithError<Type*> getTypeOfMember(const string& name, ArgumentType) const override;
  virtual optional<CodeLoc> getMemberLoc(const string& name) const override;
  virtual Type* removePointer() override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, ArgumentType) const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual ReferenceType* asReferenceType() override { return this; }

  static ReferenceType* get(Type*);
  Type* underlying;
  ReferenceType(Type*);
};

struct MutableReferenceType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual Type* removeReference() override;
  virtual JustError<MappingError> getMappingError(TypeMapping& mapping, Type* from) const override;
  virtual Type* transform(function<Type*(Type*)>) override;
  virtual WithError<Type*> getTypeOfMember(const string& name, ArgumentType) const override;
  virtual optional<CodeLoc> getMemberLoc(const string& name) const override;
  virtual Type* removePointer() override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, ArgumentType) const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual MutableReferenceType* asMutableReferenceType() override { return this; }

  static MutableReferenceType* get(Type*);
  Type* underlying;
  struct Private {};
  MutableReferenceType(Private, Type*);
};

struct PointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* transform(function<Type*(Type*)>) override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual Type* removePointer() override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual PointerType* asPointerType() override { return this; }

  static PointerType* get(Type*);
  Type* underlying;
  struct Private {};
  PointerType(Private, Type*);
};

struct MutablePointerType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* transform(function<Type*(Type*)>) override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual Type* removePointer() override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual MutablePointerType* asMutablePointerType() override { return this; }

  static MutablePointerType* get(Type*);
  Type* underlying;
  struct Private {};
  MutablePointerType(Private, Type*);
};

struct OptionalType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual JustError<MappingError> getMappingError(TypeMapping& mapping, Type* from) const override;
  virtual Type* transform(function<Type*(Type*)>) override;
  void codegenDefinition(Buffer*, Sections* accu) override;
  virtual bool hasDestructor() const override;
  virtual OptionalType* asOptionalType() override { return this; }

  static OptionalType* get(Type*);
  Type* underlying;
  struct Private {};
  OptionalType(Private, Type*);
};

struct EnumType;

struct CompileTimeValue : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual bool canReplaceBy(Type*) const override;
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&) override;
  virtual Type* expand(const Context&, Type* pack, vector<Type*> to, ErrorBuffer&) override;
  virtual Type* getType() const override;
  virtual optional<string> getMangledName() const override;
  virtual WithError<Type*> convertTo(Type*) override;
  virtual CompileTimeValue* asCompileTimeValue() override { return this; }
  struct TemplateValue {
    Type* type;
    string name;
    COMPARABLE(TemplateValue, type, name)
  };
  struct TemplateExpression {
    Operator op;
    vector<Type*> args;
    Type* type;
    COMPARABLE(TemplateExpression, op, args, type)
  };
  struct TemplateFunctionCall {
    string name;
    vector<Type*> args;
    Type* retVal;
    CodeLoc loc;
    vector<CodeLoc> argLoc;
    Context::BuiltInFunctionInfo functionInfo;
    COMPARABLE(TemplateFunctionCall, name, args)
  };
  struct EnumValue {
    EnumType* type;
    int index;
    COMPARABLE(EnumValue, type, index)
  };
  struct ReferenceValue {
    ReferenceValue(Type*);
    Type* value;
    int id;
    COMPARABLE(ReferenceValue, id)
  };
  struct NullValue { COMPARABLE(NullValue) };
  struct VoidValue { COMPARABLE(VoidValue) };
  struct CharLiteral {
    string literal;
    COMPARABLE(CharLiteral, literal)
  };
  using Value = variant<int, bool, double, CharLiteral, NullValue, VoidValue, string, EnumValue, TemplateValue,
      TemplateExpression, TemplateFunctionCall, ReferenceValue, FunctionType*>;
  static CompileTimeValue* get(Value);
  static CompileTimeValue* getReference(Type*);
  static CompileTimeValue* getTemplateValue(Type* type, string name);
  Value value;

  struct Private {};
  CompileTimeValue(Private, Value);
};

struct TemplateParameterType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual bool canReplaceBy(Type*) const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* getType() const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual WithError<Type*> getTypeOfMember(const string&, ArgumentType) const override;
  virtual bool canBeValueTemplateParam() const override;
  virtual optional<CodeLoc> getDefinition() const override;
  TemplateParameterType(string name, CodeLoc);
  TemplateParameterType(Type* type, string name, CodeLoc);
  string name;
  CodeLoc declarationLoc;
  Type* type;
};

struct TemplateRequirement {
  using Base = variant<Concept*, shared_ptr<Expression>>;
  TemplateRequirement(Base base, bool variadic) : base(std::move(base)), variadic(variadic) {}
  Base base;
  bool variadic;
  COMPARABLE(TemplateRequirement, base, variadic)
};

struct StructType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&) override;
  virtual Type* expand(const Context&, Type* pack, vector<Type*> to, ErrorBuffer&) override;
  virtual WithErrorLine<Type*> instantiate(const Context&, vector<Type*> templateArgs, CodeLoc) override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, ArgumentType) const override;
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual Type* getType() const override;
  virtual WithError<Type*> getTypeOfMember(const string&, ArgumentType) const override;
  virtual optional<CodeLoc> getMemberLoc(const string& name) const override;
  virtual bool hasDestructor() const override;
  virtual StructType* asStructType() override { return this; }
  virtual optional<CodeLoc> getDefinition() const override;
  string name;
  StructType* getInstance(vector<Type*> templateParams);
  vector<Type*> templateParams;
  vector<StructType*> instances;
  StructType* parent = nullptr;
  FunctionInfo* destructor = nullptr;
  vector<TemplateRequirement> requirements;
  struct Variable {
    string name;
    Type* type;
    bool isConst;
    CodeLoc codeLoc;
  };
  vector<Variable> members;
  vector<Variable> alternatives;
  bool external = false;
  vector<int> memberTemplateParams;
  optional<CodeLoc> definition;
  void updateInstantations(const Context&);
  struct Private {};
  StructType(string name, Private);
};

struct EnumType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual JustError<ErrorLoc> handleSwitchStatement(SwitchStatement&, Context&, ArgumentType) const override;
  virtual bool isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const override;
  virtual Type* getType() const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual optional<CodeLoc> getDefinition() const override;

  string name;
  vector<string> elements;
  bool external = false;
  EnumDefinition* definition = nullptr;
  struct Private {};
  EnumType(string name, Private);
};

struct ArrayType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual string getCodegenName() const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&) override;
  virtual Type* expand(const Context&, Type* pack, vector<Type*> to, ErrorBuffer&) override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  static ArrayType* get(Type*, SCompileTimeValue size);
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual bool hasDestructor() const override;
  ArrayType(Type*, SCompileTimeValue size);
  SCompileTimeValue size;
  Type* underlying;
};

struct VariablePack : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  VariablePack(Type*, string);
  string identifier;
  Type* packType;
};

struct FunctionSignature {
  FunctionSignature(Type* returnType, vector<Type*> params, vector<Type*> templateParams);
  FunctionSignature setBuiltin();
  Type* retVal;
  vector<Type*> params;
  vector<Type*> templateParams;
  vector<TemplateRequirement> requirements;
  Type* parentType = nullptr;
  Concept* concept = nullptr;
  bool builtinOperator = false;
  bool generatedConstructor = false;
  bool variadicTemplate = false;
  bool variadicParams = false;
  COMPARABLE(FunctionSignature, retVal, params, templateParams, parentType, variadicTemplate, variadicParams, requirements, concept, builtinOperator, generatedConstructor)
};

namespace std {
  template <>
  struct hash<FunctionSignature> {
    size_t operator()(const FunctionSignature& x) const {
      auto ret = size_t(x.retVal) + size_t(x.parentType) + size_t(x.concept)
          + size_t(x.builtinOperator) + 2 * size_t(x.generatedConstructor) + 3 * size_t(x.variadicTemplate)
          + 4 * size_t(x.variadicParams);
      for (auto p : x.params)
        ret += size_t(p);
      for (auto p : x.templateParams)
        ret += size_t(p);
      for (auto& r : x.requirements) {
        ret += size_t(r.variadic);
        ret += r.base.visit(
          [](Concept* concept) {
            return size_t(concept);
          },
          [](const shared_ptr<Expression>& expr) {
            return size_t(expr.get());
          }
        );
      }
      return ret;
    }
  };
}

struct FunctionInfo : public owned_object<FunctionInfo> {
  struct Private {};
  static FunctionInfo* getImplicit(FunctionId, FunctionSignature);
  static FunctionInfo* getInstance(FunctionId, FunctionSignature, FunctionInfo* parent);
  static FunctionInfo* getDefined(FunctionId, FunctionSignature, FunctionDefinition*);
  const FunctionId id;
  const FunctionSignature type;
  const string& prettyString() const;
  string getMangledName();
  bool isMainFunction() const;
  optional<string> getMangledSuffix();
  optional<string> getParamName(int index, const FunctionDefinition*) const;
  FunctionInfo* getWithoutRequirements();
  FunctionInfo(Private, FunctionId, FunctionSignature, FunctionInfo* parent);
  FunctionInfo(Private, FunctionId, FunctionSignature, FunctionDefinition*);
  FunctionInfo* getParent();
  FunctionDefinition* getDefinition();
  JustError<ErrorLoc> addInstance(const Context&);
  bool isConceptTypeFunction() const;

  private:
  FunctionInfo* parent = nullptr;
  FunctionInfo* noRequirements = nullptr;
  FunctionDefinition* const definition = nullptr;
  string pretty;
  void genPrettyString();
};

struct LambdaType : public Type {
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual string getCodegenName() const override;
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&) override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual bool hasDestructor() const override;
  static LambdaType* get(string name, vector<Type*> templateParams);
  static LambdaType* get(vector<Type*> templateParams);
  struct Private {};
  LambdaType(Private);
  FunctionInfo* functionInfo = nullptr;
  vector<LambdaCapture> captures;
  vector<Type*> templateParams;
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
  Concept* translate(vector<Type*> params, bool variadicParams, ErrorBuffer&) const;
  Concept* replace(const Context&, Type* from, Type* to, ErrorBuffer&) const;
  Concept* expand(const Context&, Type* from, vector<Type*> newParams, ErrorBuffer& errors);
  const vector<Type*>& getParams() const;
  bool isVariadic() const;
  JustError<ErrorLoc> canCreateConceptType() const;
  const Context& getContext() const;
  vector<Type*>& modParams();
  Context& modContext();
  ConceptDefinition* def;

  private:
  vector<Type*> params;
  string name;
  Context context;
  bool variadic;
};

struct ConceptType : public Type {
  static ConceptType* get(Concept*, vector<Type*> params, bool variadic);
  struct Private{};
  ConceptType(Private, Concept*, vector<Type*> params, bool variadic);
  virtual string getName(bool withTemplateArguments = true) const override;
  virtual optional<string> getMangledName() const override;
  virtual Type* replaceImpl(const Context&, Type* from, Type* to, ErrorBuffer&) override;
  virtual JustError<string> getSizeError(const Context&) const override;
  virtual JustError<MappingError> getMappingError(TypeMapping&, Type* argType) const override;
  virtual Type* expand(const Context&, Type* pack, vector<Type*> to, ErrorBuffer&) override;
  virtual bool hasDestructor() const override;
  virtual Type* getType() const override;
  virtual void codegenDefinition(Buffer*, Sections*) override;
  virtual ConceptType* asConceptType() override { return this; }
  virtual optional<CodeLoc> getDefinition() const override;

  Concept* getConceptFor(Type*) const;
  Concept* concept = nullptr;
  vector<Type*> params;
  bool variadic;
};

struct Expression;
class Context;

struct IdentifierInfo;
extern WithErrorLine<FunctionInfo*> instantiateFunction(const Context& context, FunctionInfo*, CodeLoc,
    vector<Type*> templateArgs, vector<Type*> argTypes, vector<CodeLoc> argLoc,
    vector<FunctionSignature> existing = {});
extern FunctionSignature replaceInFunction(const Context&, FunctionSignature, Type* from, Type* to, ErrorBuffer&,
    const vector<Type*>& origParams);
extern FunctionInfo* replaceInFunction(const Context&, FunctionInfo*, Type* from, Type* to, ErrorBuffer&);
extern FunctionInfo* addTemplateParams(FunctionInfo*, vector<Type*> params, bool variadic);
extern string joinTemplateParams(const vector<Type*>&, bool variadic = false);
extern optional<string> mangleTemplateParams(const vector<Type*>&);
extern string joinTypeList(const vector<Type*>&);
extern string joinTemplateParamsCodegen(const vector<Type*>&);
extern string joinTypeListCodegen(const vector<Type*>&);
extern string getExpandedParamName(const string& packName, int index);
void generateConversions(const Context&, const vector<Type*>& paramTypes, const vector<Type*>& argTypes,
    vector<unique_ptr<Expression>>&);

Type* convertPointerToReference(Type*);
Type* convertReferenceToPointer(Type*);
Type* convertPointerToReferenceStrict(Type*);
Type* convertReferenceToPointerStrict(Type*);
vector<FunctionInfo*> getSpecialOverloads(const FunctionId&, const vector<Type*>& argTypes);
