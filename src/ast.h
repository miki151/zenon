#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "context.h"
#include "operator.h"
#include "identifier.h"
#include "type.h"
#include "import_cache.h"
#include "identifier_type.h"

struct Accu;

struct CodegenStage {
  static CodegenStage types();
  static CodegenStage define();
  static CodegenStage declare();
  CodegenStage setImport();
  bool isTypes;
  bool isDefine;
  bool isImport;

  private:
  CodegenStage() {}
};

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&, CodegenStage) const {}
  virtual ~Node() {}
};

extern WithErrorLine<SType> getType(Context&, unique_ptr<Expression>&, bool evaluateAtCompileTime = true);

struct Expression : Node {
  using Node::Node;
  virtual WithErrorLine<SType> getTypeImpl(Context&) = 0;
  virtual nullable<SType> eval(const Context&) const;
  virtual WithErrorLine<SType> getDotOperatorType(Expression* left, Context& callContext);
  virtual void codegenDotOperator(Accu&, CodegenStage, Expression* leftSide) const;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const = 0;
};

struct Constant : Expression {
  Constant(CodeLoc, SCompileTimeValue);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  SCompileTimeValue value;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, string enumElement);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  string enumName;
  nullable<SType> enumType;
  string enumElement;
};

struct Variable : Expression {
  Variable(CodeLoc, IdentifierInfo);
  Variable(CodeLoc, string);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual WithErrorLine<SType> getDotOperatorType(Expression* left, Context& callContext) override;
  IdentifierInfo identifier;
};

struct BinaryExpression : Expression {
  static unique_ptr<Expression> get(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  static unique_ptr<Expression> get(CodeLoc, Operator, vector<unique_ptr<Expression>>);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  Operator op;
  vector<unique_ptr<Expression>> expr;
  nullable<SFunctionInfo> functionInfo;
  struct Private {};
  BinaryExpression(Private, CodeLoc, Operator, vector<unique_ptr<Expression>>);
};

struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  Operator op;
  unique_ptr<Expression> expr;
  nullable<SFunctionInfo> functionInfo;
};

struct MoveExpression : Expression {
  MoveExpression(CodeLoc, string);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string identifier;
  nullable<SType> type;
};

struct ArrayLiteral : Expression {
  ArrayLiteral(CodeLoc);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  vector<unique_ptr<Expression>> contents;
};

enum class MethodCallType { METHOD, FUNCTION_AS_METHOD, FUNCTION_AS_METHOD_WITH_POINTER };

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, IdentifierInfo);
  FunctionCall(CodeLoc, IdentifierInfo, unique_ptr<Expression> arg);
  static unique_ptr<FunctionCall> constructor(CodeLoc, SType type);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual WithErrorLine<SType> getDotOperatorType(Expression* left, Context& callContext) override;
  virtual void codegenDotOperator(Accu&, CodegenStage, Expression* leftSide) const override;
  virtual nullable<SType> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorBuffer&) const override;
  optional<IdentifierInfo> identifier;
  optional<IdentifierType> identifierType;
  optional<vector<SType>> templateArgs;
  nullable<SFunctionInfo> functionInfo;
  vector<unique_ptr<Expression>> arguments;
  vector<optional<string>> argNames;
  optional<MethodCallType> callType;

  struct Private {};
  FunctionCall(CodeLoc, Private);
};

struct Statement : Node {
  using Node::Node;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&);
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext);
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool topLevelNotInImport = false) { return none; }
  NODISCARD virtual bool hasReturnStatement(const Context&) const;
  virtual void codegen(Accu&, CodegenStage) const {}
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const;
  enum class TopLevelAllowance {
    CANT,
    CAN,
    MUST
  };
  virtual TopLevelAllowance allowTopLevel() const { return TopLevelAllowance::CANT; }
  bool exported = false;
};

struct VariableDeclaration : Statement {
  VariableDeclaration(CodeLoc, optional<IdentifierInfo> type, string identifier, unique_ptr<Expression> initExpr);
  optional<IdentifierInfo> type;
  nullable<SType> realType;
  string identifier;
  unique_ptr<Expression> initExpr;
  bool isMutable = false;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
};

struct ExternConstantDeclaration : Statement {
  ExternConstantDeclaration(CodeLoc, IdentifierInfo type, string identifier);
  IdentifierInfo type;
  nullable<SType> realType;
  string identifier;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct IfStatement : Statement {
  IfStatement(CodeLoc,
      unique_ptr<VariableDeclaration> decl /*can be null*/,
      unique_ptr<Expression> cond /*can be null if decl is non-null*/,
      unique_ptr<Statement> ifTrue,
      unique_ptr<Statement> ifFalse /* can be null*/);
  unique_ptr<VariableDeclaration> declaration;
  unique_ptr<Expression> condition;
  unique_ptr<Statement> ifTrue, ifFalse;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  ReturnStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct BreakStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ContinueStatement : Statement {
  using Statement::Statement;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  bool canDiscard = false;
};

struct ForLoopStatement : Statement {
  ForLoopStatement(CodeLoc l, unique_ptr<Statement> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  unique_ptr<Statement> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct RangedLoopStatement : Statement {
  RangedLoopStatement(CodeLoc l, unique_ptr<VariableDeclaration> init, unique_ptr<Expression> container,
      unique_ptr<Statement> body);
  unique_ptr<VariableDeclaration> init;
  unique_ptr<Expression> container;
  unique_ptr<Expression> condition;
  unique_ptr<Expression> increment;
  unique_ptr<Statement> body;
  optional<string> containerName;
  unique_ptr<VariableDeclaration> containerEnd;
  int loopId = 0;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct FunctionDefinition;

struct TemplateParameter {
  string name;
  optional<string> type;
  CodeLoc codeLoc;
};

struct TemplateInfo {
  vector<TemplateParameter> params;
  vector<IdentifierInfo> requirements;
};

struct StructDefinition : Statement {
  StructDefinition(CodeLoc, string name);
  bool incomplete = false;
  string name;
  struct Member {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Member> members;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  bool external = false;
};

struct VariantDefinition : Statement {
  VariantDefinition(CodeLoc, string name);
  string name;
  struct Element {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Element> elements;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct ConceptDefinition : Statement {
  ConceptDefinition(CodeLoc, string name);
  string name;
  vector<unique_ptr<FunctionDefinition>> functions;
  TemplateInfo templateInfo;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<string> elements;
  bool external;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    // returns null if there is no type in the element or error if there was an error getting the type
    WithErrorLine<nullable<SType>> getType(const Context&);
    variant<none_t, IdentifierInfo, SType> type = none;
    string id;
    unique_ptr<StatementBlock> block;
    enum VarType { VALUE, POINTER, NONE } varType = NONE;
    CaseElem replace(SType from, SType to, ErrorBuffer&) const;
  };
  nullable<SType> targetType;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  enum { ENUM, VARIANT} type;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;

  private:
  void codegenEnum(Accu&) const;
  void codegenVariant(Accu&) const;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, FunctionName);
  IdentifierInfo returnType;
  FunctionName name;
  struct Parameter {
    CodeLoc codeLoc;
    IdentifierInfo type;
    optional<string> name;
    bool isMutable;
    bool isVirtual;
  };
  vector<Parameter> parameters;
  unique_ptr<StatementBlock> body;
  struct InstanceInfo {
    unique_ptr<StatementBlock> body;
    SFunctionInfo functionInfo;
    Context::ConstStates callContext;
    void generateBody(StatementBlock* parentBody);
  };
  vector<InstanceInfo> instances;
  TemplateInfo templateInfo;
  nullable<SFunctionInfo> functionInfo;
  Context::ConstStates definitionContext;
  bool external = false;
  struct Initializer {
    CodeLoc codeLoc;
    string paramName;
    unique_ptr<Expression> expr;
  };
  bool isVirtual = false;
  bool isDefault = false;
  vector<Initializer> initializers;
  NODISCARD virtual optional<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual optional<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  NODISCARD optional<ErrorLoc> setFunctionType(const Context&, bool concept = false, bool builtInImport = false);
  void handlePointerParamsInOperator(Accu&, const StatementBlock*) const;
  void handlePointerReturnInOperator(Accu&, const StatementBlock*) const;
  NODISCARD optional<ErrorLoc> generateVirtualDispatchBody(Context& bodyContext);
  WithErrorLine<unique_ptr<Expression>> getVirtualFunctionCallExpr(const Context&, const string& funName,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  WithErrorLine<unique_ptr<Expression>> getVirtualOperatorCallExpr(Context&, Operator,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  NODISCARD optional<ErrorLoc> checkAndGenerateCopyFunction(const Context&);
  void addInstance(const Context& callContext, const SFunctionInfo&);
  NODISCARD optional<ErrorLoc> generateDefaultBodies(Context&);
  NODISCARD optional<ErrorLoc> checkBody(TypeRegistry*, Context::ConstStates callContext, StatementBlock& myBody,
      const FunctionInfo& instanceInfo) const;
  NODISCARD optional<ErrorLoc> checkForIncompleteTypes(const Context&);
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  bool isTopLevel = false;
  struct ReplacementInfo {
    SType from;
    SType to;
  };
  vector<ReplacementInfo> replacements;
  virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorBuffer&) const override;
  virtual bool hasReturnStatement(const Context&) const override;
};

struct AST;

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, string path, bool isBuiltIn);
  string path;
  vector<string> importDirs;
  unique_ptr<AST> ast;
  const bool isBuiltIn;
  void setImportDirs(const vector<string>& importDirs);
  virtual optional<ErrorLoc> addToContext(Context&, ImportCache& cache, const Context& primaryContext) override;
  virtual optional<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  private:
  NODISCARD optional<ErrorLoc> processImport(const Context& primaryContext, Context&, ImportCache&, const string& content,
      const string& path);
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};

struct ModuleInfo {
  string path;
  bool builtIn;
};

extern WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST&, Context& context, const Context& primary,
    const vector<string>& importPaths, bool builtIn);
extern Context createPrimaryContext(TypeRegistry*);
