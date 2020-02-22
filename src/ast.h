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
class MoveChecker;

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

struct Statement;

using ExprTransformFun = function<unique_ptr<Expression>(Expression*)>;
using StmtTransformFun = function<unique_ptr<Statement>(Statement*)>;

unique_ptr<Statement> identityStmt(Statement*);
unique_ptr<Expression> identityExpr(Expression*);

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&, CodegenStage) const {}
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const { return success; }
  virtual ~Node() {}
};

extern WithErrorLine<SType> getType(Context&, unique_ptr<Expression>&, bool evaluateAtCompileTime = true);

struct EvalResult {
  SType value;
  bool isConstant;
};

struct Expression : Node {
  using Node::Node;
  using TransformFun = function<unique_ptr<Expression>(Expression*)>;
  virtual WithErrorLine<SType> getTypeImpl(Context&) = 0;

  virtual WithEvalError<EvalResult> eval(const Context&) const;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const;
  virtual unique_ptr<Expression> expand(SType from, vector<SType> to) const;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const;
  virtual unique_ptr<Expression> expandVar(string from, vector<string> to) const;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const = 0;
  unique_ptr<Expression> deepCopy() const;
};

struct Constant : Expression {
  Constant(CodeLoc, SCompileTimeValue);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  SCompileTimeValue value;
  optional<string> structMemberName;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, string enumElement);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  string enumName;
  nullable<SType> enumType;
  string enumElement;
};

struct Variable : Expression {
  Variable(IdentifierInfo);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
};

struct MemberAccessExpression : Expression {
  static unique_ptr<MemberAccessExpression> getPointerAccess(CodeLoc, unique_ptr<Expression> lhs, string);
  MemberAccessExpression(CodeLoc, unique_ptr<Expression> lhs, string);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  string identifier;
  bool isVariant = false;
  nullable<SFunctionInfo> destructorCall;
};

struct MemberIndexExpression : Expression {
  MemberIndexExpression(CodeLoc, unique_ptr<Expression> lhs, unique_ptr<Expression> index);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  unique_ptr<Expression> index;
  optional<string> memberName;
  bool isVariant = false;
  nullable<SFunctionInfo> destructorCall;
};

struct BinaryExpression : Expression {
  static unique_ptr<Expression> get(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  static unique_ptr<Expression> get(CodeLoc, Operator, vector<unique_ptr<Expression>>);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> expandVar(string from, vector<string> to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  vector<unique_ptr<Expression>> expr;
  nullable<SFunctionInfo> functionInfo;
  nullable<SFunctionInfo> destructorCall[2];
  struct Private {};
  BinaryExpression(Private, CodeLoc, Operator, vector<unique_ptr<Expression>>);
  JustError<ErrorLoc> considerDestructorCall(const Context&, int index, const SType& argType);
};


struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  unique_ptr<Expression> expr;
  nullable<SFunctionInfo> functionInfo;
  nullable<SFunctionInfo> destructorCall;
};

struct TernaryExpression : Expression {
  TernaryExpression(CodeLoc, unique_ptr<Expression>, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> condExpr;
  unique_ptr<Expression> e1;
  unique_ptr<Expression> e2;
};

struct MoveExpression : Expression {
  MoveExpression(CodeLoc, string, bool hasDestructor = false);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string identifier;
  nullable<SType> type;
  bool hasDestructor = false;
};

struct CountOfExpression : Expression {
  CountOfExpression(CodeLoc, string);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> expand(SType from, vector<SType> to) const override;
  virtual unique_ptr<Expression> expandVar(string from, vector<string> to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string identifier;
  nullable<SType> type;
  optional<int> count;
};

struct VariablePackElement : Expression {
  VariablePackElement(CodeLoc, string packName, vector<string> ids, SCompileTimeValue index);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string packName;
  vector<string> ids;
  SCompileTimeValue index;
};

struct StatementBlock;

struct FunctionParameter {
  CodeLoc codeLoc;
  IdentifierInfo type;
  optional<string> name;
  bool isMutable;
  bool isVirtual;
};

struct LambdaExpression : Expression {
  LambdaExpression(CodeLoc, vector<FunctionParameter>, unique_ptr<StatementBlock>, optional<IdentifierInfo> returnType,
      LambdaCaptureInfo captureInfo);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  JustError<ErrorLoc> checkBodyMoves() const;
  vector<FunctionParameter> parameters;
  unique_ptr<StatementBlock> block;
  optional<IdentifierInfo> returnType;
  nullable<shared_ptr<LambdaType>> type;
  bool recheck = false;
  LambdaCaptureInfo captureInfo;
};

struct ArrayLiteral : Expression {
  ArrayLiteral(CodeLoc);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  vector<unique_ptr<Expression>> contents;
  optional<IdentifierInfo> typeId;
  nullable<SType> type;
};

enum class MethodCallType { METHOD, FUNCTION_AS_METHOD, FUNCTION_AS_METHOD_WITH_POINTER };

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, IdentifierInfo, bool methodCall);
  FunctionCall(CodeLoc, IdentifierInfo, unique_ptr<Expression> arg, bool methodCall);
  static unique_ptr<FunctionCall> constructor(CodeLoc, SType type);
  virtual WithErrorLine<SType> getTypeImpl(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Expression> expand(SType from, vector<SType> to) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> expandVar(string from, vector<string> to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  optional<IdentifierInfo> identifier;
  optional<IdentifierType> identifierType;
  optional<vector<SType>> templateArgs;
  nullable<SFunctionInfo> functionInfo;
  vector<unique_ptr<Expression>> arguments;
  vector<optional<string>> argNames;
  optional<MethodCallType> callType;
  bool methodCall = false;
  bool variadicArgs = false;
  bool variadicTemplateArgs = false;
  struct Private {};
  FunctionCall(CodeLoc, bool methodCall, Private);

  private:
  JustError<ErrorLoc> initializeTemplateArgsAndIdentifierType(const Context&);
  JustError<ErrorLoc> checkNamedArgs() const;
  JustError<ErrorLoc> checkVariadicCall(const Context&);
};

struct AST;

struct Statement : Node {
  using Node::Node;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext);
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool topLevelNotInImport = false) { return success; }
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override final;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const;
  NODISCARD virtual bool hasReturnStatement(const Context&) const;
  virtual void addGeneratedConstructor(Context&, const AST&) const {}
  virtual void codegen(Accu&, CodegenStage) const override {}
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorLocBuffer&) const;
  virtual unique_ptr<Statement> expand(SType from, vector<SType> to) const;
  virtual unique_ptr<Statement> replaceVar(string from, string to) const;
  virtual unique_ptr<Statement> expandVar(string from, vector<string> to) const;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const;
  unique_ptr<Statement> deepCopy() const;
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
  bool isStatic = false;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ExternConstantDeclaration : Statement {
  ExternConstantDeclaration(CodeLoc, IdentifierInfo type, string identifier);
  IdentifierInfo type;
  nullable<SType> realType;
  string identifier;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
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
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  StatementBlock(CodeLoc, vector<unique_ptr<Statement>>);
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  ReturnStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct BreakStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ContinueStatement : Statement {
  using Statement::Statement;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;
  bool canDiscard = false;
  bool noReturnExpr = false;
};

struct ForLoopStatement : Statement {
  ForLoopStatement(CodeLoc l, unique_ptr<Statement> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  unique_ptr<Statement> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct StaticForLoopStatement : Statement {
  StaticForLoopStatement(CodeLoc l, string counter, unique_ptr<Expression> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  string counter;
  unique_ptr<Expression> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  bool wasChecked = false;
  vector<unique_ptr<Statement>> unrolled;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  JustError<ErrorLoc> checkExpressions(const Context&) const;
  WithErrorLine<vector<unique_ptr<Statement>>> getUnrolled(const Context&, SType counterType) const;
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
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
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
  bool variadic = false;
  struct ConceptRequirement {
    IdentifierInfo identifier;
    bool variadic = false;
  };
  using Requirement = variant<ConceptRequirement, shared_ptr<Expression>>;
  vector<Requirement> requirements;
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
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void addGeneratedConstructor(Context&, const AST&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  bool external = false;
};

struct UnionDefinition : Statement {
  UnionDefinition(CodeLoc, string name);
  string name;
  struct Element {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Element> elements;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct ConceptDefinition : Statement {
  ConceptDefinition(CodeLoc, string name);
  string name;
  vector<unique_ptr<FunctionDefinition>> functions;
  TemplateInfo templateInfo;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<string> elements;
  bool external = false;
  bool fullyDefined = true;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  public:
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    // returns null if there is no type in the element or error if there was an error getting the type
    WithErrorLine<nullable<SType>> getType(const Context&);
    variant<none_t, IdentifierInfo, SType> type = none;
    vector<string> ids;
    optional<string> declaredVar;
    unique_ptr<StatementBlock> block;
    enum VarType { VALUE, POINTER, NONE } varType = NONE;
    CaseElem replace(SType from, SType to, ErrorLocBuffer&) const;
    CaseElem transform(const StmtTransformFun&, const ExprTransformFun&) const;
  };
  nullable<SType> targetType;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  nullable<SFunctionInfo> destructorCall;
  enum { ENUM, VARIANT} type;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorLocBuffer&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;

  private:
  void codegenEnum(Accu&) const;
  void codegenVariant(Accu&) const;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, FunctionId);
  IdentifierInfo returnType;
  FunctionId name;
  using Parameter = FunctionParameter;
  vector<Parameter> parameters;
  vector<unique_ptr<Statement>> destructorCalls;
  unique_ptr<StatementBlock> body;
  struct InstanceInfo {
    unique_ptr<StatementBlock> body;
    vector<unique_ptr<Statement>> destructorCalls;
    SFunctionInfo functionInfo;
    vector<SFunctionInfo> requirements;
    NODISCARD JustError<ErrorLoc> generateBody(StatementBlock* parentBody, CodeLoc);
  };
  vector<InstanceInfo> instances;
  TemplateInfo templateInfo;
  nullable<SFunctionInfo> functionInfo;
  optional<Context> definitionContext;
  bool wasChecked = false;
  bool external = false;
  bool isVirtual = false;
  bool isDefault = false;
  bool isVariadicParams = false;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  NODISCARD JustError<ErrorLoc> setFunctionType(const Context&, bool concept = false, bool builtInImport = false);
  void handlePointerParamsInOperator(Accu&, const StatementBlock*) const;
  void handlePointerReturnInOperator(Accu&, const StatementBlock*) const;
  void addStacktraceGenerator(Accu&, const StatementBlock*) const;
  NODISCARD JustError<ErrorLoc> generateVirtualDispatchBody(Context& bodyContext);
  WithErrorLine<unique_ptr<Expression>> getVirtualFunctionCallExpr(const Context&, const string& funName,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  WithErrorLine<unique_ptr<Expression>> getVirtualOperatorCallExpr(Context&, Operator,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  NODISCARD JustError<ErrorLoc> checkAndGenerateCopyFunction(const Context&, const string&);
  NODISCARD JustError<ErrorLoc> checkAndGenerateDefaultConstructor(const Context&);
  NODISCARD JustError<ErrorLoc> addInstance(const Context* callContext, const SFunctionInfo&);
  NODISCARD JustError<ErrorLoc> generateDefaultBodies(Context&);
  NODISCARD JustError<ErrorLoc> checkBody(const vector<SFunctionInfo>& requirements, StatementBlock& myBody,
      const FunctionInfo& instanceInfo, vector<unique_ptr<Statement> >& destructorCalls) const;
  NODISCARD JustError<ErrorLoc> checkForIncompleteTypes(const Context&);
  WithErrorLine<SType> getReturnType(const Context&) const;
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
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual unique_ptr<Statement> replace(SType from, SType to, ErrorLocBuffer&) const override;
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
  virtual JustError<ErrorLoc> addToContext(Context&, ImportCache& cache, const Context& primaryContext) override;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  private:
  NODISCARD JustError<ErrorLoc> processImport(const Context& primaryContext, Context&, ImportCache&, const string& content,
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
extern WithErrorLine<SFunctionInfo> getCopyFunction(const Context&, CodeLoc callLoc, const SType&);
extern WithErrorLine<SFunctionInfo> getImplicitCopyFunction(const Context&, CodeLoc callLoc, const SType&);
