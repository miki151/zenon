#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "context.h"
#include "operator.h"
#include "identifier.h"
#include "type.h"

struct Buffer;
struct Sections;
class MoveChecker;
class ImportCache;
class ASTCache;

struct Statement;

using ExprTransformFun = function<unique_ptr<Expression>(Expression*)>;
using StmtTransformFun = function<unique_ptr<Statement>(Statement*)>;
using ExprVisitFun = function<void(Expression*)>;
using StmtVisitFun = function<void(Statement*)>;
using FunctionCallVisitFun = function<void(FunctionInfo*)>;
using LambdasSet = unordered_set<const LambdaType*>;
using ConceptsSet = unordered_set<const ConceptType*>;

unique_ptr<Statement> identityStmt(Statement*);
unique_ptr<Expression> identityExpr(Expression*);

struct Sections;
struct Buffer;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Buffer*, Sections*) const {}
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const { return success; }
  virtual ~Node() {}
};

extern WithErrorLine<Type*> getType(const Context&, unique_ptr<Expression>&, bool evaluateAtCompileTime = true);

struct EvalResult {
  Type* value;
  bool isConstant;
};

struct Expression : Node {
  using Node::Node;
  using TransformFun = function<unique_ptr<Expression>(Expression*)>;
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) = 0;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const;
  virtual WithEvalError<EvalResult> eval(const Context&) const;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const = 0;
  unique_ptr<Expression> deepCopy() const;
};

struct Constant : Expression {
  Constant(CodeLoc, Type*);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  Type* value;
  optional<string> structMemberName;
  Type* refValue = nullptr;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, CodeLoc, string enumElement);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  string enumName;
  Type* enumType = nullptr;
  string enumElement;
  CodeLoc elemLoc;
};

struct Variable : Expression {
  Variable(IdentifierInfo);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
  Type* getConstantValue(const Context&) const;
};

struct MemberAccessExpression : Expression {
  static unique_ptr<MemberAccessExpression> getPointerAccess(CodeLoc, unique_ptr<Expression> lhs, string);
  MemberAccessExpression(CodeLoc, unique_ptr<Expression> lhs, string);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  string identifier;
  bool isUnion = false;
  FunctionInfo* destructorCall = nullptr;
  bool isMainDestructor = false;
};

struct BinaryExpression : Expression {
  static unique_ptr<Expression> get(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  static unique_ptr<Expression> get(CodeLoc, Operator, vector<unique_ptr<Expression>>);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  vector<unique_ptr<Expression>> expr;
  FunctionInfo* functionInfo = nullptr;
  FunctionInfo* destructorCall[2] = {nullptr, nullptr};
  struct Private {};
  BinaryExpression(Private, CodeLoc, Operator, vector<unique_ptr<Expression>>);
  JustError<ErrorLoc> considerDestructorCall(const Context&, int index, Type* argType);
};


struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  unique_ptr<Expression> expr;
  FunctionInfo* functionInfo = nullptr;
  FunctionInfo* destructorCall = nullptr;
};

struct TernaryExpression : Expression {
  TernaryExpression(CodeLoc, unique_ptr<Expression>, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> condExpr;
  unique_ptr<Expression> e1;
  unique_ptr<Expression> e2;
};

struct MoveExpression : Expression {
  MoveExpression(CodeLoc, string, CodeLoc variableLoc, bool hasDestructor = false);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string identifier;
  Type* type = nullptr;
  bool hasDestructor = false;
  CodeLoc variableLoc;
};

struct CountOfExpression : Expression {
  CountOfExpression(CodeLoc, string);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string identifier;
  Type* type = nullptr;
};

struct VariablePackElement : Expression {
  VariablePackElement(CodeLoc, string packName, unique_ptr<Expression> index);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string packName;
  unique_ptr<Expression> index;
  optional<string> codegenName;
};

struct StatementExpression : Expression {
  StatementExpression(CodeLoc, vector<unique_ptr<Statement>> statements, unique_ptr<Expression> value);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  vector<unique_ptr<Statement>> statements;
  unique_ptr<Expression> value;
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
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  JustError<ErrorLoc> checkBodyMoves() const;
  WithErrorLine<vector<LambdaCapture>> setLambda(Context&);
  vector<FunctionParameter> parameters;
  unique_ptr<StatementBlock> block;
  optional<IdentifierInfo> returnType;
  LambdaType* type = nullptr;
  LambdaCaptureInfo captureInfo;
  vector<FunctionInfo*> functionCalls;
};

struct ArrayLiteral : Expression {
  ArrayLiteral(CodeLoc);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  vector<unique_ptr<Expression>> contents;
  optional<IdentifierInfo> typeId;
  Type* type = nullptr;
};

struct FatPointerConversion : Expression {
  FatPointerConversion(CodeLoc, vector<FunctionInfo*> functions, Type* toType, Type* argType, unique_ptr<Expression> arg,
      ConceptType* conceptType);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  Type* toType = nullptr;
  Type* argType = nullptr;
  unique_ptr<Expression> arg;
  ConceptType* conceptType = nullptr;
  vector<FunctionInfo*> functions;
};

enum class MethodCallType { METHOD, FUNCTION_AS_METHOD, FUNCTION_AS_METHOD_WITH_POINTER };

struct FunctionCall : Expression {
  FunctionCall(IdentifierInfo, bool methodCall);
  FunctionCall(IdentifierInfo, unique_ptr<Expression> arg, bool methodCall);
  virtual WithErrorLine<Type*> getTypeImpl(const Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
  CodeLoc endLoc;
  optional<vector<Type*>> templateArgs;
  FunctionInfo* functionInfo = nullptr;
  vector<unique_ptr<Expression>> arguments;
  vector<string> argNames;
  optional<MethodCallType> callType;
  FunctionInfo* destructorCall = nullptr;
  bool methodCall = false;
  bool variadicArgs = false;
  bool variadicTemplateArgs = false;
  struct Private {};
  FunctionCall(CodeLoc, bool methodCall, Private);

  private:
  JustError<ErrorLoc> checkNamedArgs() const;
  JustError<ErrorLoc> checkVariadicCall(const Context&);
  JustError<ErrorLoc> considerSpecialCalls(const Context&);
};

struct AST;

struct AttributeInfo {
  string name;
  CodeLoc codeLoc;
};

using StatementEvalResult = vector<unique_ptr<Statement>>;

struct Statement : Node {
  using Node::Node;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext);
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool topLevelNotInImport = false) { return success; }
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override final;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const;
  unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const;
  NODISCARD virtual bool hasReturnStatement() const;
  virtual JustError<ErrorLoc> addGeneratedConstructor(Context&, const AST&) const { return success; }
  virtual void codegen(Buffer*, Sections*) const override {}
  virtual unique_ptr<Statement> replaceVar(string from, string to) const;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const = 0;
  virtual WithEvalError<StatementEvalResult> eval(Context&);
  virtual bool canHaveAttributes() const { return false; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*) { return success; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry* r, ASTCache&,
      const vector<string>& importDirs) {
    return registerTypes(primaryContext, r);
  }
  unique_ptr<Statement> deepCopy() const;
  enum class TopLevelAllowance {
    CANT,
    CAN,
    MUST
  };
  virtual TopLevelAllowance allowTopLevel() const { return TopLevelAllowance::CANT; }
  bool exported = false;
  vector<AttributeInfo> attributes;
};

struct VariableDeclaration : Statement {
  VariableDeclaration(CodeLoc, optional<IdentifierInfo> type, string identifier, unique_ptr<Expression> initExpr);
  optional<IdentifierInfo> type;
  Type* realType = nullptr;
  string identifier;
  unique_ptr<Expression> initExpr;
  bool isMutable = false;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool canHaveAttributes() const override { return true; }

  private:
  WithErrorLine<Type*> getRealType(const Context&) const;
  WithErrorLine<bool> considerShadowing();
};

struct AliasDeclaration : Statement {
  AliasDeclaration(CodeLoc, string identifier, unique_ptr<Expression> initExpr);
  Type* realType = nullptr;
  string identifier;
  unique_ptr<Expression> initExpr;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;

  private:
  WithErrorLine<Type*> getRealType(const Context&) const;
};

struct TypeAliasDeclaration : Statement {
  TypeAliasDeclaration(CodeLoc, string identifier, IdentifierInfo);
  string identifier;
  IdentifierInfo typeId;
  Type* type = nullptr;
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ExternConstantDeclaration : Statement {
  ExternConstantDeclaration(CodeLoc, IdentifierInfo type, string identifier);
  IdentifierInfo type;
  Type* realType = nullptr;
  string identifier;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
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
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  StatementBlock(CodeLoc, vector<unique_ptr<Statement>>);
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
};

struct ExternalStatement : Statement {
  ExternalStatement(Statement*);
  Statement* elem;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct UncheckedStatement : Statement {
  UncheckedStatement(CodeLoc, unique_ptr<Statement>);
  unique_ptr<Statement> elem;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ReturnStatement : Statement {
  ReturnStatement(CodeLoc);
  ReturnStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct BreakStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct ContinueStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
  bool canDiscard = false;
  bool noReturnExpr = false;
  bool isConstant = false;
};

unique_ptr<Statement> getForLoop(CodeLoc, unique_ptr<VariableDeclaration>, unique_ptr<Expression> cond,
      unique_ptr<Expression> iter, unique_ptr<Statement> body);

unique_ptr<Statement> getRangedLoop(CodeLoc, string iterator, unique_ptr<Expression> container,
      unique_ptr<Statement> body);

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body,
      unique_ptr<Statement> afterContinue);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  unique_ptr<Statement> afterContinue;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
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

struct AttributeDefinition : Statement {
  AttributeDefinition(CodeLoc, string name);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  string name;
};

struct StructDefinition : Statement {
  StructDefinition(CodeLoc, string name);
  string name;
  struct Member {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
    bool isConst;
    shared_ptr<Expression> defaultValue;
  };
  vector<Member> members;
  TemplateInfo templateInfo;
  StructType* type = nullptr;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual JustError<ErrorLoc> addGeneratedConstructor(Context&, const AST&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  bool external = false;
  bool checked = false;
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
  StructType* type = nullptr;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  bool checked = false;
};

struct ConceptDefinition : Statement {
  ConceptDefinition(CodeLoc, string name);
  string name;
  vector<unique_ptr<FunctionDefinition>> functions;
  TemplateInfo templateInfo;
  Concept* concept = nullptr;
  struct FatPointerInfo {
    Type* type;
    vector<FunctionInfo*> vTable;
  };
  void addFatPointer(FatPointerInfo);
  void addConceptType(ConceptType*);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;

  private:
  vector<FatPointerInfo> fatPointers;
  vector<ConceptType*> conceptInstances;
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<pair<string, CodeLoc>> elements;
  bool external = false;
  bool registered = false;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    vector<pair<string, CodeLoc>> ids;
    optional<string> declaredVar;
    unique_ptr<StatementBlock> block;
    CaseElem transform(const StmtTransformFun&, const ExprTransformFun&) const;
  };
  Type* targetType = nullptr;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  FunctionInfo* destructorCall = nullptr;
  enum { ENUM, UNION} type;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;

  private:
  void codegenEnum(Buffer*, Sections*) const;
  void codegenUnion(Buffer*, Sections*) const;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, FunctionId, CodeLoc idLoc);
  IdentifierInfo returnType;
  FunctionId name;
  CodeLoc idLoc;
  using Parameter = FunctionParameter;
  vector<Parameter> parameters;
  vector<unique_ptr<Statement>> destructorCalls;
  unique_ptr<StatementBlock> body, origBody;
  struct InstanceInfo {
    unique_ptr<StatementBlock> body;
    vector<unique_ptr<Statement>> destructorCalls;
    FunctionInfo* functionInfo;
    Context callContext;
  };
  vector<InstanceInfo> instances;
  TemplateInfo templateInfo;
  FunctionInfo* functionInfo = nullptr;
  optional<Context> definitionContext;
  bool wasChecked = false;
  bool external = false;
  bool isVirtual = false;
  bool isDefault = false;
  bool isVariadicParams = false;
  bool wasUsed = false;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual void codegenInstance(Buffer*, Sections*, FunctionInfo*) const;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD JustError<ErrorLoc> setFunctionSignature(const Context&, Concept* concept = nullptr, bool builtInImport = false);
  void handlePointerParamsInOperator(Buffer*, Sections*, const StatementBlock*) const;
  void handlePointerReturnInOperator(Buffer*, Sections*, const StatementBlock*) const;
  void addStacktraceGenerator(Buffer*, Sections*, const StatementBlock*) const;
  NODISCARD JustError<ErrorLoc> generateVirtualDispatchBody(Context& bodyContext);
  WithErrorLine<unique_ptr<Expression>> getVirtualFunctionCallExpr(const Context&, const string& funName,
      const string& alternativeName, Type* alternativeType, int virtualIndex, bool lvalueParam);
  WithErrorLine<unique_ptr<Expression>> getVirtualOperatorCallExpr(Context&, Operator,
      const string& alternativeName, Type* alternativeType, int virtualIndex, int lvalueParam);
  NODISCARD JustError<ErrorLoc> checkAndGenerateCopyFunction(const Context&, const string&);
  NODISCARD JustError<ErrorLoc> checkAndGenerateDefaultConstructor(const Context&);
  NODISCARD JustError<ErrorLoc> addInstance(const Context& callContext, FunctionInfo*);
  NODISCARD JustError<ErrorLoc> generateDefaultBodies(Context&);
  NODISCARD JustError<ErrorLoc> checkBody(const Context& callContext, StatementBlock& myBody,
      const FunctionInfo& instanceInfo, vector<unique_ptr<Statement> >& destructorCalls) const;
  void addParamsToContext(Context&, const FunctionInfo&) const;
  NODISCARD JustError<ErrorLoc> checkForIncompleteTypes(const FunctionInfo&, const Context&) const;
  WithErrorLine<Type*> getReturnType(const Context&) const;
  JustError<ErrorLoc> handleIsMemberParamsFunction();
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  bool isTopLevel = false;
  vector<Context::SubstitutionInfo> replacements;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool hasReturnStatement() const override;
};

struct MixinStatement : Statement {
  MixinStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> value;
  unique_ptr<Statement> result;
  bool returns = false;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
};

struct StaticStatement : Statement {
  StaticStatement(CodeLoc, unique_ptr<Statement>);
  unique_ptr<Statement> value;
  vector<unique_ptr<Statement>> results;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual bool canHaveAttributes() const override;
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, CodeLoc endLoc, string path, bool isBuiltIn);
  string path;
  optional<string> absolutePath;
  AST* ast = nullptr;
  CodeLoc endLoc;
  const bool isBuiltIn;
  virtual JustError<ErrorLoc> addToContext(Context&, ImportCache& cache, const Context& primaryContext) override;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*, ASTCache&,
      const vector<string>& importDirs) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ModuleInfo {
  string path;
  bool builtIn;
};

extern WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST&, Context& context, const Context& primary, ImportCache&, bool builtIn);
extern Context createPrimaryContext(TypeRegistry*, LanguageIndex*);
extern WithErrorLine<FunctionInfo*> getCopyFunction(const Context&, CodeLoc callLoc, Type*);
extern WithErrorLine<FunctionInfo*> getImplicitCopyFunction(const Context&, CodeLoc callLoc, Type*);
extern WithError<vector<FunctionInfo*> > getRequiredFunctionsForConceptType(const Context& context,
    const Concept& concept, CodeLoc codeLoc);
