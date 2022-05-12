#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"
#include "code_loc.h"
#include "type_registry.h"
#include "move_checker.h"
#include "import_cache.h"
#include "ast_cache.h"
#include "language_index.h"

Node::Node(CodeLoc l) : codeLoc(l) {}

unique_ptr<Expression> Expression::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* expr) { return expr->replaceVar(from, to); },
      [&](Expression* expr) { return expr->replaceVar(from, to); });
}

unique_ptr<Statement> identityStmt(Statement* expr) {
  return expr->transform(&identityStmt, &identityExpr);
}

unique_ptr<Expression> identityExpr(Expression* expr) {
  return expr->transform(&identityStmt, &identityExpr);
}

unique_ptr<Expression> Expression::deepCopy() const {
  return transform(&identityStmt, &identityExpr);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<VariableDeclaration> d, unique_ptr<Expression> c,
    unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), declaration(std::move(d)), condition(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, Type* v) : Expression(l), value(v) {
}

Variable::Variable(IdentifierInfo s) : Expression(s.codeLoc), identifier(std::move(s)) {
}

FunctionCall::FunctionCall(IdentifierInfo id, bool methodCall) : Expression(id.codeLoc), identifier(std::move(id)),
    methodCall(methodCall), variadicTemplateArgs(identifier.parts.back().variadic) {
}

FunctionCall::FunctionCall(IdentifierInfo id, unique_ptr<Expression> arg, bool methodCall)
    : FunctionCall(id, methodCall) {
  arguments.push_back(std::move(arg));
}

VariableDeclaration::VariableDeclaration(CodeLoc l, optional<IdentifierInfo> t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  string type = "auto";
  if (t)
    type = t->prettyString();
}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, FunctionId name)
  : Statement(l), returnType(std::move(r)), name(name) {}

WithErrorLine<Type*> Constant::getTypeImpl(const Context&) {
  return value->getType();
}

WithEvalError<EvalResult> Constant::eval(const Context&) const {
  return EvalResult{ value, true};
}

unique_ptr<Expression> Constant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<Constant>(codeLoc, value);
}

WithErrorLine<Type*> Variable::getTypeImpl(const Context& context) {
  optional<string> varError;
  if (auto id = identifier.asBasicIdentifier()) {
    if (auto newId = context.getShadowId(*id)) {
      identifier.parts[0].name = *newId;
      *id = *newId;
    }
    if (auto varType = context.getTypeOfVariable(*id, codeLoc))
      return *varType;
    else
      varError = varType.get_error();
  }
  if (auto t = getConstantValue(context))
    return t->getType();
  return codeLoc.getError(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

WithEvalError<EvalResult> Variable::eval(const Context& context) const {
  if (auto res = getConstantValue(context))
    return EvalResult { res, true};
  return EvalError::noEval();
}

unique_ptr<Expression> Variable::replaceVar(string from, string to) const {
  if (identifier.asBasicIdentifier() == from)
    return make_unique<Variable>(IdentifierInfo(to, codeLoc));
  else
    return deepCopy();
}

unique_ptr<Expression> Variable::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return make_unique<Variable>(identifier);
}

JustError<ErrorLoc> Variable::checkMoves(MoveChecker& checker) const {
  if (auto id = identifier.asBasicIdentifier())
    TRY(checker.getUsageError(*id).addCodeLoc(codeLoc));
  return success;
}

Type* Variable::getConstantValue(const Context& context) const {
  if (auto id = identifier.asBasicIdentifier())
    if (!!context.getTypeOfVariable(*id, codeLoc))
      return nullptr;
  if (auto t = context.getTypeFromString(identifier))
    return t.get();
  if (auto id = identifier.asBasicIdentifier()) {
    auto overloads = context.getFunctions(*id, false);
    if (!overloads.empty())
      return CompileTimeValue::get(FunctionType::get(*id, std::move(overloads)));
  }
  return nullptr;
}

template <typename Comp>
static bool exactArgs(const vector<Type*>& argTypes, const FunctionSignature& f, Comp comp) {
  if (f.params.size() != argTypes.size() || !f.templateParams.empty())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!comp(argTypes[i], f.params[i]))
      return false;
  return true;
}

template <typename Comp>
static bool exactFirstArg(const vector<Type*>& argTypes, const FunctionSignature& overload, Comp comp) {
  return !argTypes.empty() && !overload.params.empty() && comp(argTypes[0], overload.params[0]);
}

static bool fromConcept(const vector<Type*>&, FunctionInfo* f) {
  return !!f->type.concept;
}

static bool userDefinedConstructor(const vector<Type*>&, FunctionInfo* f) {
  return !f->type.generatedConstructor;
}

static void filterOverloads(const Context& context, vector<FunctionInfo*>& overloads, const vector<Type*>& argTypes) {
  auto filter = [&] (auto fun, const char* method) {
    vector<FunctionInfo*> better;
    for (auto& overload : overloads)
      if (fun(argTypes, overload))
        better.push_back(overload);
    if (!better.empty())
      overloads = better;
  };
  auto isExactArg = [] (Type* arg, Type* param) {
    return param == arg;
  };
  auto isExactValueArg = [] (Type* arg, Type* param) {
    return param == arg->removeReference();
  };
  auto isExactReferenceArg = [] (Type* arg, Type* param) {
    bool byConstRef = param->asReferenceType() &&
        !arg->asMutableReferenceType() &&
        param->removeReference() == arg->removeReference();
    return byConstRef;
  };
  auto isConstToMutableReferenceArg = [] (Type* arg, Type* param) {
    bool byConstRef = param->asReferenceType() &&
        arg->asMutableReferenceType() &&
        param->removeReference() == arg->removeReference();
    return byConstRef;
  };
  auto isSpecialized = [&] (const auto& args, auto& overload) {
    for (auto& other : overloads)
      if (other != overload && context.isGeneralizationWithoutReturnType(
          overload->getParent()->getWithoutRequirements(), other->getParent()->getWithoutRequirements()))
        return false;
    return true;
  };
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactArg);}, "all args exact");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactArg);}, "first arg exact");
  filter([&](const auto& args, const auto& overload) {
      return !overload->type.params.empty() && overload->getParent()->type.params[0] == overload->type.params[0];}, "non-template first");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactValueArg);}, "all args exact value");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactValueArg);}, "first arg exact value");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactReferenceArg);}, "all args exact reference");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactReferenceArg);}, "first arg exact reference");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isConstToMutableReferenceArg);}, "all args const to mut ref");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isConstToMutableReferenceArg);}, "first arg const to mut fef");
  // filter out functions that have concept type parameters or return value
  filter([](const vector<Type*>&, FunctionInfo* f){ return !f->isConceptTypeFunction(); }, "concept type");
  // sometimes a function is both in the global context and in the concept, so prefer the one in the concept
  filter(&fromConcept, "non concept");
  filter(&userDefinedConstructor, "user defined constructor");
  // try to choose a more specialized template, eg. f<T>(X<T>) instead of f<T>(T).
  filter(isSpecialized, "specialized");
  filter([&](const auto& args, const auto& overload) { return !overload->type.requirements.empty();}, "with requirement");
}


static WithErrorLine<FunctionInfo*> handleOperatorOverloads(const Context& context, CodeLoc codeLoc, Operator op,
    vector<Type*> types, vector<CodeLoc> argLocs, vector<unique_ptr<Expression>>& expr) {
  vector<FunctionInfo*> overloads;
  if (auto fun = context.getBuiltinOperator(op, types))
    overloads.push_back(fun);
  vector<string> errors;
  for (auto fun : context.getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, types, argLocs))
      overloads.push_back(inst.get());
    else
      errors.push_back("Candidate: " + fun->prettyString() + ": " + inst.get_error().error);
  filterOverloads(context, overloads, types);
  if (overloads.size() == 1) {
    generateConversions(context, overloads[0]->type.params, types, expr);
    return overloads[0];
  } else {
    string error = (overloads.empty() ? "No overload" : "Multiple overloads") + " found for operator: "s +
        quote(getString(op)) + " with argument types: " + joinTypeList(types);
    for (auto& f : overloads)
      error += "\nCandidate: " + f->prettyString();
    if (overloads.empty())
      for (auto& f : errors)
        error += "\n" + f;
    return codeLoc.getError(error);
  }
}

static WithErrorLine<FunctionInfo*> getFunction(const Context&,
    CodeLoc, IdentifierInfo, vector<Type*> templateArgs, const vector<Type*>& argTypes,
    const vector<CodeLoc>&, bool compileTimeArgs);

static WithErrorLine<FunctionInfo*> getFunction(const Context&,
    CodeLoc, IdentifierInfo, vector<Type*> templateArgs, const vector<Type*>& argTypes,
    const vector<CodeLoc>&, vector<unique_ptr<Expression>>&, bool compileTimeArgs);

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr) {
  return make_unique<BinaryExpression>(Private{}, loc, op, std::move(expr));
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, unique_ptr<Expression> a,
    unique_ptr<Expression> b) {
  return get(loc, op, makeVec<unique_ptr<Expression>>(std::move(a), std::move(b)));
}

BinaryExpression::BinaryExpression(BinaryExpression::Private, CodeLoc loc, Operator op,
    vector<unique_ptr<Expression>> expr) : Expression(loc), op(op), expr(std::move(expr)) {}

static vector<unique_ptr<Expression>> transformValueOrArg(vector<unique_ptr<Expression>> expr, CodeLoc codeLoc,
    bool usePointer) {
  auto block = make_unique<StatementBlock>(codeLoc);
  if (usePointer)
    expr[1] = make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr[1]));
  block->elems.push_back(make_unique<ReturnStatement>(codeLoc, std::move(expr[1])));
  expr[1] = make_unique<LambdaExpression>(codeLoc, vector<FunctionParameter>(), std::move(block), none,
      LambdaCaptureInfo{{}, {}, LambdaCaptureType::REFERENCE });
  return expr;
}

static unique_ptr<Expression> getDestructAndGetCall(CodeLoc codeLoc, unique_ptr<Expression> expr) {
  return make_unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
      (make_unique<FunctionCall>(IdentifierInfo("destruct_and_get", codeLoc),
          make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false)));
}

static unique_ptr<Expression> getDestructorCall(CodeLoc codeLoc, unique_ptr<Expression> expr) {
  return make_unique<FunctionCall>(IdentifierInfo("destruct", codeLoc),
      make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false);
}

static unique_ptr<Statement> getDestructorStatement(CodeLoc codeLoc, const string& identifier) {
  return make_unique<ExpressionStatement>(getDestructorCall(codeLoc, make_unique<Variable>(IdentifierInfo(identifier, codeLoc))));
}

WithErrorLine<Type*> BinaryExpression::getTypeImpl(const Context& context) {
  if (!functionInfo && op == Operator::VALUE_OR) {
    // We have to use a copy to check type of rhs, otherwise the implicit lambda might not be checked
    // again and implicit captures won't be extracted
    auto tmpExpr = expr[1]->deepCopy();
    auto type = TRY(getType(context, tmpExpr));
    expr = transformValueOrArg(std::move(expr), codeLoc,
        !!dynamic_cast<ReferenceType*>(type) || !!dynamic_cast<MutableReferenceType*>(type));
  }
  vector<Type*> exprTypes;
  for (auto& elem : expr)
    exprTypes.push_back(TRY(getType(context, elem)));
  if (op == Operator::SUBSCRIPT && dynamic_cast<VariablePack*>(exprTypes[0]) && !expr[1]->eval(context))
    return expr[1]->codeLoc.getError("Unable to evaluate variable pack index at compile-time");
  functionInfo = TRY(handleOperatorOverloads(context, codeLoc, op, exprTypes,
      ::transform(expr, [&](auto& e) { return e->codeLoc;}), expr));
  if (op == Operator::ASSIGNMENT && exprTypes[0]->removeReference()->hasDestructor()) {
    expr[0] = getDestructAndGetCall(codeLoc, std::move(expr[0]));
    CHECK(!!getType(context, expr[0]));
  }
  TRY(functionInfo->addInstance(context));
  TRY(considerDestructorCall(context, 0, exprTypes[0]));
  TRY(considerDestructorCall(context, 1, exprTypes[1]));
  return functionInfo->type.retVal;
}

WithErrorLine<FunctionInfo*> getDestructor(const Context& context, Type* type, CodeLoc codeLoc) {
  return getFunction(context, codeLoc, IdentifierInfo("destruct"s, codeLoc), {},
      {PointerType::get(type)}, {codeLoc}, false);
}

JustError<ErrorLoc> BinaryExpression::considerDestructorCall(const Context& context, int index, Type* argType) {
  if (argType->hasDestructor() && !argType->isReference() && dynamic_cast<ReferenceType*>(functionInfo->type.params[index])) {
    destructorCall[index] = TRY(getDestructor(context, argType, codeLoc));
    TRY(destructorCall[index]->addInstance(context));
  }
  return success;
}

WithEvalError<EvalResult> BinaryExpression::eval(const Context& context) const {
  auto value1 = TRY(expr[0]->eval(context));
  auto value2 = TRY(expr[1]->eval(context));
  return EvalResult{TRY(::eval(op, {value1.value, value2.value})), value1.isConstant && value2.isConstant};
}

unique_ptr<Expression> BinaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return get(codeLoc, op, fun(expr[0].get()), fun(expr[1].get()));
}

JustError<ErrorLoc> BinaryExpression::checkMoves(MoveChecker& checker) const {
  for (auto& e : expr)
    TRY(e->checkMoves(checker));
  return success;
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

WithErrorLine<Type*> UnaryExpression::getTypeImpl(const Context& context) {
  Type* ret = nullptr;
  auto right = TRY(getType(context, expr));
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right->getName())};
  auto exprTmp = makeVec(std::move(expr));
  functionInfo = TRY(handleOperatorOverloads(context, codeLoc, op, {TRY(exprTmp[0]->getTypeImpl(context))}, {exprTmp[0]->codeLoc}, exprTmp));
  expr = std::move(exprTmp[0]);
  TRY(functionInfo->addInstance(context));
  if (right->hasDestructor() && !right->isReference() &&
      (dynamic_cast<ReferenceType*>(functionInfo->type.params[0]) || op == Operator::GET_ADDRESS)) {
    destructorCall = TRY(getDestructor(context, right, codeLoc));
    TRY(destructorCall->addInstance(context));
  }
  return functionInfo->type.retVal;
}

WithEvalError<EvalResult> UnaryExpression::eval(const Context& context) const {
  auto value = TRY(expr->eval(context));
  return EvalResult{TRY(::eval(op, {value.value})), value.isConstant};
}

unique_ptr<Expression> UnaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return make_unique<UnaryExpression>(codeLoc, op, fun(expr.get()));
}

JustError<ErrorLoc> UnaryExpression::checkMoves(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

JustError<ErrorLoc> StatementBlock::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  for (auto& s : elems)
    TRY(s->check(bodyContext));
  return success;
}

WithEvalError<StatementEvalResult> StatementBlock::eval(Context& context) {
  StatementEvalResult res;
  auto bodyContext = context.getChild();
  for (auto& elem : elems)
    res.append(TRY(elem->eval(bodyContext)));
  return std::move(res);
}

JustError<ErrorLoc> StatementBlock::checkMovesImpl(MoveChecker& checker) const {
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& elem : elems)
    TRY(elem->checkMoves(checker));
  return success;
}

unique_ptr<Statement> StatementBlock::transformImpl(const StmtTransformFun& fun, const ExprTransformFun&) const {
  auto ret = make_unique<StatementBlock>(codeLoc);
  for (auto& elem : elems)
    ret->elems.push_back(fun(elem.get()));
  return ret;
}

JustError<ErrorLoc> IfStatement::check(Context& context, bool) {
  auto ifContext = context.getChild();
  if (declaration)
    TRY(declaration->check(ifContext));
  auto negate = [&] (unique_ptr<Expression> expr) {
    auto codeLoc = expr->codeLoc;
    return make_unique<UnaryExpression>(codeLoc, Operator::LOGICAL_NOT, std::move(expr));
  };
  if (!condition)
    condition = negate(negate(make_unique<Variable>(IdentifierInfo(declaration->identifier, declaration->codeLoc))));
  auto condType = TRY(getType(ifContext, condition));
  if (!ifContext.canConvert(condType, BuiltinType::BOOL)) {
    condition = negate(negate(std::move(condition)));
    condType = TRY(getType(ifContext, condition));
  }
  if (!ifContext.canConvert(condType, BuiltinType::BOOL)) {
    return codeLoc.getError(
        "Expected a type convertible to bool or with overloaded operator " +
        quote("!") + " inside if statement, got " + quote(condType->getName()));
  }
  auto trueContext = ifContext.getChild();
  trueContext.setIsInBranch();
  TRY(ifTrue->check(trueContext));
  if (ifFalse) {
    auto falseContext = ifContext.getChild();
    falseContext.setIsInBranch();
    TRY(ifFalse->check(falseContext));
  }
  return success;
}

JustError<ErrorLoc> IfStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.startBlock();
  OnExit onExit1([&]{ checker.endBlock();});
  if (declaration)
    TRY(declaration->checkMoves(checker));
  if (condition)
    TRY(condition->checkMoves(checker));
  checker.startBlock();
  OnExit onExit2([&]{ checker.endBlock();});
  checker.newAlternative();
  TRY(ifTrue->checkMoves(checker));
  if (ifFalse) {
    checker.newAlternative();
    TRY(ifFalse->checkMoves(checker));
  }
  return success;
}

unique_ptr<Statement> IfStatement::transformImpl(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return make_unique<IfStatement>(codeLoc,
      declaration ? cast<VariableDeclaration>(fun(declaration.get())) : nullptr,
      condition ? exprFun(condition.get()) : nullptr,
      fun(ifTrue.get()),
      ifFalse ? fun(ifFalse.get()) : nullptr);
}

WithEvalError<StatementEvalResult> IfStatement::eval(Context& context) {
  if (declaration)
    return EvalError::withError(getString(Keyword::STATIC) + " if with a declaration is not supported.");
  StatementEvalResult res;
  auto ifContext = context.getChild();
  auto value1 = TRY(condition->eval(ifContext)).value;
  if (value1->getType() != BuiltinType::BOOL)
    return EvalError::withError("Expected a compile-time value of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(value1->getName()));
  auto value = dynamic_cast<CompileTimeValue*>(value1);
  if (auto b = value->value.getValueMaybe<bool>()) {
    if (*b)
      res.push_back(ifTrue->deepCopy());
    else if (ifFalse)
      res.push_back(ifFalse->deepCopy());
  } else {
    res.push_back(ifTrue->deepCopy());
    if (ifFalse)
      res.push_back(ifFalse->deepCopy());
  }
  for (auto& elem : res)
    if (auto res = elem->check(ifContext); !res)
      return EvalError::withError(res.get_error().toString());
  return std::move(res);
}

static JustError<string> getVariableInitializationError(const char* action, const Context& context, Type* varType,
    Type* exprType, unique_ptr<Expression>& expr) {
  if (auto res = context.canConvert(exprType, varType, expr); !res)
    return "Can't "s + action + " of type "
       + quote(varType->getName()) + " using a value of type " + quote(exprType->getName()) + ".\n" + res.get_error();
  return success;
}

WithErrorLine<Type*> VariableDeclaration::getRealType(const Context& context) const {
  if (type)
    return context.getTypeFromString(*type);
  else
  if (initExpr)
    return TRY(initExpr->getTypeImpl(context))->removeReference();
  else
    return codeLoc.getError("Initializing expression needed to infer variable type");
}

WithErrorLine<bool> VariableDeclaration::considerShadowing() {
  if (attributes.empty())
    return false;
  for (auto& a : attributes)
    if (a.name != "@shadow")
      return a.codeLoc.getError("Unrecognized attribute: " + quote(a.name));
  static int cnt = 0;
  identifier += "shadow" + to_string(++cnt);
  return true;
}

JustError<ErrorLoc> VariableDeclaration::check(Context& context, bool) {
  string oldId = identifier;
  bool shadow = TRY(considerShadowing());
  if (!shadow)
    TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  if (!realType)
    realType = TRY(getRealType(context));
  if (!realType->canDeclareVariable())
    return codeLoc.getError("Can't declare variable of type " + quote(realType->getName()));
  TRY(realType->getSizeError(context).addCodeLoc(codeLoc));
  if (!initExpr)
    return codeLoc.getError("Variable requires initialization");
  auto exprType = TRY(getType(context, initExpr));
  TRY(getVariableInitializationError("initialize variable", context, realType, exprType, initExpr).addCodeLoc(initExpr->codeLoc));
  TRY(getType(context, initExpr));
  auto varType = isMutable ? (Type*)MutableReferenceType::get(realType) : (Type*)ReferenceType::get(realType);
  context.addVariable(identifier, std::move(varType), codeLoc);
  if (shadow)
    context.setShadowId(oldId, identifier);
  if (realType->hasDestructor()) {
    destructorCall = getDestructorStatement(codeLoc, identifier);
    TRY(destructorCall->check(context));
  }
  return success;
}

JustError<ErrorLoc> VariableDeclaration::checkMovesImpl(MoveChecker& checker) const {
  if (initExpr)
    TRY(initExpr->checkMoves(checker));
  checker.addVariable(identifier);
  return success;
}

WithEvalError<StatementEvalResult> VariableDeclaration::eval(Context& context) {
  string oldId = identifier;
  bool shadow = TRY(considerShadowing().toEvalError());
  if (!shadow)
    if (auto res = context.checkNameConflictExcludingFunctions(identifier, "Variable"); !res)
      return EvalError::withError(res.get_error());
  auto actualType = TRY(getRealType(context).toEvalError());
  auto result = TRY(TRY(initExpr->eval(context)).value->convertTo(actualType).addCodeLoc(codeLoc).toEvalError());
  if (isMutable)
    result = CompileTimeValue::getReference(result);
  context.addType(identifier, result);
  context.setTypeFullyDefined(result);
  if (shadow)
    context.setShadowId(oldId, identifier);
  return StatementEvalResult{};
}

unique_ptr<Statement> VariableDeclaration::transformImpl(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = make_unique<VariableDeclaration>(codeLoc, none, identifier, initExpr ? exprFun(initExpr.get()) : nullptr);
  ret->isMutable = isMutable;
  ret->type = type;
  return ret;
}

AliasDeclaration::AliasDeclaration(CodeLoc l, string id, unique_ptr<Expression> ini)
    : Statement(l), identifier(id), initExpr(std::move(ini)) {
}

JustError<ErrorLoc> AliasDeclaration::check(Context& context, bool) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  realType = TRY(getType(context, initExpr));
  context.addVariable(identifier, realType->isReference() ? realType : ReferenceType::get(realType), codeLoc);
  if (realType->hasDestructor()) {
    destructorCall = getDestructorStatement(codeLoc, identifier);
    TRY(destructorCall->check(context));
  }
  return success;
}

JustError<ErrorLoc> AliasDeclaration::checkMovesImpl(MoveChecker& checker) const {
  TRY(initExpr->checkMoves(checker));
  return success;
}

unique_ptr<Statement> AliasDeclaration::transformImpl(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = make_unique<AliasDeclaration>(codeLoc, identifier, exprFun(initExpr.get()));
  return ret;
}

JustError<ErrorLoc> ReturnStatement::check(Context& context, bool) {
  auto returnType = TRY([&]() -> WithErrorLine<Type*> {
    if (expr)
      return getType(context, expr);
    else
      return (Type*)BuiltinType::VOID;
  }());
  TRY(context.getReturnTypeChecker()->addReturnStatement(context, returnType, expr).addCodeLoc(codeLoc));
  return success;
}

JustError<ErrorLoc> ReturnStatement::checkMovesImpl(MoveChecker& checker) const {
  if (expr)
    TRY(expr->checkMoves(checker));
  checker.returnStatement();
  return success;
}

unique_ptr<Statement> ReturnStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return make_unique<ReturnStatement>(codeLoc, expr ? fun(expr.get()) : nullptr);
}

JustError<ErrorLoc> Statement::addToContext(Context&) {
  return success;
}

JustError<ErrorLoc> Statement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  return addToContext(context);
}

JustError<ErrorLoc> Statement::checkMoves(MoveChecker& checker) const {
  checker.clearStatementUsages();
  auto ret = checkMovesImpl(checker);
  checker.clearStatementUsages();
  return ret;
}

unique_ptr<Statement> Statement::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* s) { return s->replaceVar(from, to); },
  [&](Expression* s) { return s->replaceVar(from, to); });
}

WithEvalError<StatementEvalResult> Statement::eval(Context& context) {
  return EvalError::noEval();
}

JustError<ErrorLoc> Statement::checkMovesImpl(MoveChecker&) const {
  return success;
}

unique_ptr<Statement> Statement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  auto ret = transformImpl(f1, f2);
  ret->attributes = attributes;
  return ret;
}

bool Statement::hasReturnStatement() const {
  return false;
}

unique_ptr<Statement> Statement::deepCopy() const {
  auto ret = transform(&identityStmt, &identityExpr);
  ret->exported = exported;
  return ret;
}

bool IfStatement::hasReturnStatement() const {
  return ifTrue->hasReturnStatement() && ifFalse && ifFalse->hasReturnStatement();
}

StatementBlock::StatementBlock(CodeLoc l, vector<unique_ptr<Statement>> e) : Statement(l), elems(std::move(e)) {
}

bool StatementBlock::hasReturnStatement() const {
  for (auto& s : elems)
    if (s->hasReturnStatement())
      return true;
  return false;
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc)
    : Statement(codeLoc), expr(make_unique<Variable>(IdentifierInfo("void_value", codeLoc))) {
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc, unique_ptr<Expression> expr)
    : Statement(codeLoc), expr(std::move(expr)) {}

bool ReturnStatement::hasReturnStatement() const {
  return true;
}

static WithErrorLine<vector<Type*>> translateConceptParams(const Context& context,
    const TemplateInfo::ConceptRequirement& requirement) {
  auto& reqId = requirement.identifier;
  auto& requirementArgs = reqId.parts[0].templateArguments;
  auto concept = context.getConcept(reqId.parts[0].name, reqId.codeLoc);
  vector<Type*> translatedParams;
  bool missingTypePack = requirement.variadic;
  for (int i = 0; i < requirementArgs.size(); ++i) {
    if (auto arg = requirementArgs[i].getReferenceMaybe<IdentifierInfo>()) {
      optional<bool> requireTypePack = reqId.parts[0].variadic && i == requirementArgs.size() - 1;
      // If requirement is variadic then we are ok with both type and type pack argument.
      // We check below that at least one type pack argument is present.
      if (requirement.variadic)
        requireTypePack = none;
      auto origParam = TRY(context.getTypeFromString(*arg, requireTypePack));
      if (auto p = context.getUnexpandedTypePack())
        if (p->second == origParam)
          missingTypePack = false;
      if (auto templateParam = dynamic_cast<TemplateParameterType*>(origParam)) {
        // Support is_enum concept
        auto& conceptParams = concept->getParams();
        if (conceptParams[min<int>(i, conceptParams.size() -1 )]->getType() != BuiltinType::ANY_TYPE)
          templateParam->type = conceptParams[min<int>(i, conceptParams.size() -1 )]->getType();
      }
      translatedParams.push_back(std::move(origParam));
    } else
      return reqId.codeLoc.getError("Expected a type argument");
  }
  if (missingTypePack)
    return requirement.identifier.codeLoc.getError("Variadic parameter requires at least one type pack argument");
  return translatedParams;
}

static WithErrorLine<TemplateRequirement> applyRequirement(Context& from,
    const TemplateInfo::ConceptRequirement& requirement, bool variadicRequiements) {
  auto& reqId = requirement.identifier;
  if (requirement.variadic && reqId.parts[0].variadic)
    return reqId.codeLoc.getError("Requirement can't be both variadic and involving a variadic concept");
  if (auto concept = from.getConcept(reqId.parts[0].name, reqId.codeLoc)) {
    if (!concept->isVariadic() && reqId.parts[0].variadic)
      return reqId.codeLoc.getError("Concept " + quote(concept->getName()) + " is not variadic");
    auto& requirementArgs = reqId.parts[0].templateArguments;
    if ((!concept->isVariadic() && requirementArgs.size() != concept->getParams().size()) ||
        (concept->isVariadic() && requirementArgs.size() < concept->getParams().size() - 1))
      return reqId.codeLoc.getError(
          "Wrong number of template arguments to concept " + quote(concept->getName()) + ": " + quote(reqId.parts[0].toString()));
    ErrorBuffer errors;
    auto translated = concept->translate(TRY(translateConceptParams(from, requirement)), variadicRequiements, errors);
    if (!errors.empty())
      return reqId.codeLoc.getError(errors[0]);
    from.merge(translated->getContext());
    return TemplateRequirement(std::move(translated), requirement.variadic);
  } else
    return reqId.codeLoc.getError("Unknown concept: " + reqId.parts[0].name);
}

static WithErrorLine<TemplateRequirement> applyRequirement(Context& from,
    const shared_ptr<Expression>& expr1, bool variadicRequirements) {
  auto expr = expr1->deepCopy();
  auto tmpExpr = expr->deepCopy();
  TRY(getType(from, tmpExpr));
  auto value = TRY(expr->eval(from).addNoEvalError(
      expr1->codeLoc.getError("Unable to evaluate expression at compile-time")));
  if (value.value->getType() != BuiltinType::BOOL)
    return expr->codeLoc.getError("Expected expression of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(value.value->getType()->getName()));
  return TemplateRequirement(shared_ptr<Expression>(std::move(expr)), false);
}

NODISCARD static WithErrorLine<vector<TemplateRequirement>> applyRequirements(Context& from,
    const TemplateInfo& requirements) {
  vector<TemplateRequirement> ret;
  for (auto& req : requirements.requirements)
    ret.push_back(TRY(req.visit([&](auto& req) {
      return applyRequirement(from, req, requirements.variadic);
    })));
  return ret;
}

static bool paramsAreGoodForOperator(const vector<Type*>& params) {
  for (auto& p : params)
    if (p->removeReference()->getType() == BuiltinType::STRUCT_TYPE ||
        p->removeReference()->getType() == BuiltinType::UNION_TYPE)
      return true;
  return false;
}

WithErrorLine<Type*> FunctionDefinition::getReturnType(const Context& context) const {
  if (returnType.asBasicIdentifier() == "noreturn"s)
    return (Type*) BuiltinType::NORETURN;
  else
    return context.getTypeFromString(this->returnType);
}

static WithErrorLine<vector<Type*>> getTemplateParams(const TemplateInfo& info, const Context& context) {
  vector<Type*> ret;
  auto paramsContext = context.getChild();
  for (auto& param : info.params) {
    if (param.type) {
      if (auto type = paramsContext.getType(*param.type)) {
        if (!type->canBeValueTemplateParam())
          return param.codeLoc.getError("Value template parameter cannot have type " + quote(*param.type));
        ret.push_back(CompileTimeValue::getTemplateValue(type, param.name));
      } else
        return param.codeLoc.getError("Type not found: " + quote(*param.type));
    } else {
      TRY(paramsContext.checkNameConflict(param.name, "template parameter").addCodeLoc(param.codeLoc));
      ret.push_back(new TemplateParameterType(param.name, param.codeLoc));
      paramsContext.addType(param.name, ret.back());
    }
  }
  return ret;
}

unique_ptr<Statement> FunctionDefinition::transformImpl(const StmtTransformFun& f1, const ExprTransformFun&) const {
  auto ret = make_unique<FunctionDefinition>(codeLoc, returnType, name);
  ret->parameters = parameters;
  if (body)
    ret->body = cast<StatementBlock>(f1(body.get()));
  ret->templateInfo = templateInfo;
  ret->external = external;
  ret->isVirtual = isVirtual;
  ret->isDefault = isDefault;
  ret->isVariadicParams = isVariadicParams;
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::setFunctionSignature(const Context& context, Concept* concept,
    bool builtInImport) {
  if (functionInfo)
    return success;
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (auto s = name.getReferenceMaybe<string>())
    TRY(context.checkNameConflictExcludingFunctions(*s, "Function").addCodeLoc(codeLoc));
  else if (auto op = name.getValueMaybe<Operator>()) {
    if (!canOverload(*op, int(parameters.size())))
      return codeLoc.getError("Can't overload operator " + quote(getString(*op)) +
          " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = context.getChild();
  auto templateTypes = TRY(getTemplateParams(templateInfo, context));
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    contextWithTemplateParams.addType(param.name, templateTypes[i]);
    if (i == templateInfo.params.size() - 1 && templateInfo.variadic)
      contextWithTemplateParams.addUnexpandedTypePack(param.name, templateTypes[i]);
  }
  auto requirements = TRY(applyRequirements(contextWithTemplateParams, templateInfo));
  definitionContext.emplace(contextWithTemplateParams.getChild());
  auto returnType = TRY(getReturnType(contextWithTemplateParams));
  if (name.contains<Operator>())
    returnType = convertPointerToReference(returnType);
  vector<Type*> params;
  set<string> paramNames;
  for (int i = 0; i < parameters.size(); ++i) {
    auto& p = parameters[i];
    auto type = TRY(contextWithTemplateParams.getTypeFromString(p.type, isVariadicParams && i == parameters.size() - 1));
    if (type == BuiltinType::VOID)
      return p.codeLoc.getError("Function parameter may not have " + quote(type->getName()) + " type");
    if (name.contains<Operator>())
      type = convertPointerToReference(type);
    params.push_back(std::move(type));
    if (p.name) {
      if (paramNames.count(*p.name))
        return p.codeLoc.getError("Duplicate function parameter name: " + quote(*p.name));
      paramNames.insert(*p.name);
    }
  }
  if (!builtInImport && !concept && name.contains<Operator>() && !paramsAreGoodForOperator(params))
    return codeLoc.getError("Operator parameters must include at least one user-defined type");
  FunctionSignature functionType(returnType, params, templateTypes);
  functionType.concept = concept;
  functionType.requirements = requirements;
  functionType.variadicTemplate = templateInfo.variadic;
  functionType.variadicParams = isVariadicParams;
  if (name.contains<ConstructorTag>() && external)
    functionType.generatedConstructor = true;
  if (external)
    functionType.setBuiltin();
  CHECK(!functionInfo);
  functionInfo = FunctionInfo::getDefined(name, std::move(functionType), this);
  if (functionInfo->isMainFunction()) {
    if (!functionInfo->type.params.empty()) {
      auto expectedParam = context.getSliceType(BuiltinType::STRING);
      if (functionInfo->type.params.size() > 1 || functionInfo->type.params[0] != expectedParam)
        return codeLoc.getError("The main() function should take no arguments or take a single argument of type "
            + quote(expectedParam->getName()));
    }
    if (functionInfo->type.retVal != BuiltinType::INT && functionInfo->type.retVal != BuiltinType::VOID)
      return codeLoc.getError("The main() function should return a value of type " +
          quote(BuiltinType::INT->getName()) + " or " + quote(BuiltinType::VOID->getName()));
  }
  return success;
}

static FunctionId translateDestructorId(const FunctionId& id) {
  if (id == "destruct"s)
    return "destruct_full"s;
  if (id == "destruct_impl_dont_call"s)
    return "destruct"s;
  return id;
}

static IdentifierInfo translateDestructorId(IdentifierInfo id) {
  if (id.parts[0].name == "destruct")
    id.parts[0].name = "destruct_full";
  if (id.parts[0].name == "destruct_impl_dont_call")
    id.parts[0].name = "destruct";
  return id;
}


static WithError<FunctionInfo*> getFunction(const Context& context,
    CodeLoc codeLoc, FunctionId id, vector<FunctionInfo*> candidates,
    vector<Type*> templateArgs, const vector<Type*>& argTypes,
    const vector<CodeLoc>& argLoc) {
  vector<FunctionInfo*> overloads;
  string errors;
  for (auto& overload : candidates) {
    if (auto f = instantiateFunction(context, overload, codeLoc, templateArgs, argTypes, argLoc)) {
      overloads.push_back(f.get());
    } else
      errors += "\nCandidate: "s + overload->prettyString() + ": " + f.get_error().error;
  }
  if (id == "destruct"s) {
    if (!argTypes.empty() && dynamic_cast<PointerType*>(argTypes[0]->removeReference())) {
      if (auto s = dynamic_cast<ConceptType*>(argTypes[0]->removePointer())) {
        auto funs = s->getConceptFor(argTypes[0]->removePointer())->getContext().getFunctions("destruct"s, false);
        if (funs.empty())
          return "Concept type " + quote(s->getName()) + " has no destruct function defined.";
        return funs[0];
      }
      if (auto s = dynamic_cast<StructType*>(argTypes[0]->removePointer()))
        if (s->destructor) {
          CHECK(templateArgs.empty());
          CHECK(s->destructor->type.params[0] == PointerType::get(s));
          return TRY(instantiateFunction(context, s->destructor->getParent(), codeLoc, {}, s->destructor->type.params,
              {codeLoc}).withoutCodeLoc());
        }
      if (auto s = dynamic_cast<LambdaType*>(argTypes[0]->removePointer()))
        if (s->destructor)
          return FunctionInfo::getImplicit("destruct"s, FunctionSignature(BuiltinType::VOID, {PointerType::get(s)}, {}));
    }
  } else
  if (id == "invoke"s && !argTypes.empty() && argTypes[0]->isPointer())
    if (auto lambda = dynamic_cast<LambdaType*>(argTypes[0]->removePointer())) {
      if (auto f = instantiateFunction(context, lambda->functionInfo, codeLoc, templateArgs, argTypes, argLoc)) {
        if (!contains(overloads, *f))
          overloads.push_back(*f);
      } else
        errors += "\nCandidate: "s + lambda->functionInfo->prettyString() + ": " + f.get_error().error;
    }
  if (overloads.empty())
    return errors;
  filterOverloads(context, overloads, argTypes);
  CHECK(!overloads.empty());
  if (overloads.size() == 1)
    return overloads[0];
  else
    return "Multiple function overloads found:\n" +
        combine(transform(overloads, [](const auto& o) { return o->prettyString();}), "\n");
}

static WithErrorLine<FunctionInfo*> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierInfo id, vector<Type*> templateArgs, const vector<Type*>& argTypes,
    const vector<CodeLoc>& argLoc, bool compileTimeArgs) {
  auto candidates = TRY(context.getFunctionTemplate(translateDestructorId(id), compileTimeArgs).addCodeLoc(codeLoc));
  auto error =  codeLoc.getError("Couldn't find function " + id.prettyString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  if (candidates.empty())
    return error;
  auto functionId = candidates[0]->id;
  if (auto res = getFunction(context, codeLoc, functionId, std::move(candidates), std::move(templateArgs),
      argTypes, argLoc))
    return *res;
  else {
    error.error += res.get_error();
    return error;
  }
}

static WithErrorLine<FunctionInfo*> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierInfo id, vector<Type*> templateArgs, const vector<Type*>& argTypes,
    const vector<CodeLoc>& argLoc, vector<unique_ptr<Expression>>& expr, bool compileTimeArgs) {
  auto fun = TRY(getFunction(context, codeLoc, std::move(id), std::move(templateArgs), std::move(argTypes),
      std::move(argLoc), compileTimeArgs));
  generateConversions(context, fun->type.params, argTypes, expr);
  return fun;
}

WithErrorLine<FunctionInfo*> getCopyFunction(const Context& context, CodeLoc callLoc, Type* t) {
  return getFunction(context, callLoc, IdentifierInfo("copy", callLoc), {}, {PointerType::get(t)}, {callLoc}, false);
}

WithErrorLine<FunctionInfo*> getImplicitCopyFunction(const Context& context, CodeLoc callLoc, Type* t) {
  return getFunction(context, callLoc, IdentifierInfo("implicit_copy", callLoc), {}, {PointerType::get(t)}, {callLoc},
      false);
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualFunctionCallExpr(const Context& context,
    const string& funName, const string& alternativeName, Type* alternativeType, int virtualIndex,
    bool lvalueParam) {
  auto functionCall = make_unique<FunctionCall>(IdentifierInfo(funName, codeLoc), false);
  vector<Type*> args;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      functionCall->arguments.push_back(make_unique<MoveExpression>(codeLoc, *parameters[i].name, codeLoc));
      args.push_back(functionInfo->type.params[i]);
    } else {
      if (lvalueParam) {
        functionCall->arguments.push_back(make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            make_unique<Variable>(IdentifierInfo(alternativeName, codeLoc))));
        args.push_back(convertReferenceToPointer(alternativeType));
      } else {
        functionCall->arguments.push_back(make_unique<MoveExpression>(codeLoc, alternativeName, codeLoc));
        args.push_back(alternativeType);
      }
    }
  TRY(getFunction(context, codeLoc, IdentifierInfo(funName, codeLoc), {}, args,
      vector<CodeLoc>(args.size(), codeLoc), false));
  return unique_ptr<Expression>(std::move(functionCall));
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualOperatorCallExpr(Context& context,
    Operator op, const string& alternativeName, Type* alternativeType, int virtualIndex, int lvalueParam) {
  vector<unique_ptr<Expression>> arguments;
  vector<Type*> argTypes;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      arguments.push_back(make_unique<MoveExpression>(codeLoc, *parameters[i].name, codeLoc));
      argTypes.push_back(functionInfo->type.params[i]);
    } else {
      if (lvalueParam) {
        arguments.push_back(make_unique<Variable>(IdentifierInfo(alternativeName, codeLoc)));
      } else
        arguments.push_back(make_unique<MoveExpression>(codeLoc, alternativeName, codeLoc));
      argTypes.push_back(alternativeType);
    }
  TRY(handleOperatorOverloads(context, codeLoc, op, argTypes, vector<CodeLoc>(argTypes.size(), codeLoc), arguments));
  if (parameters.size() == 1)
    return unique_ptr<Expression>(make_unique<UnaryExpression>(codeLoc, op, std::move(arguments[0])));
  else {
    CHECK(parameters.size() == 2);
    return unique_ptr<Expression>(BinaryExpression::get(codeLoc, op, std::move(arguments)));
  }
}

JustError<ErrorLoc> FunctionDefinition::generateVirtualDispatchBody(Context& bodyContext) {
  unique_ptr<StatementBlock> defaultBlock;
  if (body)
    defaultBlock = std::move(body);
  int virtualIndex = [&]() {
    for (int i = 0; i < parameters.size(); ++i)
      if (parameters[i].isVirtual)
        return i;
    fail();
  }();
  for (int i = 0; i < parameters.size(); ++i) {
    parameters[i].isMutable = true;
    if (i != virtualIndex) {
      // we have to change the parameter names, because they could clash with union member names
      auto newName = "v_param" + *parameters[i].name;
      if (defaultBlock)
        defaultBlock = cast<StatementBlock>(defaultBlock->replaceVar(*parameters[i].name, newName));
      parameters[i].name = newName;
    }
  }
  auto& virtualParam = parameters[virtualIndex];
  auto virtualType = bodyContext.getTypeFromString(virtualParam.type);
  unique_ptr<Expression> switchExpr;
  body = make_unique<StatementBlock>(codeLoc);
  auto unionType = dynamic_cast<StructType*>(*virtualType);
  bool lvalueParam = false;
  if (!unionType) {
    unionType = dynamic_cast<StructType*>(virtualType.get()->removePointer());
    lvalueParam = true;
    switchExpr = make_unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
        make_unique<Variable>(IdentifierInfo(*virtualParam.name, codeLoc)));
  } else
    switchExpr = make_unique<MoveExpression>(codeLoc, *virtualParam.name, codeLoc);
  if (!unionType || unionType->alternatives.empty())
    return codeLoc.getError("Virtual parameter must be of a union type or a pointer to one");
  auto switchStatementPtr = make_unique<SwitchStatement>(codeLoc, std::move(switchExpr));
  auto& switchStatement = *switchStatementPtr;
  body->elems.push_back(std::move(switchStatementPtr));
  for (auto& alternative : unionType->alternatives) {
    auto alternativeType = alternative.type;
    if (dynamic_cast<MutablePointerType*>(*virtualType))
      alternativeType = MutableReferenceType::get(std::move(alternativeType));
    else if (dynamic_cast<PointerType*>(*virtualType))
      alternativeType = ReferenceType::get(std::move(alternativeType));
    WithErrorLine<unique_ptr<Expression>> call = [&] {
      if (auto regularName = name.getReferenceMaybe<string>())
        return getVirtualFunctionCallExpr(bodyContext, *regularName, alternative.name, alternativeType, virtualIndex,
            lvalueParam);
      else if (auto op = name.getValueMaybe<Operator>())
        return getVirtualOperatorCallExpr(bodyContext, *op, alternative.name, alternativeType, virtualIndex, lvalueParam);
      else
        fail();
    }();
    if (!call) {
      if (defaultBlock)
        switchStatement.defaultBlock = std::move(defaultBlock);
      else if (!switchStatement.defaultBlock)
        return call.get_error();
      continue;
    }
    auto block = make_unique<StatementBlock>(codeLoc);
    block->elems.push_back(make_unique<ReturnStatement>(codeLoc, std::move(*call)));
    switchStatement.caseElems.push_back(
        SwitchStatement::CaseElem {
          codeLoc,
          {alternative.name},
          alternative.name,
          std::move(block)
        });
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkAndGenerateCopyFunction(const Context& context, const string& functionName) {
  if (!body && isDefault) {
    if (parameters.size() != 1)
      return codeLoc.getError("Expected exactly one parameter in copy function");
    auto type = TRY(context.getTypeFromString(parameters[0].type));
    if (type != PointerType::get(context.getTypeFromString(returnType).get()))
      return codeLoc.getError("Copy function parameter type must be the same as pointer to return type");
    auto structType = dynamic_cast<StructType*>(type->removePointer());
    if (!structType)
      return codeLoc.getError("Can only generate copy function for user-defined types");
    body = make_unique<StatementBlock>(codeLoc);
    if (structType->alternatives.empty()) {
      auto call = make_unique<FunctionCall>(returnType, false);
      for (auto elem : structType->members) {
        auto copiedParam = make_unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
        auto copyCall = make_unique<FunctionCall>(IdentifierInfo(functionName, codeLoc), false);
        copyCall->arguments.push_back(make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            MemberAccessExpression::getPointerAccess(codeLoc, std::move(copiedParam), elem.name)));
        call->arguments.push_back(std::move(copyCall));
      }
      body->elems.push_back(make_unique<ReturnStatement>(codeLoc, std::move(call)));
    } else {
      auto copiedParam = make_unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
          make_unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc)));
      auto topSwitch = make_unique<SwitchStatement>(codeLoc, std::move(copiedParam));
      for (auto& alternative : structType->alternatives) {
        auto block = make_unique<StatementBlock>(codeLoc);
        auto constructorName = returnType;
        constructorName.parts.push_back(IdentifierInfo::IdentifierPart { alternative.name, {}, {} });
        auto constructorCall = make_unique<FunctionCall>(constructorName, false);
        if (alternative.type != BuiltinType::VOID)
          constructorCall->arguments.push_back(make_unique<FunctionCall>(IdentifierInfo(functionName, codeLoc),
              make_unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
                  make_unique<Variable>(IdentifierInfo(alternative.name, codeLoc))), false));
        block->elems.push_back(make_unique<ReturnStatement>(codeLoc, std::move(constructorCall)));
        topSwitch->caseElems.push_back(
            SwitchStatement::CaseElem {
              codeLoc,
              {alternative.name},
              alternative.name,
              std::move(block)
            }
        );
      }
      body->elems.push_back(std::move(topSwitch));
    }
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkAndGenerateDefaultConstructor(const Context& context) {
  if (!body && isDefault) {
    auto type = TRY(context.getTypeFromString(returnType));
    auto structType = dynamic_cast<StructType*>(type);
    if (!structType || !structType->alternatives.empty())
      return codeLoc.getError("Cannot generate default constructor for non-struct types");
    if (parameters.size() != structType->members.size())
      return codeLoc.getError("Expected exactly as many parameters as members in type " + quote(structType->getName()));
    body = make_unique<StatementBlock>(codeLoc);
    IdentifierInfo id = returnType;
    id.parts.push_back(returnType.parts[0]);
    id.parts.back().templateArguments.clear();
    auto call = make_unique<FunctionCall>(std::move(id), false);
    for (int i = 0; i < structType->members.size(); ++i) {
      call->arguments.push_back(make_unique<MoveExpression>(codeLoc, *parameters[i].name, codeLoc));
    }
    body->elems.push_back(make_unique<ReturnStatement>(codeLoc, std::move(call)));
  }
  return success;
}

static vector<unique_ptr<Statement>> getDestructorCalls(CodeLoc codeLoc, const vector<string>& ids, const vector<Type*>& types) {
  vector<unique_ptr<Statement>> ret;
  CHECK(ids.size() == types.size());
  for (int i = 0; i < ids.size(); ++i) {
    if (types[i]->hasDestructor())
      ret.push_back(getDestructorStatement(codeLoc, ids[i]));
    else
      ret.push_back(nullptr);
  }
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::addInstance(const Context& callContext, FunctionInfo* instance) {
  if (callContext.getTemplateParams())
    return success;
  wasUsed = true;
  auto callTopContext = callContext.getTopLevel();
  if (instance != functionInfo) {
    if (body) {
      CHECK(functionInfo == instance->getParent());
      for (auto& other : instances)
        if (other.functionInfo->getWithoutRequirements() == instance->getWithoutRequirements())
          return success;
      int instanceIndex = instances.size();
      instances.push_back(InstanceInfo{unique_ptr<StatementBlock>(), {},
          instance, callTopContext.getChild()});
      if (wasChecked) {
        instances[instanceIndex].body = cast<StatementBlock>(origBody->deepCopy());
        vector<unique_ptr<Statement>> destructorCallsTmp;
        auto ret = checkBody(callTopContext, *instances[instanceIndex].body,
            *instances[instanceIndex].functionInfo, destructorCallsTmp);
        instances[instanceIndex].destructorCalls = std::move(destructorCallsTmp);
        return ret;
      }
    }
  } else
    CHECK(instance->type.templateParams.empty());
  return success;
}

static void addTemplateParams(Context& context, vector<Type*> params, bool variadic) {
  for (int i = 0; i < params.size(); ++i) {
    auto& param = params[i];
    if (auto valueType = dynamic_cast<CompileTimeValue*>(param)) {
      auto templateValue = valueType->value.getReferenceMaybe<CompileTimeValue::TemplateValue>();
      context.addType(templateValue->name,
          CompileTimeValue::getTemplateValue(templateValue->type, templateValue->name));
    } else {
      context.addType(param->getName(), param);
    }
    if (variadic && i == params.size() - 1)
      context.addUnexpandedTypePack(param->getName(), param);
  }
}

JustError<ErrorLoc> FunctionDefinition::generateDefaultBodies(Context& context) {
  auto getContext = [&] () -> WithErrorLine<Context> {
    Context bodyContext = context.getChild();
    addTemplateParams(bodyContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
    auto res = TRY(applyRequirements(bodyContext, templateInfo));
    return std::move(bodyContext);
  };
  if (isVirtual) {
    auto bodyContext = TRY(getContext());
    TRY(generateVirtualDispatchBody(bodyContext));
  } else
  if (name == "copy"s || name == "implicit_copy"s) {
    auto bodyContext = TRY(getContext());
    TRY(checkAndGenerateCopyFunction(bodyContext, name.get<string>()));
  } else
  if (name == ConstructorTag{}) {
    auto bodyContext = TRY(getContext());
    TRY(checkAndGenerateDefaultConstructor(bodyContext));
  } else
  if (isDefault)
    return codeLoc.getError("Cannot generate a default body for this function");
  return success;
}

void FunctionDefinition::addParamsToContext(Context& context, const FunctionInfo& instanceInfo) const {
  vector<Type*> templateParams;
//  std::cout << "Checking " << instanceInfo.prettyString() << std::endl;
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    if (i >= instanceInfo.type.templateParams.size()) {
      context.addExpandedTypePack(templateInfo.params[i].name, {});
//      std::cout << "Adding empty expanded type pack " << templateInfo.params[i].name << std::endl;
    } else {
      auto& t = instanceInfo.type.templateParams[i];
      if (!t->getMangledName())
        templateParams.push_back(t);
      else if (i < templateInfo.params.size() - 1 || !templateInfo.variadic) {
//        std::cout << "Adding " << templateInfo.params[i].name << " = " << t->getName() << std::endl;
        context.addType(templateInfo.params[i].name, t);
        if (!dynamic_cast<ConceptType*>(t))
          context.addSubstitution(Context::SubstitutionInfo{
              templateInfo.params[i].name, t->getCodegenName(), !!dynamic_cast<CompileTimeValue*>(t)});
      } else {
        auto types = instanceInfo.type.templateParams.getSubsequence(i);
//        std::cout << "Adding expanded type pack " << templateInfo.params[i].name << " " << joinTypeList(types) << std::endl;
        context.addExpandedTypePack(templateInfo.params[i].name, types);
      }
    }
  }
  if (!templateParams.empty())
    context.setTemplated(templateParams);
  else if (!templateInfo.params.empty())
    context.setTemplateInstance();
  for (int i = 0; i < parameters.size(); ++i) {
    if (i >= instanceInfo.type.params.size()) {
      if (parameters[i].name) {
        context.addExpandedVariablePack(*parameters[i].name, {});
//        std::cout << "Adding empty expanded type pack " << *parameters[i].name << std::endl;
      }
    } else {
      auto& param = parameters[i];
      auto getParamType = [&] (Type* type) {
        if (name.contains<Operator>())
          type = convertReferenceToPointer(type);
        return param.isMutable ? (Type*)MutableReferenceType::get(type) : (Type*)ReferenceType::get(type);
      };
      if (auto name = parameters[i].name) {
        if (!isVariadicParams || i < parameters.size() - 1) {
          auto type = getParamType(instanceInfo.type.params[i]);
          context.addVariable(*name, type, param.codeLoc);
//          std::cout << "Adding param " << *name << " of type " << type->getName() << std::endl;
        } else {
          auto types = instanceInfo.type.params.getSubsequence(parameters.size() - 1)
              .transform([&](auto& type) { return getParamType(type); });
          if (instanceInfo.type.variadicParams) {
            context.addUnexpandedVariablePack(*name, types.back());
            //std::cout << "Adding unexpanded variable pack " << *name << " of type " << types.back()->getName() << std::endl;
          } else {
            context.addExpandedVariablePack(*name, types);
            //std::cout << "Adding expanded variable pack " << *name << " of type " << joinTypeList(types) << std::endl;
          }
        }
      }
    }
  }
}

static void considerAddingVoidReturn(const Context& context, StatementBlock* block, Type* retVal) {
  if (context.canConvert(BuiltinType::VOID, retVal)
      && (block->elems.empty() || !block->elems.back()->hasReturnStatement())) {
    block->elems.push_back(make_unique<ReturnStatement>(block->codeLoc));
    auto c = context.getChild();
    CHECK(!!block->elems.back()->check(c));
  }
}

JustError<ErrorLoc> FunctionDefinition::checkBody(const Context& callContext,
    StatementBlock& myBody, const FunctionInfo& instanceInfo, vector<unique_ptr<Statement>>& destructorCalls) const {
  auto bodyContext = callContext.getChild();
  bodyContext.merge(*definitionContext);
  addParamsToContext(bodyContext, instanceInfo);
  vector<Type*> templateParams;
//  std::cout << "Checking " << instanceInfo.prettyString() << std::endl;
  for (auto& t : instanceInfo.type.templateParams)
    if (!t->getMangledName())
      templateParams.push_back(t);
  auto retVal = instanceInfo.type.retVal;
  if (name.contains<Operator>())
    retVal = convertReferenceToPointer(retVal);
  TRY(checkForIncompleteTypes(instanceInfo, bodyContext));
  ReturnTypeChecker returnChecker(retVal);
  bodyContext.addReturnTypeChecker(&returnChecker);
  TRY(myBody.check(bodyContext));
  considerAddingVoidReturn(bodyContext, &myBody, retVal);
  if (templateParams.empty()) {
    vector<string> paramNames;
    for (int i = 0; i < instanceInfo.type.params.size(); ++i)
      paramNames.push_back(*instanceInfo.getParamName(i, this));
    destructorCalls = getDestructorCalls(codeLoc, std::move(paramNames), instanceInfo.type.params);
    for (auto& elem : destructorCalls)
      if (elem)
        TRY(elem->check(bodyContext));
  }
  if (retVal == BuiltinType::NORETURN && !myBody.hasReturnStatement())
    return codeLoc.getError("This function should never return");
  if (retVal != BuiltinType::VOID && !myBody.hasReturnStatement())
    return codeLoc.getError("Not all paths lead to a return statement in a function returning non-void");
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkForIncompleteTypes(const FunctionInfo& info,
    const Context& context) const {
  for (int i = 0; i < info.type.params.size(); ++i) {
    auto paramType = info.type.params[i];
    TRY(paramType->getSizeError(context).addCodeLoc(parameters[min(i, parameters.size() - 1)].codeLoc));
  }
  TRY(info.type.retVal->getSizeError(context).addCodeLoc(returnType.codeLoc));
  return success;
}

static WithError<Type*> getDestructedType(const vector<Type*>& params) {
  if (params.size() != 1)
    return "Destructor function should take exactly one argument"s;
  auto& param = params[0];
  auto ret = param->removePointer();
  if (ret == param)
    return "Destructor function parameter should be a pointer to the destructed type"s;
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::check(Context& context, bool notInImport) {
  TRY(generateDefaultBodies(context));
  // if origBody is present then body has already been checked
  // this can happen in exported functions, for example
  // it probably isn't necessary to recheck the body in these cases
  if (body && !origBody && (!templateInfo.params.empty() || notInImport)) {
    // save the original unchecked body to use by template instances
    origBody = cast<StatementBlock>(body->deepCopy());
    Context paramsContext = context.getChild();
    // use a temporary FunctionInfo with fresh template types to avoid clashes if the function
    // is calling itself with changed parameter order
    auto thisFunctionInfo = functionInfo;
    auto newParams = TRY(getTemplateParams(templateInfo, context));
    addTemplateParams(paramsContext, newParams, thisFunctionInfo->type.variadicTemplate);
    TRY(applyRequirements(paramsContext, templateInfo));
    ErrorBuffer errors;
    for (int i = 0; i < newParams.size(); ++i) {
      thisFunctionInfo = replaceInFunction(paramsContext, thisFunctionInfo,
        functionInfo->type.templateParams[i], newParams[i], errors);
    }
    if (!errors.empty())
      return codeLoc.getError(errors[0]);
    // For checking the template we use the Context that includes the template params.
    definitionContext.emplace(paramsContext.getChild());
    TRY(checkBody(*definitionContext, *body, *thisFunctionInfo, destructorCalls));
    // For checking instances we just use the top level context.
    definitionContext.emplace(context.getChild());
    wasChecked = true;
    for (int i = 0; i < instances.size(); ++i)
      if (!instances[i].body) {
        instances[i].body = cast<StatementBlock>(origBody->deepCopy());
        TRY(checkBody(instances[i].callContext, *instances[i].body,
            *instances[i].functionInfo, instances[i].destructorCalls));
      }
    MoveChecker moveChecker;
    for (auto& p : parameters)
      if (p.name)
        moveChecker.addVariable(*p.name);
    if (name == "destruct"s) {
      auto destructedType = TRY(getDestructedType(thisFunctionInfo->type.params).addCodeLoc(codeLoc));
      if (auto structType = dynamic_cast<StructType*>(destructedType))
        if (!structType->definition || structType->definition->file != codeLoc.file)
          return codeLoc.getError("Destructor function must be defined in the same file as the destructed type");
    }
    return body->checkMoves(moveChecker);
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::handleIsMemberParamsFunction() {
  if (functionInfo->type.params.size() < 2)
    return codeLoc.getError("This special function requires at least two parameters");
  auto t = functionInfo->type.params[0];
  if (auto s = dynamic_cast<StructType*>(t))
    if (s->external) {
      for (int i = 1; i < functionInfo->type.params.size(); ++i) {
        auto t = functionInfo->type.params[i];
        auto index = [&]()-> optional<int> {
          for (int j = 0; j < s->templateParams.size(); ++j)
            if (s->templateParams[j] == t)
              return j;
          return none;
        }();
        if (!index)
          return parameters[i].codeLoc.getError("Parameter is not a member of extern struct " + quote(s->getName()));
        s->parent->memberTemplateParams.push_back(*index);
      }
      return success;
    }
  return codeLoc.getError("This special function requires that the first parameter is an extern struct");
}

JustError<ErrorLoc> FunctionDefinition::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  TRY(setFunctionSignature(context, nullptr, cache.isCurrentlyBuiltIn()));
  if (name == "is_member_params"s)
    return handleIsMemberParamsFunction();
  TRY(context.addFunction(functionInfo).addCodeLoc(codeLoc));
  if (name == "destruct"s) {
    auto destructedType = TRY(getDestructedType(functionInfo->type.params).addCodeLoc(codeLoc));
    if (auto structType = dynamic_cast<StructType*>(destructedType)) {
      if (!structType->alternatives.empty())
        return codeLoc.getError("User-defined destructors for union types are not supported");
      auto adjusted = functionInfo;
      if (functionInfo->type.templateParams.size() != structType->templateParams.size())
        return codeLoc.getError("Number of template parameters of destructor function must match destructed type");
      for (auto& param : functionInfo->type.templateParams)
        if (!structType->templateParams.contains(param))
          return codeLoc.getError("Template parameters of destructor function must match destructed type");
      ErrorBuffer errors;
      for (int i = 0; i < structType->parent->templateParams.size(); ++i)
        adjusted = replaceInFunction(context, adjusted, functionInfo->type.templateParams[i],
            structType->parent->templateParams[i], errors);
      if (!errors.empty())
        return codeLoc.getError("Error in the destructor definition for type " + quote(structType->getName()) + ":\n"
            + errors[0]);
      if (structType->parent->destructor && structType->parent->destructor != adjusted)
        return codeLoc.getError("Destructor function for type " + quote(destructedType->getName())
            + " already defined here: " + structType->destructor->getDefinition()->codeLoc.toString());
      structType->parent->destructor = adjusted;
    } else
    if (!cache.isCurrentlyBuiltIn())
      return codeLoc.getError("User-defined destructor is allowed only for struct types");
  }
  return success;
}

static void addBuiltInConcepts(Context& context) {
  auto addType = [&context](const char* name, Type* type) {
    auto concept = new Concept(name, nullptr, Context(context.typeRegistry, context.languageIndex, true), false);
    concept->modParams().push_back(new TemplateParameterType(type, "T", CodeLoc()));
    context.typeRegistry->addConcept(name, concept);
    context.addConcept(name, concept);
  };
  addType("is_enum", BuiltinType::ENUM_TYPE);
  addType("is_struct", BuiltinType::STRUCT_TYPE);
  addType("is_union", BuiltinType::UNION_TYPE);
  addType("is_concept", BuiltinType::CONCEPT_TYPE);
}

Context createPrimaryContext(TypeRegistry* typeRegistry, LanguageIndex* languageIndex) {
  Context context(typeRegistry, languageIndex, true);
  context.addVariable("void_value", BuiltinType::VOID, CodeLoc(), true);
  for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE, BuiltinType::DOUBLE,
       BuiltinType::BOOL, BuiltinType::VOID, BuiltinType::CHAR, BuiltinType::STRING, BuiltinType::NULL_TYPE}) {
    context.addType(type->getName(), type);
    context.setTypeFullyDefined(type);
  }
  CHECK(context.addImplicitFunction(Operator::PLUS, FunctionSignature(BuiltinType::STRING,
      {BuiltinType::STRING, BuiltinType::STRING}, {}).setBuiltin()));
  CHECK(context.addImplicitFunction(Operator::PLUS, FunctionSignature(BuiltinType::STRING,
      {BuiltinType::STRING, BuiltinType::CHAR}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(type, {type}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT, Operator::DECREMENT})
    for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::VOID,
          {MutableReferenceType::get(type)}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULO})
    for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE, BuiltinType::DOUBLE})
      if (type != BuiltinType::DOUBLE || op != Operator::MODULO)
        CHECK(context.addImplicitFunction(op, FunctionSignature(type, {type, type}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT_BY, Operator::DECREMENT_BY, Operator::MULTIPLY_BY, Operator::DIVIDE_BY})
    for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::VOID,
          {MutableReferenceType::get(type), type}, {}).setBuiltin()));
  for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
    CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
        {BuiltinType::BOOL, BuiltinType::BOOL}, {}).setBuiltin()));
  CHECK(context.addImplicitFunction(Operator::LOGICAL_NOT, FunctionSignature(BuiltinType::BOOL,
      {BuiltinType::BOOL}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::NOT_EQUAL, Operator::LESS_THAN})
    for (auto type : {BuiltinType::INT, BuiltinType::LONG, BuiltinType::SHORT, BuiltinType::BYTE, BuiltinType::STRING,
         BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
          {type, type}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::NOT_EQUAL})
    for (auto type : {BuiltinType::BOOL, BuiltinType::CHAR})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
          {type, type}, {}).setBuiltin()));
  auto metaTypes = {BuiltinType::ANY_TYPE, BuiltinType::STRUCT_TYPE, BuiltinType::ENUM_TYPE, BuiltinType::UNION_TYPE};
  CHECK(context.addImplicitFunction(Operator::EQUALS, FunctionSignature(BuiltinType::BOOL, {BuiltinType::ANY_TYPE, BuiltinType::ANY_TYPE}, {}).setBuiltin()));
  CHECK(context.addImplicitFunction(Operator::NOT_EQUAL, FunctionSignature(BuiltinType::BOOL, {BuiltinType::ANY_TYPE, BuiltinType::ANY_TYPE}, {}).setBuiltin()));
  addBuiltInConcepts(context);
  context.addBuiltInFunction("enum_count", BuiltinType::INT, {(Type*)BuiltinType::ENUM_TYPE},
      [](const Context& context, vector<Type*> args) -> WithError<Type*> {
        if (!context.isFullyDefined(args[0]))
          return "Enum " + quote(args[0]->getName()) + " is incomplete in this context";
        if (auto enumType = dynamic_cast<EnumType*>(args[0]))
          return (Type*) CompileTimeValue::get((int) enumType->elements.size());
        else
          fail();
      });
  context.addBuiltInFunction("struct_count", BuiltinType::INT, {(Type*)BuiltinType::STRUCT_TYPE},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          return (Type*) CompileTimeValue::get((int) structType->members.size());
        else
          fail();
      });
  context.addBuiltInFunction("union_count", BuiltinType::INT, {(Type*)BuiltinType::UNION_TYPE},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          return (Type*) CompileTimeValue::get((int) structType->alternatives.size());
        else
          fail();
      });
  context.addBuiltInFunction("to_string", BuiltinType::STRING, {(Type*)BuiltinType::ANYTHING},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        return (Type*) CompileTimeValue::get(args[0]->getName());
      });
  context.addBuiltInFunction("get_member_name", BuiltinType::STRING, {(Type*)BuiltinType::STRUCT_TYPE,
      (Type*)BuiltinType::INT},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          if (auto value = dynamic_cast<CompileTimeValue*>(args[1]))
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (Type*) CompileTimeValue::get(structType->members[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("is_const_member", BuiltinType::BOOL, {(Type*)BuiltinType::STRUCT_TYPE,
      (Type*)BuiltinType::INT},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          if (auto value = dynamic_cast<CompileTimeValue*>(args[1]))
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (Type*) CompileTimeValue::get(structType->members[*intValue].isConst);
            }
        fail();
      });
  context.addBuiltInFunction("get_member_type", BuiltinType::ANY_TYPE, {(Type*)BuiltinType::STRUCT_TYPE,
      (Type*)BuiltinType::INT},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          if (auto value = dynamic_cast<CompileTimeValue*>(args[1]))
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return structType->members[*intValue].type;
            }
        fail();
      });
  context.addBuiltInFunction("get_alternative_name", BuiltinType::STRING, {(Type*)BuiltinType::UNION_TYPE,
      (Type*)BuiltinType::INT},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          if (auto value = dynamic_cast<CompileTimeValue*>(args[1]))
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->alternatives.size())
                return "Union " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (Type*) CompileTimeValue::get(structType->alternatives[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("get_alternative_type", BuiltinType::ANY_TYPE, {(Type*)BuiltinType::UNION_TYPE,
      (Type*)BuiltinType::INT},
      [](const Context&, vector<Type*> args) -> WithError<Type*> {
        if (auto structType = dynamic_cast<StructType*>(args[0]))
          if (auto value = dynamic_cast<CompileTimeValue*>(args[1]))
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->alternatives.size())
                return "Union " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return structType->alternatives[*intValue].type;
            }
        fail();
      });
  context.addBuiltInFunction("compile_error", BuiltinType::VOID, {(Type*)BuiltinType::STRING},
      [](const Context& context, vector<Type*> args) -> WithError<Type*> {
        if (!!context.getTemplateParams())
          return (Type*)CompileTimeValue::get(CompileTimeValue::VoidValue{});
        if (auto value = dynamic_cast<CompileTimeValue*>(args[0]))
          if (auto s = value->value.getReferenceMaybe<string>())
            return *s;
        fail();
      });
  context.addBuiltInFunction("known_size", BuiltinType::BOOL, {(Type*)BuiltinType::ANY_TYPE},
      [](const Context& context, vector<Type*> args) -> WithError<Type*> {
        return (Type*) CompileTimeValue::get(!!args[0]->getSizeError(context));
      });
  return context;
}

static JustError<ErrorLoc> addExportedContext(const Context& primaryContext, ImportCache& cache, AST& ast,
    const string& path, bool isBuiltIn) {
  cache.pushCurrentImport(path, isBuiltIn);
  auto importContext = primaryContext.getChild(true);
  for (auto& elem : ast.elems) {
    if (elem->exported) {
      TRY(elem->addToContext(importContext, cache, primaryContext));
      elem->addGeneratedConstructor(importContext, ast);
    }
  }
  for (auto& elem : ast.elems)
    if (elem->exported)
      TRY(elem->check(importContext));
  cache.popCurrentImport(isBuiltIn);
  cache.insert(path, std::move(importContext), isBuiltIn || cache.isCurrentlyBuiltIn());
  return success;
}

WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST& ast, Context& context,
    const Context& primaryContext, ImportCache& cache, bool isBuiltInModule) {
  TRY(addExportedContext(primaryContext, cache, ast, path, isBuiltInModule));
  context.merge(cache.getContext(path));
  for (auto& elem : ast.elems)
    if (!elem->exported) {
      TRY(elem->addToContext(context, cache, primaryContext));
      elem->addGeneratedConstructor(context, ast);
    }
  for (auto& elem : ast.elems)
    TRY(elem->check(context, true));
  return cache.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

JustError<ErrorLoc> ExpressionStatement::check(Context& context, bool) {
  auto oldExpr = expr.get();
  auto res = TRY(getType(context, expr));
  isConstant = oldExpr != expr.get();
  noReturnExpr = res == BuiltinType::NORETURN;
  if (!canDiscard && res != BuiltinType::VOID && res != BuiltinType::NORETURN)
    return codeLoc.getError("Expression result of type " + quote(res->getName()) + " discarded");
  if (canDiscard && !isConstant) {
    expr = make_unique<FunctionCall>(IdentifierInfo("discard_impl", codeLoc), std::move(expr), false);
    TRY(getType(context, expr));
  }
  return success;
}

unique_ptr<Statement> ExpressionStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = make_unique<ExpressionStatement>(fun(expr.get()));
  ret->canDiscard = canDiscard;
  ret->noReturnExpr = noReturnExpr;
  return ret;
}

JustError<ErrorLoc> ExpressionStatement::checkMovesImpl(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

bool ExpressionStatement::hasReturnStatement() const {
  return noReturnExpr;
}

WithEvalError<StatementEvalResult> ExpressionStatement::eval(Context& context) {
  TRY(expr->eval(context));
  return StatementEvalResult{};
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

JustError<ErrorLoc> FunctionCall::checkNamedArgs() const {
  int cnt = argNames.size();
  if (variadicArgs)
    --cnt;
  for (int i = 0; i < cnt; ++i) {
    optional<string> paramName;
    if (auto def = functionInfo->getParent()->getDefinition())
      paramName = functionInfo->getParamName(callType ? (i + 1) : i, def);
    if (argNames[i] && paramName && argNames[i] != paramName) {
      return arguments[i]->codeLoc.getError("Function argument " + quote(*argNames[i]) +
          " doesn't match parameter " + quote(*paramName) + " of function " +
          functionInfo->prettyString());
    }
  }
  return success;
}

JustError<ErrorLoc> FunctionCall::checkVariadicCall(const Context& callContext) {
  bool variadicCall = variadicArgs && !callContext.getExpandedVariablePack();
  bool variadicTemplateCall = variadicTemplateArgs && !callContext.getExpandedTypePack();
  if (!variadicCall && !variadicTemplateCall)
    return success;
  auto unExpandedTypePack = callContext.getUnexpandedTypePack();
  if (variadicTemplateCall && !unExpandedTypePack)
    return codeLoc.getError("No unexpanded template parameter pack found");
  Type* returnType = nullptr;
  vector<Type*> expanded;
  vector<Type*> expandedArgs;
  unordered_set<FunctionInfo*> allCalls;
  auto typePack = unExpandedTypePack->second;
  while (true) {
    auto call = cast<FunctionCall>(deepCopy());
    auto context = callContext.getChild();
    if (variadicTemplateCall)
      context.addExpandedTypePack(templateArgs->back()->getName(), expanded);
    if (variadicCall)
      context.addExpandedVariablePack(callContext.getUnexpandedVariablePack()->first, expandedArgs);
    auto funContext = Context(context.typeRegistry, context.languageIndex, true);
    ErrorBuffer errors;
    for (auto& e : expanded)
      for (auto& fun : callContext.getAllFunctions())
        if (!!fun->type.concept) {
          auto replaced = fun;
          auto from = unExpandedTypePack->second;
            replaced = replaceInFunction(funContext, replaced, from, e, errors);
          if (errors.empty() && replaced != fun)
            ignore(funContext.addFunction(replaced));
          CHECK(errors.empty()); // not sure if any errors should appear here, so checking just in case
        }
    context.merge(funContext);
    auto type = TRY(call->getTypeImpl(context));
    if (!!returnType && returnType != type)
      return codeLoc.getError("Return type mismatch between called functions in variadic function call");
    returnType = type;
    expanded.push_back(new TemplateParameterType(getExpandedParamName(
        typePack->getName(), expanded.size()), codeLoc));
    if (auto variablePack = callContext.getUnexpandedVariablePack())
      expandedArgs.push_back(variablePack->second->replace(callContext, typePack, expanded.back(), errors));
    CHECK(errors.empty());
    auto target = call->functionInfo->getParent();
    if (allCalls.count(target))
      break;
    allCalls.insert(target);
  }
  return success;
}

JustError<ErrorLoc> FunctionCall::considerSpecialCalls(const Context& context) {
  if (identifier.parts.empty())
    return success;
  auto var = identifier.parts[0].name;
  auto type = [&]() -> Type*{
    if (auto t = context.getTypeOfVariable(var, identifier.codeLoc))
      return *t;
    if (auto t = context.getTypeFromString(identifier))
      if (!(*t)->getType()->isMetaType())
        return (*t)->getType();
    return nullptr;
  }();
  if (!methodCall) {
    if (type) {
      if (auto functionType = dynamic_cast<FunctionType*>(type->removePointer())) {
        identifier.parts[0].name = functionType->name;
      } else {
        auto tmp = std::move(arguments);
        arguments.clear();
        arguments.push_back(make_unique<Variable>(identifier));
        methodCall = true;
        arguments.append(std::move(tmp));
        identifier.parts[0].name = "invoke";
      }
    }
  } else if (!arguments.empty()) {
    auto argType = TRY(getType(context, arguments[0]));
    if (var == "invoke" && !arguments.empty()) {
      if (auto functionType = dynamic_cast<FunctionType*>(argType->removePointer())) {
        identifier = IdentifierInfo(functionType->name, codeLoc);
        arguments.removeIndex(0);
      }
    } else
    if (argType->getTypeOfMember(var)) {
      arguments[0] = make_unique<MemberAccessExpression>(codeLoc, std::move(arguments[0]), var);
      identifier.parts[0].name = "invoke";
    }
  }
  return success;
}

WithErrorLine<Type*> FunctionCall::getTypeImpl(const Context& callContext) {
  optional<ErrorLoc> error;
  if (!functionInfo) {
    templateArgs = TRY(callContext.getTypeList(identifier.parts.back().templateArguments, variadicTemplateArgs));
    TRY(considerSpecialCalls(callContext));
    vector<Type*> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      auto context = callContext.getChild();
      if (variadicArgs && i == arguments.size() - 1) {
        if (auto types = callContext.getExpandedVariablePack().addCodeLoc(arguments[i]->codeLoc)) {
          auto origArg = arguments[i]->deepCopy();
          arguments.pop_back();
          for (int j = 0; j < types->second.size(); ++j) {
            auto varName = getExpandedParamName(types->first, j);
            arguments.push_back(origArg->replaceVar(types->first, varName));
            auto argContext = callContext.getChild();
            argContext.addVariable(varName, ReferenceType::get(types->second[j]), arguments.back()->codeLoc);
            argTypes.push_back(TRY(getType(argContext, arguments.back())));
            argLocs.push_back(arguments.back()->codeLoc);
          }
        }
        break;
      } else {
        argTypes.push_back(TRY(getType(context, arguments[i])));
        argLocs.push_back(arguments[i]->codeLoc);
      }
    }
    bool compileTimeArgs = true;
    for (auto& arg : arguments)
      // this evaluates the expression for the second time, can it cause a side effect to happen twice?
      if (!arg->eval(callContext)) {
        compileTimeArgs = false;
        break;
      }
    auto tryMethodCall = [&](MethodCallType thisCallType, Type* origType) -> JustError<ErrorLoc> {
      auto res = getFunction(callContext, codeLoc, identifier, *templateArgs, argTypes, argLocs, arguments,
          compileTimeArgs);
      if (res)
        callType = thisCallType;
      if (callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER) {
        if (!origType->isReference() && argTypes[0]->removePointer()->hasDestructor()) {
          destructorCall = TRY(getDestructor(callContext, argTypes[0]->removePointer(), codeLoc));
          TRY(destructorCall->addInstance(callContext));
        }
      }
      res.unpack(functionInfo, error);
      return success;
    };
    if (methodCall) {
      auto leftType = argTypes[0];
      if (!dynamic_cast<PointerType*>(leftType->removeReference()) &&
          !dynamic_cast<MutablePointerType*>(leftType->removeReference()))
        TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD, leftType));
      if (!functionInfo) {
        argTypes[0] = dynamic_cast<MutableReferenceType*>(leftType)
            ? (Type*)MutablePointerType::get(leftType->removeReference())
            : (Type*)PointerType::get(leftType->removeReference());
        TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER, leftType));
      }
    } else
      getFunction(callContext, codeLoc, identifier, *templateArgs, argTypes, argLocs, arguments, compileTimeArgs)
          .unpack(functionInfo, error);
  }
  if (functionInfo) {
    if (auto def = functionInfo->getDefinition())
      callContext.languageIndex->addDefinition(codeLoc, identifier.parts[0].name.size(), def->codeLoc);
    callContext.languageIndex->addSignature(codeLoc, endLoc.column, endLoc.line, functionInfo->prettyString());
    TRY(checkVariadicCall(callContext));
    TRY(functionInfo->type.retVal->getSizeError(callContext).addCodeLoc(codeLoc));
    if (!callContext.getExpandedVariablePack())
      TRY(checkNamedArgs());
    if (auto res = functionInfo->addInstance(callContext); !res)
      return codeLoc.getError("When instantiating template " + functionInfo->getParent()->prettyString() + " as " + functionInfo->prettyString()  + ":\n"
        + res.get_error().loc.toString() + ": " + res.get_error().error);
    return functionInfo->type.retVal;
  }
  return *error;
}

WithEvalError<EvalResult> FunctionCall::eval(const Context& context) const {
  if (auto name = identifier.asBasicIdentifier()) {
    vector<Type*> args;
    vector<CodeLoc> locs;
    for (auto& e : arguments) {
      locs.push_back(e->codeLoc);
      args.push_back(TRY(e->eval(context)).value->removeValueReference());
    }
    return EvalResult{TRY(context.invokeFunction(*name, codeLoc, std::move(args), std::move(locs))), true};
  }
  return EvalError::noEval();
}

unique_ptr<Expression> FunctionCall::replaceVar(string from, string to) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to](Statement* expr) { return expr->replaceVar(from, to); },
      [from, to](Expression* expr) { return expr->replaceVar(from, to); }));
  ret->argNames = ::transform(argNames, [&](auto& n) -> optional<string> { if (n == from) return to; else return n; });
  return ret;
}

unique_ptr<Expression> FunctionCall::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = make_unique<FunctionCall>(codeLoc, methodCall, Private{});
  ret->identifier = identifier;
  for (auto& arg : arguments)
    ret->arguments.push_back(fun(arg.get()));
  ret->templateArgs = templateArgs;
  ret->argNames = argNames;
  ret->variadicArgs = variadicArgs;
  ret->variadicTemplateArgs = variadicTemplateArgs;
  return ret;
}

JustError<ErrorLoc> FunctionCall::checkMoves(MoveChecker& checker) const {
  for (auto& e : arguments)
    TRY(e->checkMoves(checker));
  return success;
}

FunctionCall::FunctionCall(CodeLoc l, bool methodCall, Private) : Expression(l), methodCall(methodCall) {}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

JustError<ErrorLoc> SwitchStatement::check(Context& context, bool) {
  auto type = TRY(getType(context, expr));
  if (type->hasDestructor()) {
    destructorCall = TRY(getDestructor(context, type, codeLoc));
    TRY(destructorCall->addInstance(context));
  }
  if (!context.isFullyDefined(type->removePointer()))
    return codeLoc.getError("Type " + quote(type->getName()) + " is incomplete in this context");
  return type->handleSwitchStatement(*this, context, Type::ArgumentType::VALUE);
}

JustError<ErrorLoc> SwitchStatement::checkMovesImpl(MoveChecker& checker) const {
  TRY(expr->checkMoves(checker));
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& elem : caseElems) {
    checker.newAlternative();
    checker.startBlock();
    OnExit onExit2([&]{ checker.endBlock();});
    if (elem.declaredVar)
      checker.addVariable(*elem.declaredVar);
    TRY(elem.block->checkMoves(checker));
  }
  if (defaultBlock) {
    checker.newAlternative();
    TRY(defaultBlock->checkMoves(checker));
  }
  return success;
}

unique_ptr<Statement> SwitchStatement::transformImpl(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  auto ret = make_unique<SwitchStatement>(codeLoc, exprFun(expr.get()));
  ret->targetType = targetType;
  if (defaultBlock)
    ret->defaultBlock = cast<StatementBlock>(fun(defaultBlock.get()));
  for (auto& elem : caseElems)
    ret->caseElems.push_back(elem.transform(fun, exprFun));
  return ret;
}

bool SwitchStatement::hasReturnStatement() const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement())
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement())
    return false;
  return true;
}

UnionDefinition::UnionDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

JustError<ErrorLoc> UnionDefinition::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!type) {
    auto ret = r->addStruct(name, false, codeLoc).addCodeLoc(codeLoc);
    if (ret) {
      type = r->getStruct(name);
      type->templateParams = TRY(getTemplateParams(templateInfo, primaryContext));
    }
    return ret;
  }
  return success;
}

unique_ptr<Statement> UnionDefinition::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<UnionDefinition>(*this);
}

static WithErrorLine<set<AttributeType*>> getAttributeTypes(const Context& context,
    const vector<AttributeInfo>& attributes) {
  set<AttributeType*> ret;
  for (auto& attr : attributes)
    if (auto t = context.getType(attr.name)) {
      if (auto attrType = dynamic_cast<AttributeType*>(t))
        ret.insert(attrType);
      else
        return attr.codeLoc.getError(quote(attr.name) + " is not an attribute");
    } else
      return attr.codeLoc.getError("Attribute not found: " + quote(attr.name));
  return ret;
}

JustError<ErrorLoc> UnionDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  context.addType(name, type);
  context.setTypeFullyDefined(type);
  for (auto& attr : TRY(getAttributeTypes(context, attributes)))
    context.setAttribute(type, attr, type->templateParams);
  type->definition = codeLoc;
  auto membersContext = context.getChild();
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  type->requirements = TRY(applyRequirements(membersContext, templateInfo));
  unordered_set<string> subtypeNames;
  if (elements.size() > type->alternatives.size())
    for (auto& subtype : elements) {
      if (subtypeNames.count(subtype.name))
        return subtype.codeLoc.getError("Duplicate union member: " + quote(subtype.name));
      subtypeNames.insert(subtype.name);
      type->alternatives.push_back({subtype.name, TRY(membersContext.getTypeFromString(subtype.type)), false,
          subtype.codeLoc});
      vector<Type*> params;
      auto subtypeInfo = TRY(membersContext.getTypeFromString(subtype.type));
      if (subtypeInfo != BuiltinType::VOID)
        params.push_back(subtypeInfo);
      auto constructor = FunctionSignature(type, params, {});
      constructor.parentType = type;
      CHECK(type->staticContext.addImplicitFunction(subtype.name, constructor));
    }
  return success;
}

JustError<ErrorLoc> UnionDefinition::check(Context& context, bool) {
  if (checked)
    // This is a workaround to the buggy updateInstantiations function,
    // which fails if ran twice and should be removed
    return success;
  checked = true;
  auto bodyContext = context.getChild();
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  CHECK(!!applyRequirements(bodyContext, templateInfo));
  type->updateInstantations(context);
  return success;
}

JustError<ErrorLoc> StructDefinition::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!type) {
    auto ret = r->addStruct(name, external, codeLoc).addCodeLoc(codeLoc);
    if (ret) {
      type = r->getStruct(name);
      type->templateParams = TRY(getTemplateParams(templateInfo, primaryContext));
    }
    return ret;
  }
  return success;
}

unique_ptr<Statement> StructDefinition::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<StructDefinition>(*this);
}

JustError<ErrorLoc> StructDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  context.addType(name, type);
  context.setTypeFullyDefined(type);
  for (auto& attr : TRY(getAttributeTypes(context, attributes)))
    context.setAttribute(type, attr, type->templateParams);
  auto membersContext = context.getChild();
  addTemplateParams(membersContext, type->templateParams, false);
  type->requirements = TRY(applyRequirements(membersContext, templateInfo));
  if (members.size() > type->members.size())
    for (auto& member : members)
      type->members.push_back({member.name, TRY(membersContext.getTypeFromString(member.type)), member.isConst,
          member.codeLoc});
  for (auto& member : members)
    TRY(type->members.back().type->getSizeError(membersContext).addCodeLoc(member.codeLoc));
  for (int i = 0; i < members.size(); ++i)
    for (int j = i + 1; j < members.size(); ++j)
      if (members[i].name == members[j].name)
        return members[j].codeLoc.getError("Duplicate member: " + quote(members[j].name));
  context.setStructMembers(type, type->members.transform([](auto& elem) { return elem.type; }),
      type->templateParams);
  return success;
}

JustError<ErrorLoc> StructDefinition::check(Context& context, bool notInImport) {
  if (exported && type->destructor && !type->destructor->getDefinition()->exported)
    return type->destructor->getDefinition()->codeLoc.getError(
        "Destuctor function of an exported type must also be exported");
  if (checked)
    // This is a workaround to the buggy updateInstantiations function,
    // which fails if ran twice and should be removed
    return success;
  checked = true;
  auto methodBodyContext = context.getChild();
  addTemplateParams(methodBodyContext, type->templateParams, false);
  CHECK(!!applyRequirements(methodBodyContext, templateInfo));
  type->updateInstantations(context);
  TRY(type->getSizeError(context).addCodeLoc(codeLoc));
  return success;
}

void StructDefinition::addGeneratedConstructor(Context& context, const AST& ast) const {
  bool hasUserDefinedConstructors = false;
  for (auto& elem : ast.elems)
    if (auto functionDef = dynamic_cast<const FunctionDefinition*>(elem.get()))
      if (functionDef->name.contains<ConstructorTag>() && functionDef->returnType.parts[0].name == name)
        hasUserDefinedConstructors = true;
  if (!external && type->getStaticContext().getAllFunctions().empty()) {
    vector<Type*> constructorParams;
    for (auto& member : type->members)
      constructorParams.push_back(member.type);
    auto fun = FunctionSignature(type, std::move(constructorParams), type->templateParams);
    fun.generatedConstructor = true;
    if (!hasUserDefinedConstructors)
      CHECK(context.addImplicitFunction(ConstructorTag{}, fun));
    fun.templateParams.clear();
    fun.parentType = type;
    CHECK(type->getStaticContext().addImplicitFunction(ConstructorTag{}, fun));
    type->getStaticContext().addType(name, type);
  }
}

MoveExpression::MoveExpression(CodeLoc l, string id, CodeLoc variableLoc, bool hasDestructor)
    : Expression(l), identifier(id), hasDestructor(hasDestructor), variableLoc(variableLoc) {}

WithErrorLine<Type*> MoveExpression::getTypeImpl(const Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier, variableLoc)) {
      if (context.isNonMovable(identifier))
        return codeLoc.getError("Can't move " + quote(identifier) + ", because the switch condition is an l-value");
      if (context.isCapturedVariable(identifier))
        return codeLoc.getError("Can't move from a captured value");
      if (!dynamic_cast<MutableReferenceType*>(ret.get_value()) &&
          !dynamic_cast<ReferenceType*>(ret.get_value()))
        return codeLoc.getError("Can't move from " + quote(ret.get_value()->getName()));
      type = ret.get_value()->removeReference();
    } else
      return codeLoc.getError(ret.get_error());
  }
  hasDestructor = type->hasDestructor();
  return type;
}

unique_ptr<Expression> MoveExpression::replaceVar(string from, string to) const {
  if (from == identifier)
    return make_unique<MoveExpression>(codeLoc, to, variableLoc, hasDestructor);
  else
    return deepCopy();
}

unique_ptr<Expression> MoveExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<MoveExpression>(codeLoc, identifier, variableLoc, hasDestructor);
}

JustError<ErrorLoc> MoveExpression::checkMoves(MoveChecker& checker) const {
  return checker.moveVariable(codeLoc, identifier).addCodeLoc(codeLoc);
}

EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

JustError<ErrorLoc> EmbedStatement::check(Context& context, bool) {
  replacements = context.getSubstitutions();
  return success;
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  return returns ? TopLevelAllowance::CANT : TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = make_unique<EmbedStatement>(codeLoc, value);
  ret->returns = returns;
  ret->isTopLevel = isTopLevel;
  return std::move(ret);
}

bool EmbedStatement::hasReturnStatement() const {
  return returns;
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b,
    unique_ptr<Statement> a)
  : Statement(l), cond(std::move(c)), body(std::move(b)), afterContinue(std::move(a)) {}

JustError<ErrorLoc> WhileLoopStatement::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  auto condType = TRY(getType(bodyContext, cond));
  if (condType != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  loopId = bodyContext.setIsInLoop();
  if (afterContinue)
    TRY(afterContinue->check(bodyContext));
  return body->check(bodyContext);
}

JustError<ErrorLoc> WhileLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.startLoop(loopId);
  TRY(cond->checkMoves(checker));
  TRY(body->checkMoves(checker));
  if (afterContinue)
    TRY(afterContinue->checkMoves(checker));
  return checker.endLoop(loopId);
}

unique_ptr<Statement> WhileLoopStatement::transformImpl(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return make_unique<WhileLoopStatement>(codeLoc,
      exprFun(cond.get()),
      fun(body.get()),
      afterContinue ? fun(afterContinue.get()) : nullptr);
}

WithEvalError<StatementEvalResult> WhileLoopStatement::eval(Context& context) {
  StatementEvalResult res;
  auto forContext = context.getChild();
  const int maxIterations = 500;
  int countIter = 0;
  while (1) {
    auto bodyContext = forContext.getChild();
    auto condValue = TRY(cond->eval(bodyContext)).value;
    if (condValue->getType() != BuiltinType::BOOL)
    return EvalError::withError("Expected a compile-time value of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(condValue->getName()));
    auto value = dynamic_cast<CompileTimeValue*>(condValue);
    bool onePass = false;
    if (auto b = value->value.getValueMaybe<bool>()) {
      if (!*b)
        break;
    } else
      onePass = true;
    auto thisBody = body->deepCopy();
    auto bodyContext2 = bodyContext.getChild();
    if (auto res = thisBody->check(bodyContext2); !res)
      return EvalError::withError(res.get_error().toString());
    if (afterContinue)
      TRY(afterContinue->eval(bodyContext2));
    res.push_back(make_unique<StatementBlock>(codeLoc, makeVec(std::move(thisBody))));
    if (onePass)
      break;
    if (++countIter > maxIterations)
      return EvalError::withError("Static loop reached maximum number of iterations (" + to_string(maxIterations) + ")");
  }
  return std::move(res);
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool isBuiltIn)
    : Statement(l), path(p), isBuiltIn(isBuiltIn) {
}

JustError<ErrorLoc> ImportStatement::registerTypes(const Context& context, TypeRegistry* r, ASTCache& cache,
    const vector<string>& importDirs) {
  if (ast)
    return success;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    auto importPath = fs::path(importDir) / path;
    if (auto content = readFromFile(importPath.c_str())) {
      absolutePath = string(fs::canonical(importPath));
      ast = TRY(cache.getAST(*absolutePath));
      for (auto& elem : ast->elems)
        TRY(elem->registerTypes(context, r, cache, importDirs));
      break;
    }
  }
  if (!ast)
    return codeLoc.getError("Couldn't resolve import path: " + path);
  return success;
}

unique_ptr<Statement> ImportStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<ImportStatement>(codeLoc, path, isBuiltIn);
}

JustError<ErrorLoc> ImportStatement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  if (cache.isCurrentlyBuiltIn() && isBuiltIn) {
    ast = nullptr;
    return success;
  }
  if (cache.isCurrentlyImported(*absolutePath))
    return codeLoc.getError("Exported import cycle: " + combine(cache.getCurrentImports(), ", "));
  if (!cache.contains(*absolutePath)) {
    TRY(addExportedContext(primaryContext, cache, *ast, *absolutePath, isBuiltIn));
  } else
    INFO << "Import " << *absolutePath << " already cached";
  context.merge(cache.getContext(*absolutePath));
  return success;
}

JustError<ErrorLoc> ImportStatement::check(Context& context, bool) {
  return success;
}

WithEvalError<EvalResult> Expression::eval(const Context&) const {
  return EvalError::noEval();
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

JustError<ErrorLoc> EnumDefinition::registerTypes(const Context&, TypeRegistry* r) {
  if (registered)
    return success;
  registered = true;
  return r->addEnum(name, external, this).addCodeLoc(codeLoc);
}

unique_ptr<Statement> EnumDefinition::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = make_unique<EnumDefinition>(codeLoc, name);
  ret->elements = elements;
  return ret;
}

JustError<ErrorLoc> EnumDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  auto type = context.typeRegistry->getEnum(name);
  if (elements.empty())
    return codeLoc.getError("Enum requires at least one element");
  type->definition = this;
  type->elements = elements.transform([](auto elem) { return elem.first;});
  type->external = external;
  unordered_set<string> occurences;
  for (auto& e : elements)
    if (occurences.count(e.first))
      return e.second.getError("Duplicate enum element: " + quote(e.first));
  context.addType(name, type);
  context.setTypeFullyDefined(type);
  return success;
}

JustError<ErrorLoc> EnumDefinition::check(Context&, bool) {
  return success;
}

EnumConstant::EnumConstant(CodeLoc l, string name, CodeLoc elemLoc, string element)
    : Expression(l), enumName(name), enumElement(element), elemLoc(std::move(elemLoc)) {
}

WithErrorLine<Type*> EnumConstant::getTypeImpl(const Context& context) {
  auto type = TRY(context.getTypeFromString(IdentifierInfo(enumName, codeLoc)));
  if (auto enumType = dynamic_cast<EnumType*>(type)) {
    if (!context.isFullyDefined(enumType))
      return codeLoc.getError("Enum type " + quote(enumType->getName()) + " elements are not known in this context");
    if (auto ind = enumType->elements.findElement(enumElement)) {
      if (enumType->definition)
        context.languageIndex->addDefinition(elemLoc, enumElement.size(), enumType->definition->elements[*ind].second);
    } else
      return codeLoc.getError(quote(enumElement) + " is not an element of enum " + quote(enumName));
  } else
    return codeLoc.getError(quote(type->getName()) + " is not an enum type");
  return type;
}

WithEvalError<EvalResult> EnumConstant::eval(const Context& context) const {
  if (auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc))) {
    if (auto enumType = dynamic_cast<EnumType*>(*type)) {
      for (int i = 0; i < enumType->elements.size(); ++i)
        if (enumType->elements[i] == enumElement)
          return EvalResult{ CompileTimeValue::get(CompileTimeValue::EnumValue{enumType, i}), true};
    }
  }
  FATAL << "Unrecognized enum element - should have been discovered by the type checker";
  fail();
}

unique_ptr<Expression> EnumConstant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<EnumConstant>(codeLoc, enumName, elemLoc, enumElement);
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {
}

void ConceptDefinition::addFatPointer(ConceptDefinition::FatPointerInfo info) {
  for (auto& elem : fatPointers)
    if (elem.type == info.type)
      return;
  fatPointers.push_back(info);
}

void ConceptDefinition::addConceptType(ConceptType* conceptType) {
  if (!conceptInstances.contains(conceptType))
    conceptInstances.push_back(conceptType);
}

JustError<ErrorLoc> ConceptDefinition::addToContext(Context& context) {
  auto declarationsContext = context.getChild();
  if (!functions.empty() && concept->getContext().getAllFunctions().empty()) {
    // A bit iffy that we're adding the functions once but with a specific Context that happened to be first.
    for (int i = 0; i < templateInfo.params.size(); ++i) {
      auto& param = templateInfo.params[i];
      declarationsContext.addType(param.name, concept->getParams()[i]);
      if (templateInfo.variadic && i == templateInfo.params.size() - 1)
        declarationsContext.addUnexpandedTypePack(templateInfo.params[i].name, concept->modParams().back());
    }
    for (auto& function : functions) {
      if (function->isVirtual)
        return function->codeLoc.getError("Virtual functions are not allowed here");
      TRY(function->setFunctionSignature(declarationsContext, concept));
      TRY(function->check(declarationsContext));
      TRY(concept->modContext().addFunction(function->functionInfo).addCodeLoc(function->codeLoc));
    }
  }
  context.addConcept(name, concept);
  return success;
}

JustError<ErrorLoc> ConceptDefinition::check(Context& context, bool) {
  return success;
}

unique_ptr<Statement> ConceptDefinition::transformImpl(const StmtTransformFun& f1, const ExprTransformFun&) const {
  auto ret = make_unique<ConceptDefinition>(codeLoc, name);
  for (auto& f : functions)
    ret->functions.push_back(cast<FunctionDefinition>(f1(f.get())));
  ret->templateInfo = templateInfo;
  return ret;
}

JustError<ErrorLoc> ConceptDefinition::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!concept) {
    concept = new Concept(name, this, Context(primaryContext.typeRegistry, primaryContext.languageIndex, true),
        templateInfo.variadic);
    for (int i = 0; i < templateInfo.params.size(); ++i) {
      auto& param = templateInfo.params[i];
      if (param.type)
        return param.codeLoc.getError("Concept value template parameters are not supported.");
      concept->modParams().push_back(new TemplateParameterType(param.name, param.codeLoc));
    }
    r->addConcept(name, concept);
  }
  return success;
}

JustError<ErrorLoc> BreakStatement::check(Context& context, bool) {
  if (auto id = context.getLoopId()) {
    loopId = *id;
    return success;
  } else
    return codeLoc.getError("Break statement outside of a loop");
}

unique_ptr<Statement> BreakStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = make_unique<BreakStatement>(codeLoc);
  ret->loopId = loopId;
  return ret;
}

JustError<ErrorLoc> BreakStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.breakStatement(loopId);
  return success;
}

JustError<ErrorLoc> ContinueStatement::check(Context& context, bool) {
  if (auto id = context.getLoopId()) {
    loopId = *id;
    return success;
  } else
    return codeLoc.getError("Continue statement outside of a loop");
}

unique_ptr<Statement> ContinueStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<ContinueStatement>(codeLoc);
}

JustError<ErrorLoc> ContinueStatement::checkMovesImpl(MoveChecker& checker) const {
  return checker.continueStatement().addCodeLoc(codeLoc);
}

ArrayLiteral::ArrayLiteral(CodeLoc codeLoc) : Expression(codeLoc) {
}

WithErrorLine<Type*> ArrayLiteral::getTypeImpl(const Context& context) {
  auto typeTmp = TRY(typeId ? context.getTypeFromString(*typeId, false) : getType(context, contents[0]));
  auto ret = typeTmp->removeReference();
  for (int i = 0; i < contents.size(); ++i) {
    if (i > 0 || !typeId)
      typeTmp = TRY(getType(context, contents[i]));
    TRY(getVariableInitializationError("construct array", context, ret, typeTmp, contents[i]).addCodeLoc(contents[i]->codeLoc));
  }
  type = ret;
  return (Type*)ArrayType::get(ret, CompileTimeValue::get((int)contents.size()));
}

unique_ptr<Expression> ArrayLiteral::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = make_unique<ArrayLiteral>(codeLoc);
  for (auto& elem : contents)
    ret->contents.push_back(fun(elem.get()));
  ret->typeId = typeId;
  return ret;
}

JustError<ErrorLoc> ArrayLiteral::checkMoves(MoveChecker& checker) const {
  for (auto& e : contents)
    TRY(e->checkMoves(checker));
  return success;
}

WithErrorLine<Type*> getType(const Context& context, unique_ptr<Expression>& expr, bool evaluateAtCompileTime) {
  auto type = TRY(expr->getTypeImpl(context));
  if (evaluateAtCompileTime) {
    if (auto type = expr->eval(context)) {
      if (type->isConstant) {
          auto c = make_unique<Constant>(expr->codeLoc, type->value);
          auto refValue = type->value->removeValueReference();
          if (refValue != type->value)
            c->refValue = refValue;
          expr = std::move(c);
        }
    } else
    if (type.get_error().canEval)
      return expr->codeLoc.getError(type.get_error().error);
  }
  return type;
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::transform(const StmtTransformFun& fun, const ExprTransformFun&) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.ids = ids;
  ret.block = cast<StatementBlock>(fun(block.get()));
  return ret;
}

ExternConstantDeclaration::ExternConstantDeclaration(CodeLoc l, IdentifierInfo type, string identifier)
  : Statement(l), type(type), identifier(identifier) {
}

JustError<ErrorLoc> ExternConstantDeclaration::addToContext(Context& context) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  realType = TRY(context.getTypeFromString(type));
  if (realType == BuiltinType::VOID)
    return codeLoc.getError("Can't declare constant of type " + quote(BuiltinType::VOID->getName()));
  context.addVariable(identifier, ReferenceType::get(realType), codeLoc);
  return success;
}

unique_ptr<Statement> ExternConstantDeclaration::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<ExternConstantDeclaration>(codeLoc, type, identifier);
}

LambdaExpression::LambdaExpression(CodeLoc l, vector<FunctionParameter> params, unique_ptr<StatementBlock> block,
    optional<IdentifierInfo> returnType, LambdaCaptureInfo captureInfo)
    : Expression(l), parameters(std::move(params)), block(std::move(block)), returnType(std::move(returnType)),
      captureInfo(std::move(captureInfo)) {
}

static Type* getCapturedType(Type* input, LambdaCaptureType type) {
  switch (type) {
    case LambdaCaptureType::REFERENCE:
      return convertReferenceToPointer(input);
    case LambdaCaptureType::COPY:
    case LambdaCaptureType::IMPLICIT_COPY:
    case LambdaCaptureType::MOVE:
      return input->removeReference();
  }
}

WithErrorLine<vector<LambdaCapture>> LambdaExpression::setLambda(Context& context) {
  vector<LambdaCapture> ret;
  for (auto& capture : captureInfo.captures) {
    if (auto type = context.getTypeOfVariable(capture.name, capture.codeLoc)) {
      auto underlying = (*type)->removeReference();
/*      if (capture.type == LambdaCaptureType::IMPLICIT_COPY && !context.canConvert(*type, underlying))
        return capture.codeLoc.getError("Variable " + capture.name + " of type " +
            quote(underlying->getName()) + " can't be captured by implicit copy");*/
      if (capture.type == LambdaCaptureType::IMPLICIT_COPY) {
        if (auto f = getImplicitCopyFunction(context, capture.codeLoc, underlying)) {
          TRY((*f)->getDefinition()->addInstance(context, *f));
          functionCalls.push_back(*f);
        } else
          return capture.codeLoc.getError("No implicit copy function defined for type " +
              quote(underlying->getName())+ "\n" + f.get_error().error);
      }
      if (capture.type == LambdaCaptureType::COPY) {
        if (auto f = getCopyFunction(context, capture.codeLoc, underlying)) {
          TRY((*f)->getDefinition()->addInstance(context, *f));
          functionCalls.push_back(*f);
        } else
          return capture.codeLoc.getError("No copy function defined for type " +
              quote(underlying->getName())+ "\n" + f.get_error().error);
      }
      ret.push_back(LambdaCapture{capture.name, getCapturedType(*type, capture.type), capture.type});
    } else
      return capture.codeLoc.getError(type.get_error());
  }
  context.setLambda(&captureInfo);
  return ret;
}

WithErrorLine<Type*> LambdaExpression::getTypeImpl(const Context& context) {
  if (type)
    return type;
  Type* retType = nullptr;
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (returnType)
    retType = TRY(context.getTypeFromString(*returnType));
  auto bodyContext = context.getChild();
  ReturnTypeChecker returnChecker(retType);
  bodyContext.addReturnTypeChecker(&returnChecker);
  auto captureTypes = TRY(setLambda(bodyContext));
  vector<Type*> params;
  type = LambdaType::get(context.getTemplateParams().value_or(vector<Type*>()));
  type->captures = captureTypes;
  for (auto& param : parameters)
    type->parameterNames.push_back(param.name);
  params.push_back(PointerType::get(type));
  set<string> paramNames;
  for (auto& param : parameters) {
    auto type = TRY(bodyContext.getTypeFromString(param.type));
    if (type == BuiltinType::VOID)
      return param.codeLoc.getError("Function parameter may not have " + quote(type->getName()) + " type");
    auto varType = param.isMutable ? (Type*)MutableReferenceType::get(type) : (Type*)ReferenceType::get(type);
    params.push_back(type);
    if (param.name) {
      bodyContext.addVariable(*param.name, varType, param.codeLoc);
      if (paramNames.count(*param.name))
        return param.codeLoc.getError("Duplicate function parameter name: " + quote(*param.name));
      paramNames.insert(*param.name);
    }
  }
  auto bodyContext2 = bodyContext.getChild();
  // Only used to prevent from modifying comptime mutable values. Make a new flag if needed.
  bodyContext2.setIsInBranch();
  TRY(block->check(bodyContext2));
  type->captures.append(captureInfo.implicitCaptures);
  if (!returnType)
    retType = returnChecker.getReturnType();
  considerAddingVoidReturn(bodyContext2, block.get(), retType);
  if (!type->functionInfo) {
    FunctionSignature functionType(retType, params, {});
    auto functioInfo = FunctionInfo::getImplicit("invoke"s, std::move(functionType));
    type->functionInfo = std::move(functioInfo);
  }
  TRY(checkBodyMoves());
  if (!block->hasReturnStatement() && retType != BuiltinType::VOID)
    return block->codeLoc.getError("Not all paths lead to a return statement in a lambda expression returning non-void");
  type->body = cast<Statement>(make_unique<ExternalStatement>(block.get()));
  type->destructorCalls = getDestructorCalls(codeLoc,
      parameters.transform([](auto& p) { return *p.name;}),
      getSubsequence(type->functionInfo->type.params, 1));
  for (auto& elem : type->destructorCalls)
    if (elem)
      TRY(elem->check(bodyContext));
  vector<unique_ptr<Statement>> toDestruct;
  for (auto& capture : type->captures) {
    if (capture.captureType != LambdaCaptureType::REFERENCE && capture.type->hasDestructor()) {
      toDestruct.push_back(getDestructorStatement(codeLoc, capture.name));
      CHECK(toDestruct.back()->check(bodyContext));
      for (auto& elem : captureInfo.captures)
        if (elem.name == capture.name)
          elem.hasConstructor = true;
    }
    if (capture.captureType == LambdaCaptureType::IMPLICIT_COPY) {
      TRY(getImplicitCopyFunction(bodyContext, codeLoc, capture.type).get()->addInstance(bodyContext));
    }
  }
  if (!toDestruct.empty())
    type->destructor = make_unique<StatementBlock>(codeLoc, std::move(toDestruct));
  return type;
}

JustError<ErrorLoc> LambdaExpression::checkBodyMoves() const {
  MoveChecker moveChecker;
  for (auto& p : parameters)
    if (p.name)
      moveChecker.addVariable(*p.name);
  return block->checkMoves(moveChecker);
}

JustError<ErrorLoc> LambdaExpression::checkMoves(MoveChecker& checker) const {
  for (auto& capture : captureInfo.captures) {
    if (capture.type == LambdaCaptureType::MOVE)
      TRY(checker.moveVariable(capture.codeLoc, capture.name).addCodeLoc(capture.codeLoc));
    else
      TRY(checker.getUsageError(capture.name).addCodeLoc(capture.codeLoc));
  }
  return success;
}

unique_ptr<Expression> LambdaExpression::transform(const StmtTransformFun& fun, const ExprTransformFun& exprFun) const {
  auto ret = make_unique<LambdaExpression>(codeLoc, parameters, cast<StatementBlock>(block->transform(fun, exprFun)), returnType, captureInfo);
  return ret;
}

CountOfExpression::CountOfExpression(CodeLoc l, string id) : Expression(l), identifier(std::move(id)) {
}

WithErrorLine<Type*> CountOfExpression::getTypeImpl(const Context& context) {
  if (auto p = context.getExpandedVariablePack())
    if (p->first == identifier)
      return (Type*)BuiltinType::INT;
  if (auto p = context.getExpandedTypePack())
    if (p->first == identifier)
      return (Type*)BuiltinType::INT;
  if (auto p = context.getUnexpandedTypePack())
    if (p->first == identifier)
      return (Type*)BuiltinType::INT;
  if (auto p = context.getUnexpandedVariablePack())
    if (p->first == identifier)
      return (Type*)BuiltinType::INT;
  return codeLoc.getError("Operator countof requires a type or variable pack");
}

unique_ptr<Expression> CountOfExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<CountOfExpression>(codeLoc, identifier);
}

WithEvalError<EvalResult> CountOfExpression::eval(const Context& context) const {
  if (auto t = context.getExpandedVariablePack()) {
    if (t->first == identifier)
      return EvalResult{CompileTimeValue::get(t->second.size()), true};
  }
  if (auto t = context.getExpandedTypePack())
    if (t->first == identifier)
      return EvalResult{CompileTimeValue::get(t->second.size()), true};
  return EvalResult{CompileTimeValue::getTemplateValue(BuiltinType::INT, "countof"), false};
}


VariablePackElement::VariablePackElement(CodeLoc l, string packName, unique_ptr<Expression> index)
    : Expression(l), packName(std::move(packName)), index(std::move(index)) {
}

WithErrorLine<Type*> VariablePackElement::getTypeImpl(const Context& context) {
  auto indexValue1 = TRY(index->eval(context).addNoEvalError(
      index->codeLoc.getError("Unable to evaluate constant expression at compile-time")));
  auto indexValue = dynamic_cast<CompileTimeValue*>(indexValue1.value->removeValueReference());
  if (indexValue->getType() != BuiltinType::INT)
    return index->codeLoc.getError("Expected subscript index of type " + quote(BuiltinType::INT->getName())
        + ",  got " + quote(indexValue->getType()->getName()));
  const int intValue = indexValue->value.getValueMaybe<int>().value_or(0);
  auto getType = [&](const vector<Type*>& types) -> WithErrorLine<Type*> {
    if (intValue >= 0 && intValue < types.size())
      return types[intValue];
    return codeLoc.getError("Index out of range: " + to_string(intValue) + ". Expected value between 0 and "
        + to_string(types.size()));
  };
  if (auto pack = context.getExpandedVariablePack())
    if (pack->first == packName) {
      codegenName = getExpandedParamName(packName, intValue);
      return getType(pack->second);
    }
  if (auto pack = context.getUnexpandedVariablePack())
    if (pack->first == packName)
      return pack->second;
  return codeLoc.getError(quote(packName) + " is neither a type or variable pack name.");
}

unique_ptr<Expression> VariablePackElement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return cast<Expression>(make_unique<VariablePackElement>(codeLoc, packName, index->transform(f1, f2)));
}

unique_ptr<MemberAccessExpression> MemberAccessExpression::getPointerAccess(CodeLoc l, unique_ptr<Expression> lhs, string id) {
  return make_unique<MemberAccessExpression>(l,
      make_unique<UnaryExpression>(l, Operator::POINTER_DEREFERENCE, std::move(lhs)),
      id);
}

MemberAccessExpression::MemberAccessExpression(CodeLoc l , unique_ptr<Expression> lhs, string id)
  : Expression(l), lhs(std::move(lhs)), identifier(id) { }

static JustError<ErrorLoc> initializeDestructor(const Context& context, Type* type, const string& member,
    CodeLoc codeLoc, FunctionInfo*& destructorCall, bool& mainDestructor) {
  if (auto structType = dynamic_cast<StructType*>(type)) {
    if (structType->destructor) {
      mainDestructor = true;
      destructorCall = TRY(getDestructor(context, structType, codeLoc));
      TRY(destructorCall->addInstance(context));
    } else
    if (structType->hasDestructor()) {
      for (int i = 0; i < structType->members.size(); ++i)
        if (structType->members[i].name == member) {
          destructorCall = TRY(getFunction(context, codeLoc, IdentifierInfo("destruct_except", codeLoc),
              {CompileTimeValue::get(i)}, {PointerType::get(type)}, {codeLoc}, false));
          TRY(destructorCall->addInstance(context));
        }
    }
  }
  return success;
}

WithErrorLine<Type*> MemberAccessExpression::getTypeImpl(const Context& context) {
  auto leftType = TRY(lhs->getTypeImpl(context));
  if (auto res = leftType->getSizeError(context); !res)
    return codeLoc.getError(leftType->getName() + res.get_error());
  isUnion = leftType->removeReference()->getType() == BuiltinType::UNION_TYPE;
  auto ret = TRY(leftType->getTypeOfMember(identifier).addCodeLoc(codeLoc));
  TRY(initializeDestructor(context, leftType, identifier, codeLoc, destructorCall, isMainDestructor));
  if (auto loc = leftType->getMemberLoc(identifier))
    context.languageIndex->addDefinition(codeLoc, identifier.size(), *loc);
  return ret;
}

unique_ptr<Expression> MemberAccessExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return make_unique<MemberAccessExpression>(codeLoc, lhs->transform(fun1, fun2), identifier);
}

JustError<ErrorLoc> MemberAccessExpression::checkMoves(MoveChecker& checker) const {
  return lhs->checkMoves(checker);
}

TernaryExpression::TernaryExpression(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Expression> e1, unique_ptr<Expression> e2)
  : Expression(l), condExpr(std::move(cond)), e1(std::move(e1)), e2(std::move(e2)) {
}

WithErrorLine<Type*> TernaryExpression::getTypeImpl(const Context& context) {
  auto condType = TRY(getType(context, condExpr));
  if (!context.canConvert(condType, BuiltinType::BOOL))
    return condExpr->codeLoc.getError("Expected expression of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(condType->getName()));
  auto t1 = TRY(getType(context, e1));
  auto t2 = TRY(getType(context, e2));
  if (context.canConvert(t1, t2, e1))
    return t2;
  else
  if (context.canConvert(t2, t1, e2))
    return t1;
  else
    return e1->codeLoc.getError("Ternary operator operands have incompatible types: "
        + quote(t1->getName()) + " and " + quote(t2->getName()));
}

WithEvalError<EvalResult> TernaryExpression::eval(const Context& context) const {
  auto cond = TRY(condExpr->eval(context));
  if (auto value = dynamic_cast<CompileTimeValue*>(cond.value)) {
    auto res1 = TRY(e1->eval(context));
    auto res2 = TRY(e2->eval(context));
    if (auto boolValue = value->value.getValueMaybe<bool>())
      return *boolValue ? res1 : res2;
    return EvalResult{CompileTimeValue::getTemplateValue(res1.value->getType(), "ternary_result"), false};
  }
  return EvalError::noEval();
}

unique_ptr<Expression> TernaryExpression::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return make_unique<TernaryExpression>(codeLoc, f2(condExpr.get()), f2(e1.get()), f2(e2.get()));
}

JustError<ErrorLoc> TernaryExpression::checkMoves(MoveChecker& checker) const {
  TRY(condExpr->checkMoves(checker));
  checker.startBlock();
  checker.newAlternative();
  TRY(e1->checkMoves(checker));
  checker.clearStatementUsages();
  checker.newAlternative();
  TRY(e2->checkMoves(checker));
  checker.endBlock();
  return success;
}

FatPointerConversion::FatPointerConversion(CodeLoc l, vector<FunctionInfo*> functions, Type* toType, Type* argType, unique_ptr<Expression> arg, ConceptType* conceptType)
    : Expression(l), toType(toType), argType(argType), arg(std::move(arg)), conceptType(std::move(conceptType)), functions(std::move(functions)) {
}

WithError<vector<FunctionInfo*>> getRequiredFunctionsForConceptType(const Context& context,
    const Concept& concept, CodeLoc codeLoc) {
  vector<FunctionInfo*> ret;
  for (auto& fun : concept.getContext().getAllFunctions()) {
    auto candidates = context.getFunctions(translateDestructorId(fun->id), false);
    if (candidates.empty())
      return "No candidates found for function " + fun->prettyString();
    ret.push_back(TRY(getFunction(context, codeLoc, translateDestructorId(fun->id),
        std::move(candidates), {}, fun->type.params,
        vector<CodeLoc>(fun->type.params.size(), codeLoc))));
  }
  return ret;
}

WithErrorLine<Type*> FatPointerConversion::getTypeImpl(const Context& context) {
  return toType;
}

unique_ptr<Expression> FatPointerConversion::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return f2(arg.get());
}

JustError<ErrorLoc> FatPointerConversion::checkMoves(MoveChecker& c) const {
  return arg->checkMoves(c);
}

UncheckedStatement::UncheckedStatement(CodeLoc l, unique_ptr<Statement> elem) : Statement(l), elem(std::move(elem)) {
}

bool UncheckedStatement::hasReturnStatement() const {
  return elem->hasReturnStatement();
}

JustError<ErrorLoc> UncheckedStatement::check(Context& context, bool) {
  if (!context.getTemplateParams())
    return elem->check(context);
  return success;
}

JustError<ErrorLoc> UncheckedStatement::checkMovesImpl(MoveChecker& checker) const {
  return elem->checkMovesImpl(checker);
}

unique_ptr<Statement> UncheckedStatement::transformImpl(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return make_unique<UncheckedStatement>(codeLoc, elem->transform(f1, f2));
}

AttributeDefinition::AttributeDefinition(CodeLoc l, string name) : Statement(l), name(std::move(name)) {
}

JustError<ErrorLoc> AttributeDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflictExcludingFunctions(name, "attribute").addCodeLoc(codeLoc));
  auto t = AttributeType::get(name);
  context.addType(name, t);
  context.setTypeFullyDefined(t);
  return success;
}

JustError<ErrorLoc> AttributeDefinition::check(Context&, bool) {
  return success;
}

unique_ptr<Statement> AttributeDefinition::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<AttributeDefinition>(codeLoc, name);
}

ExternalStatement::ExternalStatement(Statement* s) : Statement(s->codeLoc), elem(s) {}

bool ExternalStatement::hasReturnStatement() const {
  return elem->hasReturnStatement();
}

JustError<ErrorLoc> ExternalStatement::check(Context& context, bool b) {
  return elem->check(context, b);
}

JustError<ErrorLoc> ExternalStatement::checkMovesImpl(MoveChecker& checker) const {
  return elem->checkMoves(checker);
}

unique_ptr<Statement> ExternalStatement::transformImpl(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return elem->transform(f1, f2);
}

StatementExpression::StatementExpression(CodeLoc l, vector<unique_ptr<Statement>> s, unique_ptr<Expression> v)
    : Expression(l), statements(std::move(s)), value(std::move(v)) {
}

WithErrorLine<Type*> StatementExpression::getTypeImpl(const Context& context) {
  auto bodyContext = context.getChild();
  for (auto& s : statements)
    TRY(s->check(bodyContext));
  return TRY(getType(bodyContext, value));
}

unique_ptr<Expression> StatementExpression::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return make_unique<StatementExpression>(codeLoc,
      statements.transform([&](auto& s) { return s->transform(f1, f2); }),
      value->transform(f1, f2));
}

JustError<ErrorLoc> StatementExpression::checkMoves(MoveChecker& checker) const {
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& s : statements)
    TRY(s->checkMoves(checker));
  TRY(value->checkMoves(checker));
  return success;
}

MixinStatement::MixinStatement(CodeLoc l, unique_ptr<Expression> value) : Statement(l), value(std::move(value)) {
}

JustError<ErrorLoc> MixinStatement::check(Context& context, bool) {
  if (context.getTemplateParams()) {
    returns = true;
    return success;
  }
  auto evalResult = TRY(value->eval(context)
      .addNoEvalError(value->codeLoc.getError("Unable to evaluate expression at compile-time"))).value;
  string str =
      *dynamic_cast<CompileTimeValue*>(TRY(evalResult->convertTo(BuiltinType::STRING).addCodeLoc(value->codeLoc)))->value.getReferenceMaybe<string>();
  istringstream iss("\"" + str + "\"");
  iss >> std::quoted(str);
  auto addMixinCode = [&] (const ErrorLoc& error) {
    auto ret = error;
    ret.error = "When parsing mixin:\n" + str + "\n:" + ret.error;
    return ret;
  };
  auto tokens = TRY(lex(str, value->codeLoc, "end of expression").transform_error(addMixinCode));
  result = TRY(parseStatement(tokens, false).transform_error(addMixinCode));
  if (!result)
    return tokens.peek().codeLoc.getError("Expected a non-empty mixin parameter");
  if (!tokens.peek().contains<EofToken>())
    return tokens.peek().codeLoc.getError("Expected a single statement in mixin parameter");
  TRY(result->check(context, false).transform_error(addMixinCode));
  returns = result->hasReturnStatement();
  return success;
}


unique_ptr<Statement> MixinStatement::transformImpl(const StmtTransformFun&, const ExprTransformFun& f) const {
  return make_unique<MixinStatement>(codeLoc, f(value.get()));
}

bool MixinStatement::hasReturnStatement() const {
  return returns;
}

JustError<ErrorLoc> MixinStatement::checkMovesImpl(MoveChecker& checker) const {
  if (result)
    return result->checkMoves(checker);
  return success;
}

StaticStatement::StaticStatement(CodeLoc l, unique_ptr<Statement> value) : Statement(l), value(std::move(value)) {
}

JustError<ErrorLoc> StaticStatement::check(Context& context, bool) {
  value->attributes = attributes;
  results = TRY(value->eval(context)
      .addNoEvalError(value->codeLoc.getError("Unable to evaluate statement at compile-time")));
  return success;
}

unique_ptr<Statement> StaticStatement::transformImpl(const StmtTransformFun& f, const ExprTransformFun&) const {
  return make_unique<StaticStatement>(codeLoc, f(value.get()));
}

bool StaticStatement::hasReturnStatement() const {
  for (auto& s : results)
    if (s->hasReturnStatement())
      return true;
  return false;
}

JustError<ErrorLoc> StaticStatement::checkMovesImpl(MoveChecker& checker) const {
  return value->checkMovesImpl(checker);
}

bool StaticStatement::canHaveAttributes() const {
  return value->canHaveAttributes();
}

unique_ptr<Statement> getForLoop(CodeLoc l, unique_ptr<VariableDeclaration> decl, unique_ptr<Expression> cond,
    unique_ptr<Expression> iter, unique_ptr<Statement> body) {
  auto ret = make_unique<StatementBlock>(l);
  decl->isMutable = true;
  ret->elems.push_back(std::move(decl));
  auto exprStmt = make_unique<ExpressionStatement>(std::move(iter));
  exprStmt->canDiscard = true;
  ret->elems.push_back(make_unique<WhileLoopStatement>(l, std::move(cond), std::move(body), std::move(exprStmt)));
  return ret;
}

unique_ptr<Statement> getRangedLoop(CodeLoc l, string iterator, unique_ptr<Expression> container,
    unique_ptr<Statement> body) {
  auto ret = make_unique<StatementBlock>(l);
  static int cnt = 0;
  ++cnt;
  auto containerId = "container" + to_string(cnt);
  auto itEndId = "itEnd" + to_string(cnt);
  ret->elems.push_back(make_unique<AliasDeclaration>(l, containerId, std::move(container)));
  auto itDecl = make_unique<VariableDeclaration>(l, none, iterator,
      make_unique<FunctionCall>(IdentifierInfo("begin", l), make_unique<Variable>(IdentifierInfo(containerId, l)), true));
  itDecl->isMutable = true;
  ret->elems.push_back(std::move(itDecl));
  ret->elems.push_back(make_unique<VariableDeclaration>(l, none, itEndId,
      make_unique<FunctionCall>(IdentifierInfo("end", l), make_unique<Variable>(IdentifierInfo(containerId, l)), true)));
  auto whileBody = make_unique<StatementBlock>(l);
  whileBody->elems.push_back(std::move(body));
  auto incExpr = make_unique<ExpressionStatement>(make_unique<UnaryExpression>(l, Operator::INCREMENT,
      make_unique<Variable>(IdentifierInfo(iterator, l))));
  incExpr->canDiscard = true;
  ret->elems.push_back(make_unique<WhileLoopStatement>(l, BinaryExpression::get(l, Operator::NOT_EQUAL,
      make_unique<Variable>(IdentifierInfo(iterator, l)), make_unique<Variable>(IdentifierInfo(itEndId, l))),
      std::move(whileBody), std::move(incExpr)));
  return ret;
}

TypeAliasDeclaration::TypeAliasDeclaration(CodeLoc loc, string identifier, IdentifierInfo type)
  : Statement(loc), identifier(std::move(identifier)), typeId(std::move(type)) {
}

JustError<ErrorLoc> TypeAliasDeclaration::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!type) {
    type = TRY(primaryContext.getTypeFromString(typeId));
    TRY(primaryContext.checkNameConflict(identifier, "type alias").addCodeLoc(codeLoc));
    r->addAlias(identifier, type, codeLoc);
  }
  return success;
}

JustError<ErrorLoc> TypeAliasDeclaration::addToContext(Context&) {
  return success;
}

JustError<ErrorLoc> TypeAliasDeclaration::check(Context&, bool) {
  return success;
}

unique_ptr<Statement> TypeAliasDeclaration::transformImpl(const StmtTransformFun&, const ExprTransformFun&) const {
  return make_unique<TypeAliasDeclaration>(codeLoc, identifier, typeId);
}
