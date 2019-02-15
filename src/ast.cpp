#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"
#include "code_loc.h"
#include "identifier_type.h"

Node::Node(CodeLoc l) : codeLoc(l) {}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<VariableDeclaration> d, unique_ptr<Expression> c,
    unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), declaration(std::move(d)), condition(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, SCompileTimeValue v) : Expression(l), value(v) {
  INFO << "Created constant " << v->getName() << " of type " << v->getType();
}

Variable::Variable(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {
  INFO << "Parsed variable " << id.prettyString();
}

Variable::Variable(CodeLoc l, string s) : Variable(l, IdentifierInfo(s, l)) {
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(std::move(id)) {
  INFO << "Function call " << id.prettyString();;
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id, unique_ptr<Expression> arg) : FunctionCall(l, id) {
  arguments.push_back(std::move(arg));
 }

unique_ptr<FunctionCall> FunctionCall::constructor(CodeLoc l, SType type) {
  auto ret = unique<FunctionCall>(l, Private{});
  vector<SType> templateArgs;
  if (auto structType = type.dynamicCast<StructType>()) {
    templateArgs = structType->templateParams;
    type = structType->parent.get();
  }
  ret->templateArgs = std::move(templateArgs);
  ret->identifierType = IdentifierType(type);
  return ret;
}

VariableDeclaration::VariableDeclaration(CodeLoc l, optional<IdentifierInfo> t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  string type = "auto";
  if (t)
    type = t->prettyString();
  INFO << "Declared variable " << quote(id) << " of type " << quote(type);
}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, FunctionId name)
  : Statement(l), returnType(std::move(r)), name(name) {}

SType Constant::getTypeImpl(Context&) {
  return value->getType();
}

nullable<SType> Constant::eval(const Context&) const {
  return (SType) value;
}

unique_ptr<Expression> Constant::replace(SType from, SType to) const {
  return unique<Constant>(codeLoc, value->replace(from, to).dynamicCast<CompileTimeValue>());
}

SType Variable::getTypeImpl(Context& context) {
  optional<string> varError;
  if (auto id = identifier.asBasicIdentifier()) {
    if (auto varType = context.getTypeOfVariable(*id))
      return *varType;
    else
      varError = varType.get_error();
  }
  if (auto t = context.getTypeFromString(identifier))
    return t.get()->getType();
  codeLoc.error(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

nullable<SType> Variable::eval(const Context& context) const {
  if (auto res = context.getTypeFromString(identifier))
    return *res;
  else
    return nullptr;
}

unique_ptr<Expression> Variable::replace(SType from, SType to) const {
  return unique<Variable>(codeLoc, identifier);
}

nullable<SType> Variable::getDotOperatorType(Expression* left, Context& callContext) {
  if (left)
    if (auto id = identifier.asBasicIdentifier())
      if (auto structType = left->getTypeImpl(callContext)->getUnderlying().dynamicCast<StructType>())
        return SType(MutableReferenceType::get(structType->getTypeOfMember(*id).get(codeLoc)));
  return nullptr;
}

static bool isExactArg(SType arg, SType param) {
  bool byValue = param == arg->getUnderlying();
  bool byConstRef = param.dynamicCast<ReferenceType>() &&
      !arg.dynamicCast<MutableReferenceType>() &&
      param->getUnderlying() == arg->getUnderlying();
  bool byRef = param == arg;
  /*cout << "Passing " << arg->getName() << " to " << param->getName() << endl;
  if (byValue) cout << "By value "; if (byConstRef) cout << "By const ref "; if (byRef) cout << "By ref ";
  cout << endl;*/
  return byValue || byConstRef || byRef;
}

static bool exactArgs(const vector<SType>& argTypes, const FunctionType& f) {
  if (f.params.size() != argTypes.size() || !f.templateParams.empty())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!isExactArg(argTypes[i], f.params[i].type))
      return false;
  return true;
}

static bool exactFirstArg(const vector<SType>& argTypes, const FunctionType& overload) {
  return !argTypes.empty() && !overload.params.empty() && isExactArg(argTypes[0], overload.params[0].type);
}

static bool fromConcept(const vector<SType>&, const FunctionType& f) {
  return f.fromConcept;
}

static bool userDefinedConstructor(const vector<SType>&, const FunctionType& f) {
  return !f.generatedConstructor;
}

static void filterOverloads(vector<SFunctionInfo>& overloads, const vector<SType>& argTypes) {
  auto filter = [&] (auto fun, const char* method) {
    auto worse = overloads;
    overloads.clear();
    for (auto& overload : worse)
      if (fun(argTypes, overload->type)) {
        overloads.push_back(overload);
        //cout << overload.toString() << " chosen by " << method << endl;
      }
    if (overloads.empty())
      overloads = worse;
  };
  filter(&exactArgs, "all args exact");
  filter(&exactFirstArg, "first arg exact");
  // sometimes a function is both in the global context and in the concept, so prefer the one in the concept
  filter(&fromConcept, "non concept");
  filter(&userDefinedConstructor, "user defined constructor");
}


static WithErrorLine<SFunctionInfo> handleOperatorOverloads(Context& context, CodeLoc codeLoc, Operator op,
    vector<SType> types, vector<CodeLoc> argLocs) {
  vector<SFunctionInfo> overloads;
  if (auto fun = context.getBuiltinOperator(op, types))
    overloads.push_back(fun.get());
  vector<string> errors;
  for (auto fun : context.getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, types, argLocs, {}))
      overloads.push_back(inst.get());
    else
      errors.push_back("Candidate: " + fun->prettyString() + ": " + inst.get_error().error);
  filterOverloads(overloads, types);
  if (overloads.size() == 1) {
    //cout << "Chosen overload " << overloads[0].toString() << endl;
    return overloads[0];
  } else {
      string error = "No overload found for operator: " + quote(getString(op)) + " with argument types: " +
          joinTypeList(types);
      for (auto& f : overloads)
        error += "\nCandidate: " + f->prettyString();
      for (auto& f : errors)
        error += "\n" + f;
      return codeLoc.getError(error);
  }
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr) {
  switch (op) {
    case Operator::NOT_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::EQUALS, std::move(expr)));
    case Operator::LESS_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::MORE_THAN, std::move(expr)));
    case Operator::MORE_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::LESS_THAN, std::move(expr)));
    case Operator::POINTER_MEMBER_ACCESS:
      return get(loc, Operator::MEMBER_ACCESS,
          unique<UnaryExpression>(loc, Operator::POINTER_DEREFERENCE, std::move(expr[0])),
          std::move(expr[1]));
    default:
      return unique<BinaryExpression>(Private{}, loc, op, std::move(expr));
  }
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, unique_ptr<Expression> a,
    unique_ptr<Expression> b) {
  return get(loc, op, makeVec<unique_ptr<Expression>>(std::move(a), std::move(b)));
}

BinaryExpression::BinaryExpression(BinaryExpression::Private, CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr)
    : Expression(loc), op(op), expr(std::move(expr)) {}

SType BinaryExpression::getTypeImpl(Context& context) {
  auto left = getType(context, expr[0]);
  switch (op) {
    case Operator::POINTER_MEMBER_ACCESS:
    case Operator::NOT_EQUAL:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
      FATAL << "This operator should have been rewritten";
      fail();
    case Operator::MEMBER_ACCESS: {
      if (auto rightType = expr[1]->getDotOperatorType(expr[0].get(), context)) {
        if (!left.dynamicCast<ReferenceType>() && !left.dynamicCast<MutableReferenceType>())
          rightType = rightType->getUnderlying();
        else if (left.dynamicCast<ReferenceType>() && rightType.get().dynamicCast<MutableReferenceType>())
          rightType = ReferenceType::get(rightType->getUnderlying());
        return rightType.get();
      } else
        codeLoc.error("Bad use of operator " + quote("."));
    }
    default: {
      auto right = getType(context, expr[1]);
      if (auto fun = handleOperatorOverloads(context, codeLoc, op,
          transform(expr, [&](auto& e) { return e->getTypeImpl(context);}),
          transform(expr, [&](auto& e) { return e->codeLoc;}))) {
        functionInfo = *fun;
        if (auto parent = functionInfo->parent)
          if (parent->definition)
            parent->definition->addInstance(context, functionInfo.get());
        return functionInfo->type.retVal;
      } else
        fun.get_error().execute();
    }
  }
}

nullable<SType> BinaryExpression::eval(const Context& context) const {
  if (auto value1 = expr[0]->eval(context)) {
    if (auto value2 = expr[1]->eval(context))
      return ::eval(op, {value1.get(), value2.get()});
  }
  return nullptr;
}

unique_ptr<Expression> BinaryExpression::replace(SType from, SType to) const {
  return get(codeLoc, op, expr[0]->replace(from, to), expr[1]->replace(from, to));
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

SType UnaryExpression::getTypeImpl(Context& context) {
  nullable<SType> ret;
  auto right = getType(context, expr, op != Operator::GET_ADDRESS);
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right->getName())};
  if (auto fun = handleOperatorOverloads(context, codeLoc, op, {expr->getTypeImpl(context)}, {expr->codeLoc})) {
    functionInfo = *fun;
    if (auto parent = functionInfo->parent)
      if (parent->definition)
        parent->definition->addInstance(context, functionInfo.get());
    return functionInfo->type.retVal;
  } else
    fun.get_error().execute();
}

nullable<SType> UnaryExpression::eval(const Context& context) const {
  if (auto value = expr->eval(context))
    return ::eval(op, {value.get()});
  else
    return nullptr;
}

unique_ptr<Expression> UnaryExpression::replace(SType from, SType to) const {
  return unique<UnaryExpression>(codeLoc, op, expr->replace(from, to));
}

void StatementBlock::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& s : elems) {
    s->check(bodyContext);
  }
}

unique_ptr<Statement> StatementBlock::replace(SType from, SType to) const {
  auto ret = unique<StatementBlock>(codeLoc);
  for (auto& elem : elems)
    ret->elems.push_back(elem->replace(from, to));
  return ret;
}

void IfStatement::check(Context& context) {
  auto ifContext = Context::withParent(context);
  if (declaration)
    declaration->check(ifContext);
  if (!condition) {
    auto negate = [&] (unique_ptr<Expression> expr) {
      return unique<UnaryExpression>(declaration->codeLoc, Operator::LOGICAL_NOT, std::move(expr));
    };
    condition = negate(negate(unique<Variable>(declaration->codeLoc, declaration->identifier)));
  }
  auto condType = getType(ifContext, condition);
  codeLoc.check(ifContext.canConvert(condType, ArithmeticType::BOOL),
      "Expected a type convertible to bool or with overloaded operator " +
      quote("!") + " inside if statement, got " + quote(condType->getName()));
  auto movedBeforeTrueSegment = ifContext.getMovedVarsSnapshot();
  ifTrue->check(ifContext);
  if (ifFalse) {
    auto movedAfterTrueSegment = ifContext.getMovedVarsSnapshot();
    ifContext.setMovedVars(std::move(movedBeforeTrueSegment));
    ifFalse->check(ifContext);
    ifContext.mergeMovedVars(std::move(movedAfterTrueSegment));
  }
}

unique_ptr<Statement> IfStatement::replace(SType from, SType to) const {
  return unique<IfStatement>(codeLoc,
      declaration ? cast<VariableDeclaration>(declaration->replace(from, to)) : nullptr,
      condition ? condition->replace(from, to) : nullptr,
      ifTrue->replace(from, to),
      ifFalse ? ifFalse->replace(from, to) : nullptr);
}

void VariableDeclaration::check(Context& context) {
  context.checkNameConflict(codeLoc, identifier, "Variable");
  auto inferType = [&] () -> SType {
    if (realType)
      return realType.get();
    if (type)
      return context.getTypeFromString(*type).get();
    if (initExpr)
      return getType(context, initExpr)->getUnderlying();
    else
      codeLoc.error("Initializing expression needed to infer variable type");
  };
  realType = inferType();
  codeLoc.check(realType != ArithmeticType::VOID,
      "Can't declare variable of type " + quote(ArithmeticType::VOID->getName()));
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (!initExpr) {
    codeLoc.check(context.canDefaultInitialize(realType.get()), "Type " + quote(realType->getName()) + " requires initialization");
    initExpr = FunctionCall::constructor(codeLoc, realType.get());
  }
  auto exprType = getType(context, initExpr);
  if (!isMutable)
    if (auto value = initExpr->eval(context))
      context.addType(identifier, value.get());
  initExpr->codeLoc.check((!exprType.dynamicCast<ReferenceType>() && !exprType.dynamicCast<MutableReferenceType>()) ||
      exprType->getUnderlying()->isBuiltinCopyable(context),
      "Type " + quote(exprType->getUnderlying()->getName()) + " cannot be copied");
  initExpr->codeLoc.check(context.canConvert(exprType, realType.get()), "Can't initialize variable of type "
      + quote(realType.get()->getName()) + " with value of type " + quote(exprType->getName()));
  auto varType = isMutable ? SType(MutableReferenceType::get(realType.get())) : SType(ReferenceType::get(realType.get()));
  context.addVariable(identifier, std::move(varType));
}

unique_ptr<Statement> VariableDeclaration::replace(SType from, SType to) const {
  auto ret = unique<VariableDeclaration>(codeLoc, none, identifier, initExpr ? initExpr->replace(from, to) : nullptr);
  ret->isMutable = isMutable;
  ret->realType = realType->replace(from, to);
  return ret;
}

void ReturnStatement::check(Context& context) {
  if (!expr)
    codeLoc.check(context.getReturnType() && context.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = getType(context, expr);
    codeLoc.check(context.canConvert(returnType, context.getReturnType().get()),
        "Can't return value of type " + quote(returnType->getName()) +
        " from a function returning " + context.getReturnType()->getName());
  }
}

unique_ptr<Statement> ReturnStatement::replace(SType from, SType to) const {
  return unique<ReturnStatement>(codeLoc, expr->replace(from, to));
}

void Statement::addToContext(Context&) {}

void Statement::addToContext(Context& context, ImportCache& cache) {
  addToContext(context);
}

bool Statement::hasReturnStatement(const Context&) const {
  return false;
}

unique_ptr<Statement> Statement::replace(SType from, SType to) const {
  fail();
}

bool IfStatement::hasReturnStatement(const Context& context) const {
  return ifTrue->hasReturnStatement(context) && ifFalse && ifFalse->hasReturnStatement(context);
}

bool StatementBlock::hasReturnStatement(const Context& context) const {
  for (auto& s : elems)
    if (s->hasReturnStatement(context))
      return true;
  return false;
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc, unique_ptr<Expression> expr)
    : Statement(codeLoc), expr(std::move(expr)) {}

bool ReturnStatement::hasReturnStatement(const Context&) const {
  return true;
}

static vector<SConcept> applyConcept(Context& from, const vector<IdentifierInfo>& requirements) {
  vector<SConcept> ret;
  for (auto& requirement : requirements) {
    if (auto concept = from.getConcept(requirement.parts[0].name)) {
      auto& requirementArgs = requirement.parts[0].templateArguments;
      requirement.codeLoc.check(requirementArgs.size() == concept->getParams().size(),
          "Wrong number of template arguments to concept " + quote(requirement.parts[0].toString()));
      vector<SType> translatedParams;
      for (int i = 0; i < requirementArgs.size(); ++i) {
        if (auto arg = requirementArgs[i].getReferenceMaybe<IdentifierInfo>()) {
          auto origParam = from.getTypeFromString(IdentifierInfo(arg->parts[0], arg->codeLoc)).get();
          if (auto templateParam = origParam.dynamicCast<TemplateParameterType>())
            // Support is_enum concept
            if (concept->getParams()[i]->getType() != ArithmeticType::ANY_TYPE)
              templateParam->type = concept->getParams()[i]->getType();
          translatedParams.push_back(std::move(origParam));
        } else
          requirement.codeLoc.error("Expected a type argument");
      }
      auto translated = concept->translate(translatedParams);
      from.merge(translated->getContext());
      ret.push_back(translated);
    } else
      requirement.codeLoc.error("Uknown concept: " + requirement.parts[0].name);
  }
  return ret;
}

static SType convertPointerToReference(SType type) {
  if (auto p = type.dynamicCast<PointerType>())
    return ReferenceType::get(p->underlying);
  else if (auto p = type.dynamicCast<MutablePointerType>())
    return MutableReferenceType::get(p->underlying);
  else
    return type;
}

static SType convertReferenceToPointer(SType type) {
  if (auto p = type.dynamicCast<ReferenceType>())
    return PointerType::get(p->underlying);
  else if (auto p = type.dynamicCast<MutableReferenceType>())
    return MutablePointerType::get(p->underlying);
  else
    return type;
}

static bool paramsAreGoodForOperator(const vector<FunctionType::Param>& params) {
  for (auto& p : params)
    if (p.type->getUnderlying().dynamicCast<StructType>())
      return true;
  return false;
}

void FunctionDefinition::setFunctionType(const Context& context, bool concept, bool builtInImport) {
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (auto s = name.getReferenceMaybe<string>())
    context.checkNameConflictExcludingFunctions(codeLoc, *s, "Function");
  else if (auto op = name.getValueMaybe<Operator>()) {
    codeLoc.check(canOverload(*op, (int) parameters.size()), "Can't overload operator " + quote(getString(*op)) +
        " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = Context::withParent(context);
  vector<SType> templateTypes;
  for (auto& param : templateInfo.params) {
    if (param.type) {
      auto type = contextWithTemplateParams.getType(*param.type);
      param.codeLoc.check(!!type, "Type not found: " + quote(*param.type));
      templateTypes.push_back(CompileTimeValue::getTemplateValue(type.get(), param.name));
      contextWithTemplateParams.addType(param.name, templateTypes.back());
    } else {
      contextWithTemplateParams.checkNameConflict(param.codeLoc, param.name, "template parameter");
      templateTypes.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
      contextWithTemplateParams.addType(param.name, templateTypes.back());
    }
  }
  auto requirements = applyConcept(contextWithTemplateParams, templateInfo.requirements);
  if (auto returnType1 = contextWithTemplateParams.getTypeFromString(this->returnType)) {
    auto returnType = *returnType1;
    if (name.contains<Operator>())
      returnType = convertPointerToReference(returnType);
    vector<FunctionType::Param> params;
    set<string> paramNames;
    for (auto& p : parameters) {
      auto type = contextWithTemplateParams.getTypeFromString(p.type).get();
      if (name.contains<Operator>())
        type = convertPointerToReference(type);
      params.push_back({p.name, std::move(type)});
      if (p.name) {
        p.codeLoc.check(!paramNames.count(*p.name), "Duplicate function parameter name: " + quote(*p.name));
        paramNames.insert(*p.name);
      }
    }
    codeLoc.check(builtInImport || concept || !name.contains<Operator>() || paramsAreGoodForOperator(params),
        "Operator parameters must include at least one user-defined type");
    FunctionType functionType(returnType, params, templateTypes);
    functionType.fromConcept = concept;
    functionType.requirements = requirements;
    if (name.contains<ConstructorTag>() && external)
      functionType.generatedConstructor = true;
    if (name.contains<Operator>() && external)
      functionType.setBuiltin();
    functionInfo = FunctionInfo::getDefined(name, std::move(functionType), this);
  } else
    returnType1.get_error().execute();
}

static WithErrorLine<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierType id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  ErrorLoc errors = codeLoc.getError("Couldn't find function " + id.prettyString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  vector<SFunctionInfo> overloads;
  if (auto templateType = context.getFunctionTemplate(id)) {
    for (auto& overload : *templateType)
      if (auto f = context.instantiateFunctionTemplate(codeLoc, overload, templateArgs, argTypes, argLoc)) {
        overloads.push_back(f.get());
      } else
        errors = codeLoc.getError(errors.error + "\nCandidate: "s + overload->prettyString() + ": " +
            f.get_error().error);
  }
  if (overloads.empty())
    return errors;
  filterOverloads(overloads, argTypes);
  CHECK(!overloads.empty());
  if (overloads.size() == 1)
    return overloads[0];
  else
    return codeLoc.getError("Multiple function overloads found:\n" +
        combine(transform(overloads, [](const auto& o) { return o->prettyString();}), "\n"));
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualFunctionCallExpr(const Context& context,
    const string& funName, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  auto functionCall = unique<FunctionCall>(codeLoc, IdentifierInfo(funName, codeLoc));
  vector<SType> args;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      args.push_back(functionInfo->type.params[i].type);
    } else {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      args.push_back(alternativeType);
    }
  if (auto fun = getFunction(context, codeLoc, IdentifierType(funName), {}, args,
      vector<CodeLoc>(args.size(), codeLoc)))
    return unique_ptr<Expression>(std::move(functionCall));
  else
    return fun.get_error();
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualOperatorCallExpr(Context& context,
    Operator op, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  vector<unique_ptr<Expression>> arguments;
  vector<SType> argTypes;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      argTypes.push_back(functionInfo->type.params[i].type);
    } else {
      arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      argTypes.push_back(alternativeType);
      if (i == 0 && (alternativeType.dynamicCast<PointerType>() || alternativeType.dynamicCast<MutablePointerType>())) {
        arguments.back() = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(arguments.back()));
        argTypes.back() = argTypes.back()->removePointer();
      }
    }
  if (auto fun = handleOperatorOverloads(context, codeLoc, op, argTypes, vector<CodeLoc>(argTypes.size(), codeLoc))) {
    if (parameters.size() == 1)
      return unique_ptr<Expression>(unique<UnaryExpression>(codeLoc, op, std::move(arguments[0])));
    else {
      CHECK(parameters.size() == 2);
      return unique_ptr<Expression>(BinaryExpression::get(codeLoc, op, std::move(arguments)));
    }
  } else
    return fun.get_error();
}

void FunctionDefinition::generateVirtualDispatchBody(Context& bodyContext) {
  unique_ptr<StatementBlock> defaultBlock;
  if (body)
    defaultBlock = std::move(body);
  for (int i = 0; i < parameters.size(); ++i)
    parameters[i].isMutable = true;
  int virtualIndex = [&]() {
    for (int i = 0; i < parameters.size(); ++i)
      if (parameters[i].isVirtual)
        return i;
    fail();
  }();
  auto& virtualParam = parameters[virtualIndex];
  auto virtualType = bodyContext.getTypeFromString(virtualParam.type);
  unique_ptr<Expression> switchExpr = unique<MoveExpression>(codeLoc, *virtualParam.name);
  body = unique<StatementBlock>(codeLoc);
  auto variantType = virtualType->dynamicCast<StructType>();
  bool isPointerParam = false;
  if (!variantType) {
    variantType = virtualType.get()->removePointer().dynamicCast<StructType>();
    switchExpr = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(switchExpr));
    isPointerParam = true;
  }
  codeLoc.check(!!variantType && !variantType->alternatives.empty(),
      "Virtual parameter must be of a variant type or a pointer to one");
  auto switchStatementPtr = unique<SwitchStatement>(codeLoc, std::move(switchExpr));
  auto& switchStatement = *switchStatementPtr;
  body->elems.push_back(std::move(switchStatementPtr));
  for (auto& alternative : variantType->alternatives) {
    auto alternativeType = alternative.type;
    if (virtualType->dynamicCast<MutablePointerType>())
      alternativeType = MutablePointerType::get(std::move(alternativeType));
    else if (virtualType->dynamicCast<PointerType>())
      alternativeType = PointerType::get(std::move(alternativeType));
    WithErrorLine<unique_ptr<Expression>> call = [&] {
      if (auto regularName = name.getReferenceMaybe<string>())
        return getVirtualFunctionCallExpr(bodyContext, *regularName, alternative.name, alternativeType, virtualIndex);
      else if (auto op = name.getValueMaybe<Operator>())
        return getVirtualOperatorCallExpr(bodyContext, *op, alternative.name, alternativeType, virtualIndex);
      else
        fail();
    }();
    if (!call) {
      if (defaultBlock)
        switchStatement.defaultBlock = std::move(defaultBlock);
      else if (!switchStatement.defaultBlock)
        call.get_error().execute();
      continue;
    }
    auto block = unique<StatementBlock>(codeLoc);
    block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(*call)));
    switchStatement.caseElems.push_back(
        SwitchStatement::CaseElem {
          codeLoc,
          alternativeType,
          alternative.name,
          std::move(block)
        });
  }
}

void FunctionDefinition::checkAndGenerateCopyFunction(const Context& context) {
  if (!body && isDefault) {
    codeLoc.check(parameters.size() == 1, "Expected exactly one parameter in copy function");
    auto type = context.getTypeFromString(parameters[0].type).get();
    codeLoc.check(type == PointerType::get(context.getTypeFromString(returnType).get()),
        "Copy function parameter type must be the same as pointer to return type");
    auto structType = type->removePointer().dynamicCast<StructType>();
    codeLoc.check(!!structType, "Can only generate copy function for user-defined types");
    body = unique<StatementBlock>(codeLoc);
    if (structType->alternatives.empty()) {
      auto call = unique<FunctionCall>(codeLoc, returnType);
      for (auto elem : structType->members) {
        auto copiedParam = unique<Variable>(codeLoc, *parameters[0].name);
        auto copyCall = unique<FunctionCall>(codeLoc, IdentifierInfo("copy", codeLoc));
        copyCall->arguments.push_back(unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            BinaryExpression::get(codeLoc, Operator::POINTER_MEMBER_ACCESS,
            std::move(copiedParam), unique<Variable>(codeLoc, elem.name))));
        call->arguments.push_back(std::move(copyCall));
      }
      body->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(call)));
    } else {
      auto copiedParam = unique<Variable>(codeLoc, *parameters[0].name);
      auto topSwitch = unique<SwitchStatement>(codeLoc,
          unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(copiedParam)));
      for (auto& alternative : structType->alternatives) {
        auto block = unique<StatementBlock>(codeLoc);
        auto constructorName = returnType;
        constructorName.parts.push_back(IdentifierInfo::IdentifierPart { alternative.name, {} });
        auto constructorCall = unique<FunctionCall>(codeLoc, constructorName);
        if (alternative.type != ArithmeticType::VOID)
          constructorCall->arguments.push_back(unique<FunctionCall>(codeLoc, IdentifierInfo("copy", codeLoc),
              unique<Variable>(codeLoc, alternative.name)));
        block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(constructorCall)));
        topSwitch->caseElems.push_back(
            SwitchStatement::CaseElem {
              codeLoc,
              SType(alternative.type != ArithmeticType::VOID ? PointerType::get(alternative.type) : alternative.type),
              alternative.name,
              std::move(block)
            }
        );
      }
      body->elems.push_back(std::move(topSwitch));
    }
  }
}

void FunctionDefinition::InstanceInfo::generateBody(StatementBlock* parentBody) {
  CHECK(!body);
  auto templateParams = functionInfo->parent->type.templateParams;
  for (int i = 0; i < functionInfo->type.templateParams.size(); ++i) {
    StatementBlock* useBody = (i == 0) ? parentBody : body.get();
    body = cast<StatementBlock>(
        useBody->replace(templateParams[i], functionInfo->type.templateParams[i]));
    for (int j = i + 1; j < templateParams.size(); ++j)
      templateParams[j] = templateParams[j]->replace(templateParams[i], functionInfo->type.templateParams[i]);
  }
  CHECK(!!body);
}

void FunctionDefinition::addInstance(const Context& callContext, const SFunctionInfo& instance) {
  if (callContext.isTemplated())
    return;
  auto callTopContext = callContext.getTopLevelStates();
  if (instance != functionInfo.get()) {
    if (body) {
      CHECK(instance->parent == functionInfo);
      for (auto& other : instances)
        if (other.functionInfo == instance)
          return;
      instances.push_back(InstanceInfo{unique_ptr<StatementBlock>(), instance, callTopContext});
      if (!definitionContext.empty()) {
        instances.back().generateBody(body.get());
        checkBody(callTopContext, *instances.back().body, *instances.back().functionInfo);
      }
    }
  } else
    CHECK(instance->type.templateParams.empty());
}

static void addTemplateParam(Context& context, SType param) {
  if (auto valueType = param.dynamicCast<CompileTimeValue>()) {
    auto templateValue = valueType->value.getReferenceMaybe<CompileTimeValue::TemplateValue>();
    context.addType(templateValue->name,
        CompileTimeValue::get(CompileTimeValue::TemplateValue{templateValue->type, templateValue->name}));
  } else
    context.addType(param->getName(), param);
}

static Context::ConstStates mergeStates(Context::ConstStates v1, const Context::ConstStates& v2) {
  for (auto& elem : v2)
    if (find(v1.begin(), v1.end(), elem) == v1.end())
      v1.push_back(elem);
  return v1;
}

void FunctionDefinition::generateDefaultBodies(Context& context) {
  Context bodyContext = Context::withParent(context);
  for (auto& param : functionInfo->type.templateParams)
    addTemplateParam(bodyContext, param);
  applyConcept(bodyContext, templateInfo.requirements);
  if (isVirtual)
    generateVirtualDispatchBody(bodyContext);
  if (name == "copy"s)
    checkAndGenerateCopyFunction(bodyContext);
}

void FunctionDefinition::checkBody(Context::ConstStates callContext, StatementBlock& myBody,
    const FunctionInfo& instanceInfo) const {
  auto bodyContext = Context::withStates(mergeStates(definitionContext, callContext));
  bodyContext.setAsTopLevel();
  for (auto& t : instanceInfo.type.templateParams)
    if (!t->getMangledName()) {
      bodyContext.setTemplated();
      break;
    }
  for (int i = 0; i < instanceInfo.type.params.size(); ++i) {
    auto& p = instanceInfo.type.params[i];
    auto type = p.type;
    if (name.contains<Operator>())
      type = convertReferenceToPointer(type);
    if (p.name) {
      bodyContext.addVariable(*p.name, parameters[i].isMutable
          ? SType(MutableReferenceType::get(type))
          : SType(ReferenceType::get(type)));
    }
  }
  auto retVal = instanceInfo.type.retVal;
  if (name.contains<Operator>())
    retVal = convertReferenceToPointer(retVal);
  bodyContext.setReturnType(retVal);
  if (retVal != ArithmeticType::VOID && !myBody.hasReturnStatement(bodyContext) && !name.contains<ConstructorTag>())
    codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  myBody.check(bodyContext);
}

void FunctionDefinition::check(Context& context) {
  generateDefaultBodies(context);
  definitionContext = context.getAllStates();
  if (body) {
    Context paramsContext = Context::withParent(context);
    for (auto& param : functionInfo->type.templateParams)
      addTemplateParam(paramsContext, param);
    applyConcept(paramsContext, templateInfo.requirements);
    checkBody(paramsContext.getAllStates(), *body, *functionInfo);
  }
  for (int i = 0; i < instances.size(); ++i)
    if (!instances[i].body) {
      instances[i].generateBody(body.get());
      checkBody(instances[i].callContext, *instances[i].body, *instances[i].functionInfo);
    }
}

void FunctionDefinition::addToContext(Context& context, ImportCache& cache) {
  setFunctionType(context, false, cache.isCurrentlyBuiltIn());
  codeLoc.checkNoError(context.addFunction(functionInfo.get()));
  if (templateInfo.params.empty() && cache.currentlyInImport())
    // we are going to ignore the function body if we're in an import and it's not a template
    body = nullptr;
}

static void addIsEnumConcept(Context& context) {
  auto name = "is_enum";
  shared_ptr<Concept> concept = shared<Concept>(name);
  concept->modParams().push_back(shared<TemplateParameterType>(ArithmeticType::ENUM_TYPE, "T", CodeLoc()));
  context.addConcept(name, concept);
}

Context createNewContext() {
  static optional<Context> context;
  if (!context) {
    context.emplace();
    for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE, ArithmeticType::BOOL,
         ArithmeticType::VOID, ArithmeticType::CHAR, ArithmeticType::STRING})
      context->addType(type->getName(), type);
    CHECK(!context->addImplicitFunction(Operator::PLUS, FunctionType(ArithmeticType::STRING,
        {{ArithmeticType::STRING}, {ArithmeticType::STRING}}, {}).setBuiltin()));
    for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
      for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
        CHECK(!context->addImplicitFunction(op, FunctionType(type, {{type}}, {}).setBuiltin()));
    for (auto op : {Operator::INCREMENT, Operator::DECREMENT})
      CHECK(!context->addImplicitFunction(op, FunctionType(MutableReferenceType::get(ArithmeticType::INT),
          {{MutableReferenceType::get(ArithmeticType::INT)}}, {}).setBuiltin()));
    for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULO})
      for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
        if (type != ArithmeticType::DOUBLE || op != Operator::MODULO)
          CHECK(!context->addImplicitFunction(op, FunctionType(type, {{type}, {type}}, {}).setBuiltin()));
    for (auto op : {Operator::INCREMENT_BY, Operator::DECREMENT_BY, Operator::MULTIPLY_BY, Operator::DIVIDE_BY})
      for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
        CHECK(!context->addImplicitFunction(op, FunctionType(ArithmeticType::VOID,
            {{MutableReferenceType::get(type)}, {type}}, {}).setBuiltin()));
    for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
      CHECK(!context->addImplicitFunction(op, FunctionType(ArithmeticType::BOOL,
          {{ArithmeticType::BOOL}, {ArithmeticType::BOOL}}, {}).setBuiltin()));
    CHECK(!context->addImplicitFunction(Operator::LOGICAL_NOT, FunctionType(ArithmeticType::BOOL,
        {{ArithmeticType::BOOL}}, {}).setBuiltin()));
    for (auto op : {Operator::EQUALS, Operator::LESS_THAN, Operator::MORE_THAN})
      for (auto type : {ArithmeticType::INT, ArithmeticType::STRING, ArithmeticType::DOUBLE})
        CHECK(!context->addImplicitFunction(op, FunctionType(ArithmeticType::BOOL, {{type}, {type}}, {}).setBuiltin()));
    for (auto op : {Operator::EQUALS})
      for (auto type : {ArithmeticType::BOOL, ArithmeticType::CHAR})
        CHECK(!context->addImplicitFunction(op, FunctionType(ArithmeticType::BOOL, {{type}, {type}}, {}).setBuiltin()));
    addIsEnumConcept(*context);
    context->addBuiltInFunction("enum_size", ArithmeticType::INT, {SType(ArithmeticType::ENUM_TYPE)},
        [](vector<SType> args) -> WithError<SType> {
          if (auto enumType = args[0].dynamicCast<EnumType>())
            return (SType) CompileTimeValue::get((int) enumType->elements.size());
          else
            fail();
        });
    context->addBuiltInFunction("enum_strings", ArrayType::get(ArithmeticType::STRING, CompileTimeValue::get(0)),
            {SType(ArithmeticType::ENUM_TYPE)},
        [](vector<SType> args) -> WithError<SType> {
          auto enumType = args[0].dynamicCast<EnumType>();
          vector<SCompileTimeValue> values;
          for (auto& elem : enumType->elements)
            values.push_back(CompileTimeValue::get(elem));
          return (SType) CompileTimeValue::get(CompileTimeValue::ArrayValue{values, ArithmeticType::STRING});
        });
  }
  return Context::withParent(*context);
}

static void addBuiltInImport(AST& ast) {
  auto tmpVec = std::move(ast.elems);
  ast.elems = makeVec<unique_ptr<Statement>>(
      unique<ImportStatement>(CodeLoc{}, "std/builtin.znn", true, true)
  );
  for (auto& elem : tmpVec)
    ast.elems.push_back(std::move(elem));
}

vector<ModuleInfo> correctness(AST& ast, Context& context, const vector<string>& importPaths, bool isBuiltInModule) {
  ImportCache cache(isBuiltInModule);
  if (!isBuiltInModule)
    addBuiltInImport(ast);
  for (auto& elem : ast.elems) {
    if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
      import->setImportDirs(importPaths);
    elem->addToContext(context, cache);
  }
  for (auto& elem : ast.elems) {
    elem->check(context);
  }
  return cache.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(Context& context) {
  getType(context, expr);
}

unique_ptr<Statement> ExpressionStatement::replace(SType from, SType to) const {
  return unique<ExpressionStatement>(expr->replace(from, to));
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

SType FunctionCall::getTypeImpl(Context& context) {
  return getDotOperatorType(nullptr, context).get();
}

nullable<SType> FunctionCall::getDotOperatorType(Expression* left, Context& callContext) {
  optional<ErrorLoc> error;
  if (!templateArgs)
    templateArgs = callContext.getTypeList(identifier->parts.back().templateArguments).get();
  if (!identifierType)
    identifierType = callContext.getIdentifierType(*identifier).get(identifier->codeLoc);
  if (!functionInfo) {
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      argTypes.push_back(getType(callContext, arguments[i]));
      argLocs.push_back(arguments[i]->codeLoc);
      INFO << "Function argument " << argTypes.back()->getName();
    }
    if (!left)
      getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs).unpack(functionInfo, error);
    else {
      auto leftType = left->getTypeImpl(callContext);
      callType = MethodCallType::METHOD;
      auto tryMethodCall = [&](MethodCallType thisCallType) {
        auto res = getFunction(callContext, codeLoc, *identifierType, *templateArgs, concat({leftType}, argTypes),
            concat({left->codeLoc}, argLocs));
        if (res)
            callType = thisCallType;
        if (res && functionInfo)
          codeLoc.error("Ambigous method call:\nCandidate: " + functionInfo->prettyString() +
              "\nCandidate: " + res.get()->prettyString());
        res.unpack(functionInfo, error);
      };
      if (!leftType->getUnderlying().dynamicCast<PointerType>() &&
          !leftType->getUnderlying().dynamicCast<MutablePointerType>())
        tryMethodCall(MethodCallType::FUNCTION_AS_METHOD);
      leftType = leftType.dynamicCast<MutableReferenceType>()
          ? SType(MutablePointerType::get(leftType->getUnderlying()))
          : SType(PointerType::get(leftType->getUnderlying()));
      tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER);
    }
  }
  if (functionInfo) {
    for (int i = 0; i < argNames.size(); ++i) {
      auto paramName = functionInfo->type.params[callType ? (i + 1) : i].name;
      if (argNames[i] && paramName && argNames[i] != paramName) {
        arguments[i]->codeLoc.error("Function argument " + quote(*argNames[i]) +
            " doesn't match parameter " + quote(*paramName) + " of function " +
            functionInfo->prettyString());
      }
    }
    if (auto parent = functionInfo->parent)
      if (parent->definition)
        parent->definition->addInstance(callContext, functionInfo.get());
    return functionInfo->type.retVal;
  } else
    error->execute();
}

nullable<SType> FunctionCall::eval(const Context& context) const {
  if (identifier)
    if (auto name = identifier->asBasicIdentifier()) {
      vector<SType> args;
      vector<CodeLoc> locs;
      for (auto& e : arguments) {
        locs.push_back(e->codeLoc);
        if (auto res = e->eval(context))
          args.push_back(res.get());
        else
          return res;
      }
      return context.invokeFunction(*name, codeLoc, std::move(args), std::move(locs));
    }
  return nullptr;
}

unique_ptr<Expression> FunctionCall::replace(SType from, SType to) const {
  auto ret = unique<FunctionCall>(codeLoc, Private{});
  ret->identifier = identifier;
  for (auto& arg : arguments)
    ret->arguments.push_back(arg->replace(from, to));
  ret->templateArgs.emplace();
  for (auto& arg : *templateArgs)
    ret->templateArgs->push_back(arg->replace(from, to));
  ret->argNames = argNames;
  ret->identifierType = identifierType->replace(from, to);
  return ret;
}

FunctionCall::FunctionCall(CodeLoc l, Private) : Expression(l) {}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

void SwitchStatement::check(Context& context) {
  getType(context, expr)->handleSwitchStatement(*this, context, expr->codeLoc, Type::SwitchArgument::VALUE);
}

unique_ptr<Statement> SwitchStatement::replace(SType from, SType to) const {
  auto ret = unique<SwitchStatement>(codeLoc, expr->replace(from,to));
  ret->targetType = targetType->replace(from, to);
  if (defaultBlock)
    ret->defaultBlock = cast<StatementBlock>(defaultBlock->replace(from, to));
  for (auto& elem : caseElems)
    ret->caseElems.push_back(elem.replace(from, to));
  return ret;
}

bool SwitchStatement::hasReturnStatement(const Context& context) const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement(context))
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement(context))
    return false;
  return true;
}

VariantDefinition::VariantDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

static shared_ptr<StructType> getNewOrIncompleteStruct(Context& context, string name, CodeLoc codeLoc,
    const TemplateInfo& templateInfo) {
  if (auto existing = context.getType(name)) {
    if (auto asStruct = existing.get().dynamicCast<StructType>()) {
      auto returnType = asStruct;
      if (!returnType->incomplete) {
        // if it's not an incomplete type then this triggers a conflict error
        context.checkNameConflict(codeLoc, name, "Type");
        fail();
      }
      returnType->incomplete = false;
      return returnType;
    } else {
      context.checkNameConflict(codeLoc, name, "Type");
      fail();
    }
  } else {
    context.checkNameConflict(codeLoc, name, "Type");
    auto paramsContext = Context::withParent(context);
    auto returnType = StructType::get(name);
    for (auto& param : templateInfo.params) {
      if (param.type) {
        if (auto type = paramsContext.getType(*param.type)) {
          auto valueType = CompileTimeValue::getTemplateValue(type.get(), param.name);
          returnType->templateParams.push_back(valueType);
        } else
          param.codeLoc.error("Type not found: " + quote(*param.type));
      } else {
        returnType->templateParams.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
        paramsContext.addType(param.name, returnType->templateParams.back());
      }
    }
    context.addType(name, returnType);
    return returnType;
  }
}

void VariantDefinition::addToContext(Context& context) {
  type = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo);
  auto membersContext = Context::withParent(context);
  codeLoc.check(templateInfo.params.size() == type->templateParams.size(),
      "Number of template parameters differs from forward declaration");
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  type->requirements = applyConcept(membersContext, templateInfo.requirements);
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    subtype.codeLoc.check(!subtypeNames.count(subtype.name), "Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    vector<FunctionType::Param> params;
    auto subtypeInfo = membersContext.getTypeFromString(subtype.type).get();
    if (subtypeInfo != ArithmeticType::VOID)
      params.push_back(FunctionType::Param{subtypeInfo});
    auto constructor = FunctionType(type.get(), params, {});
    constructor.parentType = type.get();
    CHECK(!type->staticContext.addImplicitFunction(subtype.name, constructor));
  }
}

void VariantDefinition::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  applyConcept(bodyContext, templateInfo.requirements);
  for (auto& subtype : elements)
    type->alternatives.push_back({subtype.name, bodyContext.getTypeFromString(subtype.type).get()});
  type->updateInstantations();
}

void StructDefinition::addToContext(Context& context, ImportCache& cache) {
  type = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo);
  type->incomplete = incomplete;
  if (!incomplete) {
    codeLoc.check(templateInfo.params.size() == type->templateParams.size(),
        "Number of template parameters differs from forward declaration");
    auto membersContext = Context::withParent(context);
    for (auto& param : type->templateParams)
      addTemplateParam(membersContext, param);
    type->requirements = applyConcept(membersContext, templateInfo.requirements);
    for (auto& member : members) {
      //INFO << "Struct member " << member.name << " " << member.type.toString() << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
      type->members.push_back({member.name, membersContext.getTypeFromString(member.type).get()});
    }
    for (auto& member : members)
      if (auto error = type->members.back().type->getSizeError())
        member.codeLoc.error("Member " + quote(member.name) + " of type " + quote(type->getName()) + " " + *error);
    if (!external) {
      vector<FunctionType::Param> constructorParams;
      for (auto& member : type->members)
        constructorParams.push_back({member.name, member.type});
      auto fun = FunctionType(type.get(), std::move(constructorParams), type->templateParams);
      fun.generatedConstructor = true;
      CHECK(!context.addImplicitFunction(ConstructorTag{}, fun));
      fun.templateParams.clear();
      fun.parentType = type.get();
      CHECK(!type->getStaticContext().addImplicitFunction(ConstructorTag{}, fun));
      type->getStaticContext().addType(name, type.get());
    } else
      type->external = true;
  }
}

void StructDefinition::check(Context& context) {
  auto methodBodyContext = Context::withParent(context);
  auto staticFunContext = Context::withParent({&context, &type->staticContext});
  for (auto& param : type->templateParams)
    addTemplateParam(methodBodyContext, param);
  applyConcept(methodBodyContext, templateInfo.requirements);
  type->updateInstantations();
  if (auto error = type->getSizeError())
    codeLoc.error("Type " + quote(type->getName()) + " " + *error);
}

MoveExpression::MoveExpression(CodeLoc l, string id) : Expression(l), identifier(id) {
}

SType MoveExpression::getTypeImpl(Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
      codeLoc.check(!!ret.get_value().dynamicCast<MutableReferenceType>() ||
                    !!ret.get_value().dynamicCast<ReferenceType>(),
          "Can't move from " + quote(ret.get_value()->getName()));
      codeLoc.checkNoError(context.setVariableAsMoved(identifier));
      type = ret.get_value()->getUnderlying();
    } else
      codeLoc.error(ret.get_error());
  }
  return type.get();
}

unique_ptr<Expression> MoveExpression::replace(SType from, SType to) const {
  auto ret = unique<MoveExpression>(codeLoc, identifier);
  ret->type = type->replace(from, to);
  return ret;
}


EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

void EmbedStatement::check(Context&) {
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  if (isPublic)
    return TopLevelAllowance::MUST;
  else
    return TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::replace(SType from, SType to) const {
  auto ret = unique<EmbedStatement>(codeLoc, value);
  ret->replacements = replacements;
  ret->replacements.push_back({from, to});
  return ret;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

void ForLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  init->check(bodyContext);
  cond->codeLoc.check(getType(bodyContext, cond) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  getType(bodyContext, iter);
  bodyContext.setBreakAllowed();
  body->check(bodyContext);
}

unique_ptr<Statement> ForLoopStatement::replace(SType from, SType to) const {
  return unique<ForLoopStatement>(codeLoc,
      init->replace(from, to),
      cond->replace(from, to),
      iter->replace(from, to),
      body->replace(from, to));
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b)
  : Statement(l), cond(std::move(c)), body(std::move(b)) {}

void WhileLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  cond->codeLoc.check(getType(bodyContext, cond) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  bodyContext.setBreakAllowed();
  body->check(bodyContext);
}

unique_ptr<Statement> WhileLoopStatement::replace(SType from, SType to) const {
  return unique<WhileLoopStatement>(codeLoc,
      cond->replace(from, to),
      body->replace(from, to));
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool pub, bool isBuiltIn)
    : Statement(l), path(p), isPublic(pub), isBuiltIn(isBuiltIn) {
}

void ImportStatement::setImportDirs(const vector<string>& p) {
  importDirs = p;
}

void ImportStatement::check(Context&) {
}

void ImportStatement::processImport(Context& context, ImportCache& cache, const string& content, const string& path) {
  codeLoc.check(!cache.isCurrentlyImported(path),
      "Public import cycle: " + combine(cache.getCurrentImports(), ", "));
  if ((!isPublic && cache.currentlyInImport())) {
    INFO << "Skipping non public import " << path;
    return;
  }
  if (!cache.contains(path)) {
    INFO << "Parsing import " << path;
    cache.pushCurrentImport(path, isBuiltIn);
    auto tokens = lex(content, CodeLoc(path, 0, 0), "end of file");
    ast = unique<AST>(parse(tokens));
    if (!isBuiltIn && !cache.isCurrentlyBuiltIn())
      addBuiltInImport(*ast);
    Context importContext = createNewContext();
    for (auto& elem : ast->elems) {
      if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
        import->setImportDirs(importDirs);
      elem->addToContext(importContext, cache);
    }
    for (auto& elem : ast->elems)
      elem->check(importContext);
    cache.popCurrentImport(isBuiltIn);
    cache.insert(path, std::move(importContext), isBuiltIn || cache.isCurrentlyBuiltIn());
  } else
    INFO << "Import " << path << " already cached";
  context.merge(cache.getContext(path));
}

void ImportStatement::addToContext(Context& context, ImportCache& cache) {
  INFO << "Resolving import " << path << " from " << codeLoc.file;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    INFO << "Trying directory " << importDir;
    auto importPath = fs::path(importDir)  / path;
    if (auto content = readFromFile(importPath.c_str())) {
      importPath = fs::canonical(importPath);
      INFO << "Imported file " << importPath;
      processImport(context, cache, content->value, importPath);
      return;
    }
  }
  codeLoc.error("Couldn't resolve import path: " + path);
}

nullable<SType> Expression::eval(const Context&) const {
  return nullptr;
}

nullable<SType> Expression::getDotOperatorType(Expression* left, Context& callContext) {
  return nullptr;
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

void EnumDefinition::addToContext(Context& s) {
  codeLoc.check(!elements.empty(), "Enum requires at least one element");
  unordered_set<string> occurences;
  for (auto& e : elements)
    codeLoc.check(!occurences.count(e), "Duplicate enum element: " + quote(e));
  s.addType(name, shared<EnumType>(name, elements));
}

void EnumDefinition::check(Context& s) {
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

SType EnumConstant::getTypeImpl(Context& context) {
  auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc)).get();
  if (auto enumType = type.dynamicCast<EnumType>()) {
    codeLoc.check(contains(enumType->elements, enumElement),
        quote(enumElement) + " is not an element of enum " + quote(enumName));
  } else
    codeLoc.error(quote(type->getName()) + " is not an enum type");
  return type;
}

nullable<SType> EnumConstant::eval(const Context& context) const {
  if (auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc))) {
    if (auto enumType = type->dynamicCast<EnumType>()) {
      for (int i = 0; i < enumType->elements.size(); ++i)
        if (enumType->elements[i] == enumElement)
          return (SType) CompileTimeValue::get(CompileTimeValue::EnumValue{enumType, i});
    }
  }
  FATAL << "Unrecognized enum element - should have been discovered by the type checker";
  fail();
}

unique_ptr<Expression> EnumConstant::replace(SType from, SType to) const {
  auto ret = unique<EnumConstant>(codeLoc, enumName, enumElement);
  ret->enumType = enumType->replace(from, to);
  return ret;
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {

}

void ConceptDefinition::addToContext(Context& context) {
  shared_ptr<Concept> concept = shared<Concept>(name);
  auto declarationsContext = Context::withParent(context);
  for (auto& param : templateInfo.params) {
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back());
  }
  for (auto& function : functions) {
    function->codeLoc.check(!function->isVirtual, "Virtual functions are not allowed here");
    function->setFunctionType(declarationsContext, true);
    function->check(declarationsContext);
    function->codeLoc.checkNoError(concept->modContext().addFunction(function->functionInfo.get()));
  }
  context.addConcept(name, concept);
}

void ConceptDefinition::check(Context& context) {
}

CodegenStage CodegenStage::types() {
  CodegenStage ret;
  ret.isTypes = true;
  ret.isDefine = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::define() {
  CodegenStage ret;
  ret.isDefine = true;
  ret.isTypes = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::declare() {
  CodegenStage ret;
  ret.isTypes = false;
  ret.isDefine = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::setImport() {
  isImport = true;
  return *this;
}

RangedLoopStatement::RangedLoopStatement(CodeLoc l, unique_ptr<VariableDeclaration> init,
    unique_ptr<Expression> container, unique_ptr<Statement> body)
    : Statement(l), init(std::move(init)), container(std::move(container)), body(std::move(body)) {}

void RangedLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  auto containerType = getType(context, container);
  if (containerType->getUnderlying() == containerType.get())
    containerType = ReferenceType::get(containerType);
  auto uniqueSufix = to_string(codeLoc.line) + "_" + to_string(codeLoc.column);
  containerName = "container"s + uniqueSufix;
  auto containerEndName = "container_end"s + uniqueSufix;
  bodyContext.addVariable(*containerName, containerType);
  containerEnd = unique<VariableDeclaration>(codeLoc, none, containerEndName,
      BinaryExpression::get(codeLoc, Operator::MEMBER_ACCESS,
          unique<Variable>(codeLoc, *containerName), unique<FunctionCall>(codeLoc, IdentifierInfo("end", codeLoc))));
  containerEnd->check(bodyContext);
  init->initExpr = BinaryExpression::get(codeLoc, Operator::MEMBER_ACCESS,
      unique<Variable>(codeLoc, *containerName), unique<FunctionCall>(codeLoc, IdentifierInfo("begin", codeLoc)));
  init->isMutable = true;
  condition = BinaryExpression::get(codeLoc, Operator::NOT_EQUAL, unique<Variable>(codeLoc, init->identifier),
      unique<Variable>(codeLoc, containerEndName));
  increment = unique<UnaryExpression>(codeLoc, Operator::INCREMENT, unique<Variable>(codeLoc, init->identifier));
  init->check(bodyContext);
  codeLoc.check(getType(bodyContext, condition) == ArithmeticType::BOOL, "Equality comparison between iterators"
      " does not return type " + quote("bool"));
  getType(bodyContext, increment);
  bodyContext.setBreakAllowed();
  body->check(bodyContext);
}

unique_ptr<Statement> RangedLoopStatement::replace(SType from, SType to) const {
  auto ret = unique<RangedLoopStatement>(codeLoc,
      cast<VariableDeclaration>(init->replace(from, to)),
      container->replace(from, to),
      body->replace(from, to));
  ret->condition = condition->replace(from, to);
  ret->increment = increment->replace(from, to);
  ret->containerName = containerName;
  ret->containerEnd = cast<VariableDeclaration>(containerEnd->replace(from, to));
  return ret;
}

void BreakStatement::check(Context& context) {
  codeLoc.check(context.breakAllowed(), "Break statement outside of a loop");
}

unique_ptr<Statement> BreakStatement::replace(SType from, SType to) const {
  return unique<BreakStatement>(codeLoc);
}

void ContinueStatement::check(Context& context) {
  codeLoc.check(context.breakAllowed(), "Continue statement outside of a loop");
}

unique_ptr<Statement> ContinueStatement::replace(SType from, SType to) const {
  return unique<ContinueStatement>(codeLoc);
}

ArrayLiteral::ArrayLiteral(CodeLoc codeLoc) : Expression(codeLoc) {
}

SType ArrayLiteral::getTypeImpl(Context& context) {
  auto ret = getType(context, contents[0])->getUnderlying();
  for (int i = 1; i < contents.size(); ++i) {
    auto t = getType(context, contents[i])->getUnderlying();
    contents[i]->codeLoc.check(t == ret, "Incompatible types in array literal: " +
        quote(ret->getName()) + " and " + quote(t->getName()));
  }
  return ArrayType::get(ret, CompileTimeValue::get((int)contents.size()));
}

unique_ptr<Expression> ArrayLiteral::replace(SType from, SType to) const {
  auto ret = unique<ArrayLiteral>(codeLoc);
  for (auto& elem : contents)
    ret->contents.push_back(elem->replace(from, to));
  return ret;
}


SType getType(Context& context, unique_ptr<Expression>& expr, bool evaluateAtCompileTime) {
  auto type = expr->getTypeImpl(context);
  if (evaluateAtCompileTime) {
    if (auto type = expr->eval(context)) {
      if (auto value = type.get().dynamicCast<CompileTimeValue>())
        expr = unique<Constant>(expr->codeLoc, value);
    }
  }
  return type;
}

nullable<SType> SwitchStatement::CaseElem::getType(const Context& context) {
  return type.visit(
      [&](const IdentifierInfo& id) -> nullable<SType> { auto ret = context.getTypeFromString(id).get(); type = ret; return ret; },
      [](const SType& t) -> nullable<SType> { return t;},
      [](none_t) -> nullable<SType> { return nullptr; }
  );
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::replace(SType from, SType to) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.id = id;
  ret.block = cast<StatementBlock>(block->replace(from, to));
  if (auto t = type.getReferenceMaybe<SType>())
    ret.type = (*t)->replace(from, to);
  else
    ret.type = type;
  return ret;
}
