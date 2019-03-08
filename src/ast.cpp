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

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, FunctionName name)
  : Statement(l), returnType(std::move(r)), name(name) {}

WithErrorLine<SType> Constant::getTypeImpl(Context&) {
  return value->getType();
}

nullable<SType> Constant::eval(const Context&) const {
  return (SType) value;
}

unique_ptr<Expression> Constant::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<Constant>(codeLoc, value->replace(from, to, errors).dynamicCast<CompileTimeValue>());
}

WithErrorLine<SType> Variable::getTypeImpl(Context& context) {
  optional<string> varError;
  if (auto id = identifier.asBasicIdentifier()) {
    if (auto varType = context.getTypeOfVariable(*id))
      return *varType;
    else
      varError = varType.get_error();
  }
  if (auto t = context.getTypeFromString(identifier))
    return t.get()->getType();
  return codeLoc.getError(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

nullable<SType> Variable::eval(const Context& context) const {
  if (auto res = context.getTypeFromString(identifier))
    return *res;
  else
    return nullptr;
}

unique_ptr<Expression> Variable::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<Variable>(codeLoc, identifier);
}

WithErrorLine<SType> Variable::getDotOperatorType(Expression* left, Context& callContext) {
  if (left)
    if (auto id = identifier.asBasicIdentifier()) {
      if (auto leftType = left->getTypeImpl(callContext)) {
        if (auto structType = leftType.get()->getUnderlying().dynamicCast<StructType>()) {
          if (auto member = structType->getTypeOfMember(*id))
            return SType(MutableReferenceType::get(*member));
          else
            return codeLoc.getError(member.get_error());
        }
      } else
        return leftType;
    }
  return codeLoc.getError("Bad use of comma operator");
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

WithErrorLine<SType> BinaryExpression::getTypeImpl(Context& context) {
  auto leftTmp = getType(context, expr[0]);
  if (!leftTmp)
    return leftTmp;
  auto left = *leftTmp;
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
          rightType = rightType.get()->getUnderlying();
        else if (left.dynamicCast<ReferenceType>() && rightType.get().dynamicCast<MutableReferenceType>())
          *rightType = ReferenceType::get(rightType.get()->getUnderlying());
        return rightType.get();
      } else
        return rightType;
    }
    default: {
      auto right = getType(context, expr[1]);
      vector<SType> exprTypes;
      for (auto& elem : expr)
        if (auto t = elem->getTypeImpl(context))
          exprTypes.push_back(*t);
        else
          return t;
      if (auto fun = handleOperatorOverloads(context, codeLoc, op, exprTypes,
          transform(expr, [&](auto& e) { return e->codeLoc;}))) {
        functionInfo = *fun;
        if (auto parent = functionInfo->parent)
          if (parent->definition)
            parent->definition->addInstance(context, functionInfo.get());
        return functionInfo->type.retVal;
      } else
        return fun.get_error();
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

unique_ptr<Expression> BinaryExpression::replace(SType from, SType to, ErrorBuffer& errors) const {
  return get(codeLoc, op, expr[0]->replace(from, to, errors), expr[1]->replace(from, to, errors));
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

WithErrorLine<SType> UnaryExpression::getTypeImpl(Context& context) {
  nullable<SType> ret;
  auto right = getType(context, expr, op != Operator::GET_ADDRESS);
  if (!right)
    return right;
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right.get()->getName())};
  if (auto t = expr->getTypeImpl(context)) {
    if (auto fun = handleOperatorOverloads(context, codeLoc, op, {*t}, {expr->codeLoc})) {
      functionInfo = *fun;
      if (auto parent = functionInfo->parent)
        if (parent->definition)
          parent->definition->addInstance(context, functionInfo.get());
      return functionInfo->type.retVal;
    } else
      return fun.get_error();
  } else
    return t;
}

nullable<SType> UnaryExpression::eval(const Context& context) const {
  if (auto value = expr->eval(context))
    return ::eval(op, {value.get()});
  else
    return nullptr;
}

unique_ptr<Expression> UnaryExpression::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<UnaryExpression>(codeLoc, op, expr->replace(from, to, errors));
}

optional<ErrorLoc> StatementBlock::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& s : elems)
    if (auto err = s->check(bodyContext))
      return err;
  return none;
}

unique_ptr<Statement> StatementBlock::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<StatementBlock>(codeLoc);
  for (auto& elem : elems)
    ret->elems.push_back(elem->replace(from, to, errors));
  return ret;
}

optional<ErrorLoc> IfStatement::check(Context& context) {
  auto ifContext = Context::withParent(context);
  if (declaration)
    if (auto err = declaration->check(ifContext))
      return err;
  if (!condition) {
    auto negate = [&] (unique_ptr<Expression> expr) {
      return unique<UnaryExpression>(declaration->codeLoc, Operator::LOGICAL_NOT, std::move(expr));
    };
    condition = negate(negate(unique<Variable>(declaration->codeLoc, declaration->identifier)));
  }
  auto condType = getType(ifContext, condition);
  if (!condType)
    return condType.get_error();
  if (!ifContext.canConvert(*condType, ArithmeticType::BOOL))
    return codeLoc.getError(
        "Expected a type convertible to bool or with overloaded operator " +
        quote("!") + " inside if statement, got " + quote(condType.get()->getName()));
  auto movedBeforeTrueSegment = ifContext.getMovedVarsSnapshot();
  if (auto err = ifTrue->check(ifContext))
    return err;
  if (ifFalse) {
    auto movedAfterTrueSegment = ifContext.getMovedVarsSnapshot();
    ifContext.setMovedVars(std::move(movedBeforeTrueSegment));
    if (auto err = ifFalse->check(ifContext))
      return err;
    ifContext.mergeMovedVars(std::move(movedAfterTrueSegment));
  }
  return none;
}

unique_ptr<Statement> IfStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<IfStatement>(codeLoc,
      declaration ? cast<VariableDeclaration>(declaration->replace(from, to, errors)) : nullptr,
      condition ? condition->replace(from, to, errors) : nullptr,
      ifTrue->replace(from, to, errors),
      ifFalse ? ifFalse->replace(from, to, errors) : nullptr);
}

optional<ErrorLoc> VariableDeclaration::check(Context& context) {
  if (auto err = context.checkNameConflict(identifier, "Variable"))
    return codeLoc.getError(*err);
  if (!realType) {
    if (type) {
      if (auto t = context.getTypeFromString(*type))
        realType = *t;
      else
        return t.get_error();
    } else
    if (initExpr) {
      if (auto t = getType(context, initExpr))
        realType = t.get()->getUnderlying();
      else
        return t.get_error();
    } else
      return codeLoc.getError("Initializing expression needed to infer variable type");
  }
  if (realType == ArithmeticType::VOID)
    return codeLoc.getError("Can't declare variable of type " + quote(ArithmeticType::VOID->getName()));
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (!initExpr) {
    if (!context.canDefaultInitialize(realType.get()))
      return codeLoc.getError("Type " + quote(realType->getName()) + " requires initialization");
    initExpr = FunctionCall::constructor(codeLoc, realType.get());
  }
  auto exprType = getType(context, initExpr);
  if (!exprType)
    return exprType.get_error();
  if (!isMutable)
    if (auto value = initExpr->eval(context))
      context.addType(identifier, value.get());
  if ((exprType->dynamicCast<ReferenceType>() || exprType->dynamicCast<MutableReferenceType>()) &&
      !exprType.get()->getUnderlying()->isBuiltinCopyable(context))
    return initExpr->codeLoc.getError("Type " + quote(exprType.get()->getUnderlying()->getName()) +
        " cannot be copied implicitly");
  if (!context.canConvert(*exprType, realType.get()))
    return initExpr->codeLoc.getError("Can't initialize variable of type "
       + quote(realType.get()->getName()) + " with value of type " + quote(exprType.get()->getName()));
  auto varType = isMutable ? SType(MutableReferenceType::get(realType.get())) : SType(ReferenceType::get(realType.get()));
  context.addVariable(identifier, std::move(varType));
  return none;
}

unique_ptr<Statement> VariableDeclaration::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<VariableDeclaration>(codeLoc, none, identifier,
      initExpr ? initExpr->replace(from, to, errors) : nullptr);
  ret->isMutable = isMutable;
  ret->realType = realType->replace(from, to, errors);
  return ret;
}

optional<ErrorLoc> ReturnStatement::check(Context& context) {
  if (!expr && context.getReturnType() != ArithmeticType::VOID)
    return codeLoc.getError("Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = getType(context, expr);
    if (!returnType)
      return returnType.get_error();
    if (!context.canConvert(*returnType, context.getReturnType().get()))
      return codeLoc.getError(
          "Can't return value of type " + quote(returnType.get()->getName()) +
          " from a function returning " + context.getReturnType()->getName());
  }
  return none;
}

unique_ptr<Statement> ReturnStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<ReturnStatement>(codeLoc, expr->replace(from, to, errors));
}

optional<ErrorLoc> Statement::addToContext(Context&) {
  return none;
}

optional<ErrorLoc> Statement::addToContext(Context& context, ImportCache& cache) {
  return addToContext(context);
}

bool Statement::hasReturnStatement(const Context&) const {
  return false;
}

unique_ptr<Statement> Statement::replace(SType from, SType to, ErrorBuffer& errors) const {
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

NODISCARD static WithErrorLine<vector<SConcept>> applyConcept(Context& from, const vector<IdentifierInfo>& requirements) {
  vector<SConcept> ret;
  for (auto& requirement : requirements) {
    if (auto concept = from.getConcept(requirement.parts[0].name)) {
      auto& requirementArgs = requirement.parts[0].templateArguments;
      if (requirementArgs.size() != concept->getParams().size())
        return requirement.codeLoc.getError(
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
          return requirement.codeLoc.getError("Expected a type argument");
      }
      ErrorBuffer errors;
      auto translated = concept->translate(translatedParams, errors);
      if (!errors.empty())
        return errors[0];
      from.merge(translated->getContext());
      ret.push_back(translated);
    } else
      return requirement.codeLoc.getError("Uknown concept: " + requirement.parts[0].name);
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

static FunctionId getFunctionId(const FunctionName& name) {
  return name.visit(
      [&](const string& s) -> FunctionId { return s; },
      [&](Operator op) -> FunctionId { return op; },
      [&](ConstructorId) -> FunctionId { return ConstructorTag{}; }
  );
}

optional<ErrorLoc> FunctionDefinition::setFunctionType(const Context& context, bool concept, bool builtInImport) {
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (auto s = name.getReferenceMaybe<string>()) {
    if (auto err = context.checkNameConflictExcludingFunctions(*s, "Function"))
      return codeLoc.getError(*err);
  } else
  if (auto op = name.getValueMaybe<Operator>()) {
    if (!canOverload(*op, int(parameters.size())))
      return codeLoc.getError("Can't overload operator " + quote(getString(*op)) +
          " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = Context::withParent(context);
  vector<SType> templateTypes;
  for (auto& param : templateInfo.params) {
    if (param.type) {
      auto type = contextWithTemplateParams.getType(*param.type);
      if (!type)
        return param.codeLoc.getError("Type not found: " + quote(*param.type));
      templateTypes.push_back(CompileTimeValue::getTemplateValue(type.get(), param.name));
      contextWithTemplateParams.addType(param.name, templateTypes.back());
    } else {
      if (auto err = contextWithTemplateParams.checkNameConflict(param.name, "template parameter"))
        return param.codeLoc.getError(*err);
      templateTypes.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
      contextWithTemplateParams.addType(param.name, templateTypes.back());
    }
  }
  auto requirements = applyConcept(contextWithTemplateParams, templateInfo.requirements);
  if (!requirements)
    return requirements.get_error();
  if (auto returnType1 = contextWithTemplateParams.getTypeFromString(this->returnType)) {
    auto returnType = *returnType1;
    if (name.contains<Operator>())
      returnType = convertPointerToReference(returnType);
    vector<FunctionType::Param> params;
    set<string> paramNames;
    for (auto& p : parameters) {
      auto type = contextWithTemplateParams.getTypeFromString(p.type);
      if (!type)
        return type.get_error();
      if (name.contains<Operator>())
        type = convertPointerToReference(*type);
      params.push_back({p.name, std::move(*type)});
      if (p.name) {
        if (paramNames.count(*p.name))
          return p.codeLoc.getError("Duplicate function parameter name: " + quote(*p.name));
        paramNames.insert(*p.name);
      }
    }
    if (!builtInImport && !concept && name.contains<Operator>() && !paramsAreGoodForOperator(params))
      return codeLoc.getError("Operator parameters must include at least one user-defined type");
    FunctionType functionType(returnType, params, templateTypes);
    functionType.fromConcept = concept;
    functionType.requirements = *requirements;
    auto id = getFunctionId(name);
    if (id.contains<ConstructorTag>() && external)
      functionType.generatedConstructor = true;
    if (id.contains<Operator>() && external)
      functionType.setBuiltin();
    functionInfo = FunctionInfo::getDefined(id, std::move(functionType), this);
    return none;
  } else
    return returnType1.get_error();
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

optional<ErrorLoc> FunctionDefinition::generateVirtualDispatchBody(Context& bodyContext) {
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
  if (!variantType || variantType->alternatives.empty())
    return codeLoc.getError("Virtual parameter must be of a variant type or a pointer to one");
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
        return call.get_error();
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
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkAndGenerateCopyFunction(const Context& context) {
  if (!body && isDefault) {
    if (parameters.size() != 1)
      return codeLoc.getError("Expected exactly one parameter in copy function");
    auto type = context.getTypeFromString(parameters[0].type);
    if (!type)
      return type.get_error();
    if (*type != PointerType::get(context.getTypeFromString(returnType).get()))
      return codeLoc.getError("Copy function parameter type must be the same as pointer to return type");
    auto structType = type.get()->removePointer().dynamicCast<StructType>();
    if (!structType)
      return codeLoc.getError("Can only generate copy function for user-defined types");
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
  return none;
}

void FunctionDefinition::InstanceInfo::generateBody(StatementBlock* parentBody) {
  CHECK(!body);
  auto templateParams = functionInfo->parent->type.templateParams;
  for (int i = 0; i < functionInfo->type.templateParams.size(); ++i) {
    StatementBlock* useBody = (i == 0) ? parentBody : body.get();
    ErrorBuffer errors;
    body = cast<StatementBlock>(
        useBody->replace(templateParams[i], functionInfo->type.templateParams[i], errors));
    for (int j = i + 1; j < templateParams.size(); ++j)
      templateParams[j] = templateParams[j]->replace(templateParams[i], functionInfo->type.templateParams[i], errors);
    CHECK(errors.empty());
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
        CHECK(!checkBody(callTopContext, *instances.back().body, *instances.back().functionInfo));
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

optional<ErrorLoc> FunctionDefinition::generateDefaultBodies(Context& context) {
  Context bodyContext = Context::withParent(context);
  for (auto& param : functionInfo->type.templateParams)
    addTemplateParam(bodyContext, param);
  auto res = applyConcept(bodyContext, templateInfo.requirements);
  if (!res)
    return res.get_error();
  if (isVirtual)
    if (auto err = generateVirtualDispatchBody(bodyContext))
      return err;
  if (name == "copy"s)
    if (auto err = checkAndGenerateCopyFunction(bodyContext))
      return err;
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkBody(Context::ConstStates callContext, StatementBlock& myBody,
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
  if (retVal != ArithmeticType::VOID && !myBody.hasReturnStatement(bodyContext) && !name.contains<ConstructorId>())
    return codeLoc.getError("Not all paths lead to a return statement in a function returning non-void");
  return myBody.check(bodyContext);
}

optional<ErrorLoc> FunctionDefinition::check(Context& context) {
  if (auto err = generateDefaultBodies(context))
    return err;
  definitionContext = context.getAllStates();
  if (body) {
    Context paramsContext = Context::withParent(context);
    for (auto& param : functionInfo->type.templateParams)
      addTemplateParam(paramsContext, param);
    auto res = applyConcept(paramsContext, templateInfo.requirements);
    if (!res)
      return res.get_error();
    if (auto err = checkBody(paramsContext.getAllStates(), *body, *functionInfo))
      return err;
  }
  for (int i = 0; i < instances.size(); ++i)
    if (!instances[i].body) {
      instances[i].generateBody(body.get());
      if (auto err = checkBody(instances[i].callContext, *instances[i].body, *instances[i].functionInfo))
        return err;
    }
  return none;
}

optional<ErrorLoc> FunctionDefinition::addToContext(Context& context, ImportCache& cache) {
  if (auto err = setFunctionType(context, false, cache.isCurrentlyBuiltIn()))
    return err;
  if (auto err = context.addFunction(functionInfo.get()))
    return codeLoc.getError(*err);
  if (templateInfo.params.empty() && cache.currentlyInImport())
    // we are going to ignore the function body if we're in an import and it's not a template
    body = nullptr;
  return none;
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
      CHECK(!context->addImplicitFunction(op, FunctionType(ArithmeticType::VOID,
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

WithErrorLine<vector<ModuleInfo>> correctness(AST& ast, Context& context, const vector<string>& importPaths,
    bool isBuiltInModule) {
  ImportCache cache(isBuiltInModule);
  if (!isBuiltInModule)
    addBuiltInImport(ast);
  for (auto& elem : ast.elems) {
    if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
      import->setImportDirs(importPaths);
    if (auto err = elem->addToContext(context, cache))
      return *err;
  }
  for (auto& elem : ast.elems) {
    if (auto err = elem->check(context))
      return *err;
  }
  return cache.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

optional<ErrorLoc> ExpressionStatement::check(Context& context) {
  auto res = getType(context, expr);
  if (!res)
    return res.get_error();
  if (!canDiscard && res.get() != ArithmeticType::VOID)
    return codeLoc.getError("Expression result of type " + quote(res->get()->getName()) + " discarded");
  if (canDiscard && res.get() == ArithmeticType::VOID)
    return codeLoc.getError("Void expression result unnecessarily marked as discarded");
  return none;
}

unique_ptr<Statement> ExpressionStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<ExpressionStatement>(expr->replace(from, to, errors));
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

WithErrorLine<SType> FunctionCall::getTypeImpl(Context& context) {
  return getDotOperatorType(nullptr, context);
}

WithErrorLine<SType> FunctionCall::getDotOperatorType(Expression* left, Context& callContext) {
  optional<ErrorLoc> error;
  if (!templateArgs) {
    if (auto res = callContext.getTypeList(identifier->parts.back().templateArguments))
      templateArgs = res.get();
    else
      return res.get_error();
  }
  if (!identifierType) {
    if (auto res = callContext.getIdentifierType(*identifier))
      identifierType = *res;
    else
      return identifier->codeLoc.getError(res.get_error());
  }
  if (!functionInfo) {
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      if (auto t = getType(callContext, arguments[i]))
        argTypes.push_back(*t);
      else
        return t.get_error();
      argLocs.push_back(arguments[i]->codeLoc);
      INFO << "Function argument " << argTypes.back()->getName();
    }
    if (!left)
      getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs).unpack(functionInfo, error);
    else {
      auto leftTypeTmp = left->getTypeImpl(callContext);
      if (!leftTypeTmp)
        return leftTypeTmp;
      SType leftType = *leftTypeTmp;
      callType = MethodCallType::METHOD;
      auto tryMethodCall = [&](MethodCallType thisCallType) {
        auto res = getFunction(callContext, codeLoc, *identifierType, *templateArgs, concat({leftType}, argTypes),
            concat({left->codeLoc}, argLocs));
        if (res)
          callType = thisCallType;
        if (res && functionInfo) {
          error = codeLoc.getError("Ambigous method call:\nCandidate: " + functionInfo->prettyString() +
              "\nCandidate: " + res.get()->prettyString());
          functionInfo = nullptr;
        } else
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
        return arguments[i]->codeLoc.getError("Function argument " + quote(*argNames[i]) +
            " doesn't match parameter " + quote(*paramName) + " of function " +
            functionInfo->prettyString());
      }
    }
    if (auto parent = functionInfo->parent)
      if (parent->definition)
        parent->definition->addInstance(callContext, functionInfo.get());
    return functionInfo->type.retVal;
  }
  return *error;
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

unique_ptr<Expression> FunctionCall::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<FunctionCall>(codeLoc, Private{});
  ret->identifier = identifier;
  for (auto& arg : arguments)
    ret->arguments.push_back(arg->replace(from, to, errors));
  ret->templateArgs.emplace();
  for (auto& arg : *templateArgs)
    ret->templateArgs->push_back(arg->replace(from, to, errors));
  ret->argNames = argNames;
  ret->identifierType = identifierType->replace(from, to, errors);
  return ret;
}

FunctionCall::FunctionCall(CodeLoc l, Private) : Expression(l) {}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

optional<ErrorLoc> SwitchStatement::check(Context& context) {
  if (auto t = getType(context, expr))
    return t.get()->handleSwitchStatement(*this, context, Type::SwitchArgument::VALUE);
  else
    return t.get_error();
}

unique_ptr<Statement> SwitchStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<SwitchStatement>(codeLoc, expr->replace(from, to, errors));
  ret->targetType = targetType->replace(from, to, errors);
  if (defaultBlock)
    ret->defaultBlock = cast<StatementBlock>(defaultBlock->replace(from, to, errors));
  for (auto& elem : caseElems)
    ret->caseElems.push_back(elem.replace(from, to, errors));
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

static WithErrorLine<shared_ptr<StructType>> getNewOrIncompleteStruct(Context& context, string name, CodeLoc codeLoc,
    const TemplateInfo& templateInfo) {
  if (auto existing = context.getType(name)) {
    if (auto asStruct = existing.get().dynamicCast<StructType>()) {
      auto returnType = asStruct;
      if (!returnType->incomplete) {
        // if it's not an incomplete type then this triggers a conflict error
        if (auto err = context.checkNameConflict(name, "Type"))
          return codeLoc.getError(*err);
        fail();
      }
      returnType->incomplete = false;
      return returnType;
    } else {
      if (auto err = context.checkNameConflict(name, "Type"))
        return codeLoc.getError(*err);
      fail();
    }
  } else {
    if (auto err = context.checkNameConflict(name, "Type"))
      return codeLoc.getError(*err);
    auto paramsContext = Context::withParent(context);
    auto returnType = StructType::get(name);
    for (auto& param : templateInfo.params) {
      if (param.type) {
        if (auto type = paramsContext.getType(*param.type)) {
          auto valueType = CompileTimeValue::getTemplateValue(type.get(), param.name);
          returnType->templateParams.push_back(valueType);
        } else
          return param.codeLoc.getError("Type not found: " + quote(*param.type));
      } else {
        returnType->templateParams.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
        paramsContext.addType(param.name, returnType->templateParams.back());
      }
    }
    context.addType(name, returnType);
    return returnType;
  }
}

optional<ErrorLoc> VariantDefinition::addToContext(Context& context) {
  auto res = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo);
  if (!res)
    return res.get_error();
  type = *res;
  auto membersContext = Context::withParent(context);
  if (templateInfo.params.size() != type->templateParams.size())
    return codeLoc.getError("Number of template parameters differs from forward declaration");
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  if (auto res = applyConcept(membersContext, templateInfo.requirements))
    type->requirements = *res;
  else
    return res.get_error();
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    if (subtypeNames.count(subtype.name))
      return subtype.codeLoc.getError("Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    vector<FunctionType::Param> params;
    auto subtypeInfo = membersContext.getTypeFromString(subtype.type).get();
    if (subtypeInfo != ArithmeticType::VOID)
      params.push_back(FunctionType::Param{subtypeInfo});
    auto constructor = FunctionType(type.get(), params, {});
    constructor.parentType = type.get();
    CHECK(!type->staticContext.addImplicitFunction(subtype.name, constructor));
  }
  return none;
}

optional<ErrorLoc> VariantDefinition::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  CHECK(!!applyConcept(bodyContext, templateInfo.requirements));
  for (auto& subtype : elements)
    type->alternatives.push_back({subtype.name, bodyContext.getTypeFromString(subtype.type).get()});
  type->updateInstantations();
  return none;
}

optional<ErrorLoc> StructDefinition::addToContext(Context& context, ImportCache& cache) {
  if (auto typeTmp = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo))
    type = *typeTmp;
  else
    return typeTmp.get_error();
  type->incomplete = incomplete;
  if (!incomplete) {
    if (templateInfo.params.size() != type->templateParams.size())
      return codeLoc.getError("Number of template parameters differs from forward declaration");
    auto membersContext = Context::withParent(context);
    for (auto& param : type->templateParams)
      addTemplateParam(membersContext, param);
    if (auto res = applyConcept(membersContext, templateInfo.requirements))
      type->requirements = *res;
    else
      return res.get_error();
    for (auto& member : members)
      if (auto memberType = membersContext.getTypeFromString(member.type))
        type->members.push_back({member.name, *memberType});
      else
        return memberType.get_error();
    for (auto& member : members)
      if (auto error = type->members.back().type->getSizeError())
        return member.codeLoc.getError("Member " + quote(member.name) + " of type " +
            quote(type->getName()) + " " + *error);
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
  return none;
}

optional<ErrorLoc> StructDefinition::check(Context& context) {
  auto methodBodyContext = Context::withParent(context);
  auto staticFunContext = Context::withParent({&context, &type->staticContext});
  for (auto& param : type->templateParams)
    addTemplateParam(methodBodyContext, param);
  CHECK(!!applyConcept(methodBodyContext, templateInfo.requirements));
  type->updateInstantations();
  if (auto error = type->getSizeError())
    return codeLoc.getError("Type " + quote(type->getName()) + " " + *error);
  return none;
}

MoveExpression::MoveExpression(CodeLoc l, string id) : Expression(l), identifier(id) {
}

WithErrorLine<SType> MoveExpression::getTypeImpl(Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
      if (!ret.get_value().dynamicCast<MutableReferenceType>() &&
          !ret.get_value().dynamicCast<ReferenceType>())
        return codeLoc.getError("Can't move from " + quote(ret.get_value()->getName()));
      if (auto err = context.setVariableAsMoved(identifier))
        return codeLoc.getError(*err);
      type = ret.get_value()->getUnderlying();
    } else
      return codeLoc.getError(ret.get_error());
  }
  return type.get();
}

unique_ptr<Expression> MoveExpression::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<MoveExpression>(codeLoc, identifier);
  ret->type = type->replace(from, to, errors);
  return ret;
}


EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

optional<ErrorLoc> EmbedStatement::check(Context&) {
  return none;
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  if (isPublic)
    return TopLevelAllowance::MUST;
  else
    return TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<EmbedStatement>(codeLoc, value);
  ret->replacements = replacements;
  ret->replacements.push_back({from, to});
  return ret;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

optional<ErrorLoc> ForLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  if (auto err = init->check(bodyContext))
    return *err;
  auto condType = getType(bodyContext, cond);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  auto res = getType(bodyContext, iter);
  if (!res)
    return res.get_error();
  bodyContext.setBreakAllowed();
  return body->check(bodyContext);
}

unique_ptr<Statement> ForLoopStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<ForLoopStatement>(codeLoc,
      init->replace(from, to, errors),
      cond->replace(from, to, errors),
      iter->replace(from, to, errors),
      body->replace(from, to, errors));
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b)
  : Statement(l), cond(std::move(c)), body(std::move(b)) {}

optional<ErrorLoc> WhileLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  auto condType = getType(bodyContext, cond);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  bodyContext.setBreakAllowed();
  return body->check(bodyContext);
}

unique_ptr<Statement> WhileLoopStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<WhileLoopStatement>(codeLoc,
      cond->replace(from, to, errors),
      body->replace(from, to, errors));
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool pub, bool isBuiltIn)
    : Statement(l), path(p), isPublic(pub), isBuiltIn(isBuiltIn) {
}

void ImportStatement::setImportDirs(const vector<string>& p) {
  importDirs = p;
}

optional<ErrorLoc> ImportStatement::check(Context&) {
  return none;
}

optional<ErrorLoc> ImportStatement::processImport(Context& context, ImportCache& cache, const string& content,
    const string& path) {
  if (cache.isCurrentlyImported(path))
    return codeLoc.getError("Public import cycle: " + combine(cache.getCurrentImports(), ", "));
  if ((!isPublic && cache.currentlyInImport())) {
    INFO << "Skipping non public import " << path;
    return none;
  }
  if (!cache.contains(path)) {
    INFO << "Parsing import " << path;
    cache.pushCurrentImport(path, isBuiltIn);
    if (auto tokens = lex(content, CodeLoc(path, 0, 0), "end of file")) {
      if (auto parsed = parse(std::move(*tokens)))
        ast = unique<AST>(std::move(*parsed));
      else
        return parsed.get_error();
    } else
      return tokens.get_error();
    if (!isBuiltIn && !cache.isCurrentlyBuiltIn())
      addBuiltInImport(*ast);
    Context importContext = createNewContext();
    for (auto& elem : ast->elems) {
      if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
        import->setImportDirs(importDirs);
      if (auto err = elem->addToContext(importContext, cache))
        return err;
    }
    for (auto& elem : ast->elems)
      if (auto err = elem->check(importContext))
        return err;
    cache.popCurrentImport(isBuiltIn);
    cache.insert(path, std::move(importContext), isBuiltIn || cache.isCurrentlyBuiltIn());
  } else
    INFO << "Import " << path << " already cached";
  context.merge(cache.getContext(path));
  return none;
}

optional<ErrorLoc> ImportStatement::addToContext(Context& context, ImportCache& cache) {
  INFO << "Resolving import " << path << " from " << codeLoc.file;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    INFO << "Trying directory " << importDir;
    auto importPath = fs::path(importDir)  / path;
    if (auto content = readFromFile(importPath.c_str())) {
      importPath = fs::canonical(importPath);
      INFO << "Imported file " << importPath;
      if (auto err = processImport(context, cache, content->value, importPath))
        return err;
      return none;
    }
  }
  return codeLoc.getError("Couldn't resolve import path: " + path);
}

nullable<SType> Expression::eval(const Context&) const {
  return nullptr;
}

WithErrorLine<SType> Expression::getDotOperatorType(Expression* left, Context& callContext) {
  return codeLoc.getError("Bad use of dot operator");
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

optional<ErrorLoc> EnumDefinition::addToContext(Context& s) {
  if (elements.empty())
    return codeLoc.getError("Enum requires at least one element");
  unordered_set<string> occurences;
  for (auto& e : elements)
    if (occurences.count(e))
      return codeLoc.getError("Duplicate enum element: " + quote(e));
  s.addType(name, shared<EnumType>(name, elements));
  return none;
}

optional<ErrorLoc> EnumDefinition::check(Context& s) {
  return none;
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

WithErrorLine<SType> EnumConstant::getTypeImpl(Context& context) {
  auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc));
  if (!type)
    return type.get_error();
  if (auto enumType = type->dynamicCast<EnumType>()) {
    if (!contains(enumType->elements, enumElement))
      return codeLoc.getError(quote(enumElement) + " is not an element of enum " + quote(enumName));
  } else
    return codeLoc.getError(quote(type.get()->getName()) + " is not an enum type");
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

unique_ptr<Expression> EnumConstant::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<EnumConstant>(codeLoc, enumName, enumElement);
  ret->enumType = enumType->replace(from, to, errors);
  return ret;
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {
}

optional<ErrorLoc> ConceptDefinition::addToContext(Context& context) {
  shared_ptr<Concept> concept = shared<Concept>(name);
  auto declarationsContext = Context::withParent(context);
  for (auto& param : templateInfo.params) {
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back());
  }
  for (auto& function : functions) {
    if (function->isVirtual)
      return function->codeLoc.getError("Virtual functions are not allowed here");
    if (auto err = function->setFunctionType(declarationsContext, true))
      return err;
    if (auto err = function->check(declarationsContext))
      return err;
    if (auto err = concept->modContext().addFunction(function->functionInfo.get()))
      return function->codeLoc.getError(*err);
  }
  context.addConcept(name, concept);
  return none;
}

optional<ErrorLoc> ConceptDefinition::check(Context& context) {
  return none;
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

optional<ErrorLoc> RangedLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  auto containerTypeTmp = getType(context, container);
  if (!containerTypeTmp)
    return containerTypeTmp.get_error();
  auto containerType = containerTypeTmp.get();
  if (containerType->getUnderlying() == containerType)
    containerType = ReferenceType::get(containerType);
  auto uniqueSufix = to_string(codeLoc.line) + "_" + to_string(codeLoc.column);
  containerName = "container"s + uniqueSufix;
  auto containerEndName = "container_end"s + uniqueSufix;
  bodyContext.addVariable(*containerName, containerType);
  containerEnd = unique<VariableDeclaration>(codeLoc, none, containerEndName,
      BinaryExpression::get(codeLoc, Operator::MEMBER_ACCESS,
          unique<Variable>(codeLoc, *containerName), unique<FunctionCall>(codeLoc, IdentifierInfo("end", codeLoc))));
  if (auto err = containerEnd->check(bodyContext))
    return err;
  init->initExpr = BinaryExpression::get(codeLoc, Operator::MEMBER_ACCESS,
      unique<Variable>(codeLoc, *containerName), unique<FunctionCall>(codeLoc, IdentifierInfo("begin", codeLoc)));
  init->isMutable = true;
  condition = BinaryExpression::get(codeLoc, Operator::NOT_EQUAL, unique<Variable>(codeLoc, init->identifier),
      unique<Variable>(codeLoc, containerEndName));
  increment = unique<UnaryExpression>(codeLoc, Operator::INCREMENT, unique<Variable>(codeLoc, init->identifier));
  if (auto err = init->check(bodyContext))
    return err;
  auto condType = getType(bodyContext, condition);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return codeLoc.getError("Equality comparison between iterators does not return type " + quote("bool"));
  auto incType = getType(bodyContext, increment);
  if (!incType)
    return incType.get_error();
  bodyContext.setBreakAllowed();
  return body->check(bodyContext);
}

unique_ptr<Statement> RangedLoopStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<RangedLoopStatement>(codeLoc,
      cast<VariableDeclaration>(init->replace(from, to, errors)),
      container->replace(from, to, errors),
      body->replace(from, to, errors));
  ret->condition = condition->replace(from, to, errors);
  ret->increment = increment->replace(from, to, errors);
  ret->containerName = containerName;
  ret->containerEnd = cast<VariableDeclaration>(containerEnd->replace(from, to, errors));
  return ret;
}

optional<ErrorLoc> BreakStatement::check(Context& context) {
  if (!context.breakAllowed())
    return codeLoc.getError("Break statement outside of a loop");
  return none;
}

unique_ptr<Statement> BreakStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<BreakStatement>(codeLoc);
}

optional<ErrorLoc> ContinueStatement::check(Context& context) {
  if (!context.breakAllowed())
    return codeLoc.getError("Continue statement outside of a loop");
  return none;
}

unique_ptr<Statement> ContinueStatement::replace(SType from, SType to, ErrorBuffer& errors) const {
  return unique<ContinueStatement>(codeLoc);
}

ArrayLiteral::ArrayLiteral(CodeLoc codeLoc) : Expression(codeLoc) {
}

WithErrorLine<SType> ArrayLiteral::getTypeImpl(Context& context) {
  auto typeTmp = getType(context, contents[0]);
  if (!typeTmp)
    return typeTmp.get_error();
  auto ret = typeTmp.get()->getUnderlying();
  for (int i = 1; i < contents.size(); ++i) {
    typeTmp = getType(context, contents[i]);
    if (!typeTmp)
      return typeTmp.get_error();
    auto t = typeTmp.get()->getUnderlying();
    if (t != ret)
      return contents[i]->codeLoc.getError("Incompatible types in array literal: " +
        quote(ret->getName()) + " and " + quote(t->getName()));
  }
  return SType(ArrayType::get(ret, CompileTimeValue::get((int)contents.size())));
}

unique_ptr<Expression> ArrayLiteral::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = unique<ArrayLiteral>(codeLoc);
  for (auto& elem : contents)
    ret->contents.push_back(elem->replace(from, to, errors));
  return ret;
}


WithErrorLine<SType> getType(Context& context, unique_ptr<Expression>& expr, bool evaluateAtCompileTime) {
  auto type = expr->getTypeImpl(context);
  if (!type)
    return type;
  if (evaluateAtCompileTime) {
    if (auto type = expr->eval(context)) {
      if (auto value = type.get().dynamicCast<CompileTimeValue>())
        expr = unique<Constant>(expr->codeLoc, value);
    }
  }
  return type;
}

WithErrorLine<nullable<SType>> SwitchStatement::CaseElem::getType(const Context& context) {
  return type.visit(
      [&](const IdentifierInfo& id) -> WithErrorLine<nullable<SType>> {
        if (auto ret = context.getTypeFromString(id)) {
          type = ret.get();
          return nullable<SType>(ret.get());
        } else
          return ret.get_error();
      },
      [](const SType& t) -> WithErrorLine<nullable<SType>> { return nullable<SType>(t);},
      [](none_t) -> WithErrorLine<nullable<SType>> { return nullable<SType>(); }
  );
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::replace(SType from, SType to, ErrorBuffer& errors) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.id = id;
  ret.block = cast<StatementBlock>(block->replace(from, to, errors));
  if (auto t = type.getReferenceMaybe<SType>())
    ret.type = (*t)->replace(from, to, errors);
  else
    ret.type = type;
  return ret;
}

ExternConstantDeclaration::ExternConstantDeclaration(CodeLoc l, IdentifierInfo type, string identifier)
  : Statement(l), type(type), identifier(identifier) {
}

optional<ErrorLoc> ExternConstantDeclaration::check(Context& context) {
  if (auto err = context.checkNameConflict(identifier, "Variable"))
    return codeLoc.getError(*err);
  if (auto t = context.getTypeFromString(type))
    realType = *t;
  else
    return t.get_error();
  if (realType == ArithmeticType::VOID)
    return codeLoc.getError("Can't declare constant of type " + quote(ArithmeticType::VOID->getName()));
  INFO << "Adding extern constant " << identifier << " of type " << realType.get()->getName();
  context.addVariable(identifier, ReferenceType::get(realType.get()));
  return none;
}
