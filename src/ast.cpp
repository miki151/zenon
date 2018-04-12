#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"

Node::Node(CodeLoc l) : codeLoc(l) {}


BinaryExpression::BinaryExpression(CodeLoc loc, Operator o, unique_ptr<Expression> u1, unique_ptr<Expression> u2)
    : Expression(loc), op(o) {
  e1 = std::move(u1);
  e2 = std::move(u2);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<Expression> c, unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), cond(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, SType t, string v) : Expression(l), type(t), value(v) {
  INFO << "Created constant " << quote(v) << " of type " << getName(t);
}

Variable::Variable(CodeLoc l, string id) : Expression(l), identifier(id) {
  INFO << "Parsed variable " << id;
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {
  INFO << "Function call " << id.toString();;
}

VariableDeclaration::VariableDeclaration(CodeLoc l, optional<IdentifierInfo> t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  string type = "auto";
  if (t)
    type = t->toString();
  INFO << "Declared variable " << quote(id) << " of type " << quote(type);
}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, string n) : Statement(l), returnType(r), nameOrOp(n) {}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, Operator op) : Statement(l), returnType(r), nameOrOp(op) {}

SType Constant::getType(const State&) {
  return type;
}

SType Variable::getType(const State& state) {
  if (auto ret = state.getTypeOfVariable(identifier))
    return ret.get();
  else
    codeLoc.error("Undefined variable: " + identifier);
}

nullable<SType> Variable::getDotOperatorType(const State& idContext, const State& callContext) {
  return getType(idContext);
}

SType BinaryExpression::getType(const State& state) {
  return getOperationResult(e1->codeLoc, op, state, *e1, *e2);
}

void StatementBlock::check(State& state) {
  auto stateCopy = state;
  for (auto& s : elems) {
    s->check(stateCopy);
  }
}

void IfStatement::check(State& state) {
  auto condType = cond->getType(state);
  codeLoc.check(canConvert(condType, ArithmeticType::BOOL), "Expected a type convertible to bool inside if statement, got "
      + quote(getName(condType)));
  ifTrue->check(state);
  if (ifFalse)
    ifFalse->check(state);
}

void VariableDeclaration::check(State& state) {
  state.checkNameConflict(codeLoc, identifier, "Variable");
  auto inferType = [&] () -> SType {
    if (type) {
      if (auto typeId = state.getTypeFromString(*type))
        return typeId.get();
      else
        codeLoc.error("Type " + quote(type->toString()) + " not recognized");
    }
    else if (initExpr)
      return initExpr->getType(state);
    else
      codeLoc.error("Initializing expression needed to infer variable type");
  };
  realType = inferType();
  INFO << "Adding variable " << identifier << " of type " << getName(realType.get());
  if (initExpr) {
    auto exprType = initExpr->getType(state);
    initExpr->codeLoc.check(canAssign(ReferenceType::get(realType.get()), exprType), "Can't initialize variable of type "
        + quote(getName(realType.get())) + " with value of type " + quote(getName(exprType)));
  } else
    codeLoc.check(!requiresInitialization(realType.get()), "Type " + quote(getName(realType.get())) + " requires initialization");
  state.addVariable(identifier, ReferenceType::get(realType.get()));
}

void ReturnStatement::check(State& state) {
  if (!expr)
    codeLoc.check(state.getReturnType() && state.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(state);
    codeLoc.check(canAssign(ReferenceType::get(state.getReturnType().get()), returnType),
        "Attempting to return value of type "s + quote(getName(returnType)) +
         " in a function returning "s + quote(getName(state.getReturnType().get())));
  }
}

void Statement::addToState(State&) {}

bool Statement::hasReturnStatement(const State&) const {
  return false;
}

bool IfStatement::hasReturnStatement(const State& state) const {
  return ifTrue->hasReturnStatement(state) && ifFalse && ifFalse->hasReturnStatement(state);
}

bool StatementBlock::hasReturnStatement(const State& state) const {
  for (auto& s : elems)
    if (s->hasReturnStatement(state))
      return true;
  return false;
}

bool ReturnStatement::hasReturnStatement(const State&) const {
  return true;
}

void FunctionDefinition::setFunctionType(const State& state) {
  State stateCopy = state;
  if (auto name = nameOrOp.getReferenceMaybe<string>())
    state.checkNameConflict(codeLoc, *name, "Function");
  else {
    auto op = *nameOrOp.getValueMaybe<Operator>();
    codeLoc.check(!state.getOperatorType(op), "Operator " + quote(getString(op)) + " already defined");
  }
  vector<SType> templateTypes;
  for (auto& param : templateParams) {
    templateTypes.push_back(make_shared<Type>(TemplateParameter{param}));
    stateCopy.addType(param, templateTypes.back());
  }
  if (auto returnType = stateCopy.getTypeFromString(this->returnType)) {
    vector<FunctionType::Param> params;
    for (auto& p : parameters)
      if (auto paramType = stateCopy.getTypeFromString(p.type)) {
        params.push_back({p.name, paramType.get()});
      } else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    functionType = FunctionType(FunctionCallType::FUNCTION, returnType.get(), params, templateTypes );
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
}

void FunctionDefinition::checkFunction(State& state, bool templateStruct) {
  State stateCopy = state;
  vector<SType> templateTypes;
  if (auto op = nameOrOp.getValueMaybe<Operator>()) {
    codeLoc.check(templateParams.empty(), "Operator overload can't have template parameters.");
    switch (*op) {
      case Operator::SUBSCRIPT:
        codeLoc.check(parameters.size() == 1, "Operator " + quote("[]") + " must take one parameter.");
        break;
      default:
        codeLoc.error("Operator " + quote(getString(*op)) + " overload not supported.");
        break;
    }
  }
  for (auto& param : templateParams) {
    templateTypes.push_back(make_shared<Type>(TemplateParameter{param}));
    stateCopy.addType(param, templateTypes.back());
  }
  if (auto returnType = stateCopy.getTypeFromString(this->returnType)) {
    for (auto& p : parameters)
      if (auto paramType = stateCopy.getTypeFromString(p.type))
        stateCopy.addVariable(p.name, paramType.get());
      else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    stateCopy.setReturnType(returnType.get());
    if (returnType != ArithmeticType::VOID && body && !body->hasReturnStatement(state))
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
  if (body && (!templateParams.empty() || templateStruct || state.getImports().empty()))
    body->check(stateCopy);
}

void FunctionDefinition::check(State& state) {
  checkFunction(state, false);
}

void FunctionDefinition::addToState(State& state) {
  setFunctionType(state);
  state.addFunction(nameOrOp, *functionType);
}

void correctness(const AST& ast) {
  State state;
  for (auto type : {ArithmeticType::INT, ArithmeticType::BOOL,
       ArithmeticType::VOID, ArithmeticType::CHAR, ArithmeticType::STRING})
    state.addType(getName(type), type);
  for (auto& elem : ast.elems)
    elem->addToState(state);
  for (auto& elem : ast.elems) {
    elem->check(state);
  }
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(State& state) {
  expr->getType(state);
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

SType FunctionCall::getType(const State& state) {
  return getDotOperatorType(state, state).get();
}

static FunctionType getFunction(const State& idContext, const State& callContext, CodeLoc codeLoc, IdentifierInfo id,
    const vector<SType>& argTypes, const vector<CodeLoc>& argLoc) {
  auto templateType = idContext.getFunctionTemplate(codeLoc, id);
  return callContext.instantiateFunctionTemplate(codeLoc, templateType, id, argTypes, argLoc);
}

nullable<SType> FunctionCall::getDotOperatorType(const State& idContext, const State& callContext) {
  vector<SType> argTypes;
  vector<CodeLoc> argLocs;
  for (int i = 0; i < arguments.size(); ++i) {
    argTypes.push_back(arguments[i]->getType(callContext));
    argLocs.push_back(arguments[i]->codeLoc);
    INFO << "Function argument " << getName(argTypes.back());
  }
  functionType = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  return functionType->retVal;
}

FunctionCallNamedArgs::FunctionCallNamedArgs(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {}

SType FunctionCallNamedArgs::getType(const State& state) {
  return getDotOperatorType(state, state).get();
}

nullable<SType> FunctionCallNamedArgs::getDotOperatorType(const State& idContext, const State& callContext) {
  set<string> toInitialize;
  set<string> initialized;
  map<string, int> paramIndex;
  vector<string> paramNames = idContext.getFunctionParamNames(codeLoc, identifier);
  int count = 0;
  for (auto& param : paramNames) {
    toInitialize.insert(param);
    paramIndex[param] = count++;
  }
  for (auto& elem : arguments) {
    elem.codeLoc.check(toInitialize.count(elem.name), "No parameter named " + quote(elem.name)
        + " in function " + quote(identifier.toString()));
    elem.codeLoc.check(!initialized.count(elem.name), "Parameter " + quote(elem.name) + " listed more than once");
    initialized.insert(elem.name);
  }
  vector<string> notInitialized;
  for (auto& elem : toInitialize)
    if (!initialized.count(elem))
      notInitialized.push_back("" + quote(elem));
  codeLoc.check(notInitialized.empty(), "Function parameters: " + combine(notInitialized, ",")
      + " were not initialized" );
  sort(arguments.begin(), arguments.end(),
      [&](const Argument& m1, const Argument& m2) { return paramIndex[m1.name] < paramIndex[m2.name]; });
  vector<SType> argTypes;
  vector<CodeLoc> argLocs;
  for (auto& arg : arguments) {
    argTypes.push_back(arg.expr->getType(callContext));
    argLocs.push_back(arg.codeLoc);
  }
  functionType = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  return functionType->retVal;
}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

void SwitchStatement::check(State& state) {
  auto exprType = getUnderlying(expr->getType(state));
  if (auto t = exprType->getValueMaybe<StructType>()) {
    expr->codeLoc.check(t->kind == StructType::VARIANT, "Expected a variant or enum type");
    checkVariant(state, *t, getName(exprType));
    type = VARIANT;
  } else if (auto t = exprType->getValueMaybe<EnumType>()) {
    checkEnum(state, *t, getName(exprType));
    type = ENUM;
  } else
    expr->codeLoc.error("Can't switch on value of type " + quote(getName(exprType)));
}

void SwitchStatement::checkEnum(State& state, EnumType inputType, const string& typeName) {
  unordered_set<string> handledElems;
  subtypesPrefix = inputType.name + "::";
  for (auto& caseElem : caseElems) {
    caseElem.codeloc.check(!caseElem.type, "Expected enum element");
    caseElem.codeloc.check(contains(inputType.elements, caseElem.id), "Element " + quote(caseElem.id) +
        " not present in enum" + quote(typeName));
    caseElem.codeloc.check(!handledElems.count(caseElem.id), "Enum element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledElems.insert(caseElem.id);
    caseElem.block->check(state);
  }
  if (!defaultBlock) {
    vector<string> unhandled;
    for (auto& elem : inputType.elements)
      if (!handledElems.count(elem))
        unhandled.push_back(quote(elem));
    codeLoc.check(unhandled.empty(), quote(typeName) + " elements " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    defaultBlock->codeLoc.check(handledElems.size() < inputType.elements.size(),
        "Default switch statement unnecessary when all enum elements are handled");
    defaultBlock->check(state);
  }
}

void SwitchStatement::checkVariant(State& state, StructType inputType, const string& typeName) {
  subtypesPrefix = inputType.name;
  if (!inputType.templateParams.empty()) {
    subtypesPrefix += "<";
    for (auto& t : inputType.templateParams)
      subtypesPrefix += getName(t) + ",";
    subtypesPrefix.pop_back();
    subtypesPrefix += ">";
  }
  subtypesPrefix += "::";
  unordered_set<string> handledTypes;
  for (auto& caseElem : caseElems) {
    caseElem.codeloc.check(!!inputType.getMember(caseElem.id), "Element " + quote(caseElem.id) +
        " not present in variant " + quote(typeName));
    caseElem.codeloc.check(!handledTypes.count(caseElem.id), "Variant element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledTypes.insert(caseElem.id);
    auto stateCopy = state;
    auto realType = inputType.getMember(caseElem.id).get();
    caseElem.declareVar = !(realType == ArithmeticType::VOID);
    if (caseElem.declareVar)
      stateCopy.addVariable(caseElem.id, realType);
    if (caseElem.type) {
      if (auto t = state.getTypeFromString(*caseElem.type))
        caseElem.type->codeLoc.check(t == realType, "Can't handle variant element "
            + quote(caseElem.id) + " of type " + quote(getName(realType)) + " as type " + quote(getName(t.get())));
    }
    caseElem.block->check(stateCopy);
  }
  if (!defaultBlock) {
    vector<string> unhandled;
    for (auto& elem : inputType.members)
      if (!handledTypes.count(elem.name))
        unhandled.push_back(quote(elem.name));
    codeLoc.check(unhandled.empty(), quote(typeName) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    defaultBlock->codeLoc.check(handledTypes.size() < inputType.members.size(),
        "Default switch statement unnecessary when all variant cases are handled");
    defaultBlock->check(state);
  }
}

bool SwitchStatement::hasReturnStatement(const State& state) const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement(state))
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement(state))
    return false;
  return true;
}

VariantDefinition::VariantDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

void VariantDefinition::addToState(State& state) {
  state.checkNameConflict(codeLoc, name, "Type");
  type = StructType::get(StructType::VARIANT, name);
  auto membersContext = state;
  auto& structType = *type->getReferenceMaybe<StructType>();
  for (auto& param : templateParams)
    structType.templateParams.push_back(make_shared<Type>(TemplateParameter{param}));
  for (auto& param : structType.templateParams)
    membersContext.addType(getName(param), param);
  struct ConstructorInfo {
    string subtypeName;
    FunctionCallType callType;
    vector<FunctionType::Param> constructorParams;
  };
  vector<ConstructorInfo> constructors;
  for (auto& subtype : elements) {
    ConstructorInfo constructorInfo;
    constructorInfo.callType = FunctionCallType::FUNCTION;
    constructorInfo.subtypeName = subtype.name;
    if (auto subtypeInfo = membersContext.getTypeFromString(subtype.type)) {
      if (subtypeInfo != ArithmeticType::VOID)
        constructorInfo.constructorParams.push_back(FunctionType::Param{"", subtypeInfo.get()});
    } else
      subtype.codeLoc.error("Unrecognized type: " + quote(subtype.type.toString()));
    constructors.push_back(constructorInfo);
  }
  for (auto& elem : constructors)
    structType.staticMethods.push_back({elem.subtypeName, FunctionType(elem.callType, type.get(), elem.constructorParams, {})});
  state.addType(name, type.get());
}

void VariantDefinition::check(State& state) {
  auto& structType = *type->getReferenceMaybe<StructType>();
  unordered_set<string> subtypeNames;
  State methodBodyContext = state;
  for (auto& param : structType.templateParams)
    methodBodyContext.addType(getName(param), param);
  for (auto& subtype : elements) {
    subtype.codeLoc.check(!subtypeNames.count(subtype.name), "Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    if (auto subtypeInfo = methodBodyContext.getTypeFromString(subtype.type))
      structType.members.push_back({subtype.name, subtypeInfo.get()});
    else
      subtype.codeLoc.error("Unrecognized type: " + quote(subtype.type.toString()));
  }
  for (auto& method : methods) {
    method->setFunctionType(methodBodyContext);
    methodBodyContext.addFunction(method->nameOrOp, *method->functionType);
  }
  methodBodyContext.addVariable("this", PointerType::get(type.get()));
  for (int i = 0; i < methods.size(); ++i) {
    methods[i]->checkFunction(methodBodyContext, !templateParams.empty());
    structType.methods.push_back({methods[i]->nameOrOp, *methods[i]->functionType});
  }
  structType.updateInstantations();
}

void StructDefinition::addToState(State& state) {
  state.checkNameConflict(codeLoc, name, "Type");
  auto membersContext = state;
  type = StructType::get(StructType::STRUCT, name);
  auto& structType = *type->getReferenceMaybe<StructType>();
  for (auto& param : templateParams)
    structType.templateParams.push_back(make_shared<Type>(TemplateParameter{param}));
  for (auto& param : structType.templateParams)
    membersContext.addType(getName(param), param);
  vector<FunctionType::Param> constructorParams;
  for (auto& member : members)
    if (auto memberType = membersContext.getTypeFromString(member.type))
      constructorParams.push_back({member.name, memberType.get()});
  auto constructor = FunctionType(FunctionCallType::CONSTRUCTOR, type.get(), std::move(constructorParams), structType.templateParams);
  state.addFunction(name, constructor);
  state.addType(name, type.get());
}

void StructDefinition::check(State& state) {
  auto& structType = *type->getReferenceMaybe<StructType>();
  auto methodBodyContext = state;
  for (auto& param : structType.templateParams)
    methodBodyContext.addType(getName(param), param);
  for (auto& member : members) {
    INFO << "Struct member " << member.name << " " << member.type.toString() << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
    if (auto memberType = methodBodyContext.getTypeFromString(member.type)) {
      structType.members.push_back({member.name, memberType.get()});
      methodBodyContext.addVariable(member.name, ReferenceType::get(memberType.get()));
    } else
      member.codeLoc.error("Type " + quote(member.type.toString()) + " not recognized");
  }
  vector<FunctionType> methodTypes;
  for (auto& method : methods) {
    method->setFunctionType(methodBodyContext);
    methodBodyContext.addFunction(method->nameOrOp, *method->functionType);
  }
  for (int i = 0; i < methods.size(); ++i) {
    methods[i]->checkFunction(methodBodyContext, !templateParams.empty());
    structType.methods.push_back({methods[i]->nameOrOp, *methods[i]->functionType});
  }
  structType.updateInstantations();
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

SType UnaryExpression::getType(const State& state) {
  return getUnaryOperationResult(expr->codeLoc, op, expr->getType(state));
}

EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

void EmbedStatement::check(State&) {
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  if (isPublic)
    return TopLevelAllowance::MUST;
  else
    return TopLevelAllowance::CAN;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

bool ForLoopStatement::hasReturnStatement(const State& s) const {
  return body->hasReturnStatement(s);
}

void ForLoopStatement::check(State& s) {
  auto stateCopy = s;
  init->check(stateCopy);
  cond->codeLoc.check(cond->getType(stateCopy) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  iter->getType(stateCopy);
  body->check(stateCopy);
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool pub) : Statement(l), path(p), isPublic(pub) {
}

void ImportStatement::check(State& s) {
}

void ImportStatement::addToState(State& s) {
  if ((!isPublic && !s.getImports().empty()) || contains(s.getAllImports(), path))
    return;
  codeLoc.check(!contains(s.getImports(), path), "Public import cycle: " + combine(s.getImports(), ", "));
  s.pushImport(path);
  string content = readFromFile(path.c_str(), codeLoc);
  INFO << "Imported file " << path;
  auto tokens = lex(content, path);
  ast = unique<AST>(parse(tokens));
  for (auto& elem : ast->elems)
    elem->addToState(s);
  for (auto& elem : ast->elems)
    elem->check(s);
  s.popImport();
}

nullable<SType> Expression::getDotOperatorType(const State& idContext, const State& callContext) {
  return nullptr;
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

void EnumDefinition::addToState(State& s) {
  codeLoc.check(!elements.empty(), "Enum requires at least one element");
  unordered_set<string> occurences;
  for (auto& e : elements)
    codeLoc.check(!occurences.count(e), "Duplicate enum element: " + quote(e));
  s.addType(name, make_shared<Type>(EnumType(name, elements)));
}

void EnumDefinition::check(State& s) {
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

SType EnumConstant::getType(const State& state) {
  if (auto type = state.getTypeFromString(IdentifierInfo(enumName)))
    return type.get();
  else
    codeLoc.error("Unrecognized type: " + quote(enumName));
}
