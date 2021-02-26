#include <typeinfo>

#include "codegen.h"
#include "ast.h"
#include "type_registry.h"

struct Accu {
  public:
  Accu(bool l) : includeLineNumbers(l) {}

  void add(const string& s) {
    buf += s;
  }

  const int indentSize = 2;

  void newLine(const string& s = "") {
    add("\n" + string(indent * indentSize, ' ') + s);
  }

  void newLine(CodeLoc codeLoc) {
    add("\n"  + string(indent * indentSize, ' '));
    if (includeLineNumbers)
      add("#line " + to_string(codeLoc.line) + " \"" + codeLoc.file + "\"\n");
  }

  void pop_back() {
    buf.pop_back();
  }

  string generate() {
    return std::move(buf);
  }

  int indent = 0;
  string buf;
  bool includeLineNumbers;
  unordered_set<const FunctionInfo*> instances;
  unordered_set<const ConceptType*> conceptTypes;
  set<pair<AST*, CodegenStage>> generated;
};

void Constant::codegen(Accu& accu, CodegenStage) const {
  if (structMemberName)
    accu.add(*structMemberName);
  else
    accu.add(value->getCodegenName());
}

auto constexpr lambdaArgName = "lambda_Arg";

void Variable::codegen(Accu& accu, CodegenStage) const {
  accu.add(*identifier.asBasicIdentifier());
}

static string getDestructorName(const string& id) {
  return "destruct_" + id;
}

void MoveExpression::codegen(Accu& accu, CodegenStage) const {
  if (hasDestructor)
    accu.add("moveAndSetMoved(" + identifier +", &" + getDestructorName(identifier) + ".wasMoved)");
  else
    accu.add("std::move(" + identifier + ")");
}

void BinaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  if (functionInfo && !functionInfo->type.builtinOperator)
    if (auto opName = getCodegenName(op)) {
      accu.add(opName + *functionInfo->getMangledSuffix() + "("s);
      auto handleExpr = [&](int index) {
        if (destructorCall[index])
          accu.add("*get_temporary_holder(");
        expr[index]->codegen(accu, stage);
        if (destructorCall[index])
          accu.add(", &::" + destructorCall[index]->getMangledName() + ")");
      };
      handleExpr(0);
      accu.add(", ");
      handleExpr(1);
      accu.add(")");
      return;
    }
  accu.add("(");
  expr[0]->codegen(accu, stage);
  accu.add(") ");
  if (op == Operator::SUBSCRIPT)
    accu.add("[");
  else
    accu.add(getString(op) + " "s);
  accu.add("(");
  expr[1]->codegen(accu, stage);
  accu.add(")");
  if (op == Operator::SUBSCRIPT)
    accu.add("]");
}

void StatementBlock::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("{");
  ++accu.indent;
  for (auto& s : elems) {
    accu.newLine(s->codeLoc);
    s->codegen(accu, stage);
  }
  --accu.indent;
  accu.newLine("}");
}

static void codegenDestructorCall(Accu& accu, CodegenStage stage, const Statement& destructor, const string& variable) {
  accu.add("auto " + getDestructorName(variable) + " = deferDestruct([&]{ ");
  destructor.codegen(accu, stage);
  accu.add(";});");
}

void VariableDeclaration::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add(realType.get()->getCodegenName() + " ");
/*  if (!isMutable)
    accu.add("const ");*/
  accu.add(identifier);
  if (initExpr) {
    accu.add(" = ");
    initExpr->codegen(accu, stage);
  }
  accu.add(";");
  if (destructorCall)
    codegenDestructorCall(accu, stage, *destructorCall, identifier);
}

void IfStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  if (declaration) {
    accu.newLine("{");
    declaration->codegen(accu, stage);
  }
  accu.newLine("if (");
  condition->codegen(accu, stage);
  accu.add(")");
  ++accu.indent;
  accu.newLine("{");
  ifTrue->codegen(accu, stage);
  accu.add("}");
  --accu.indent;
  if (ifFalse) {
    accu.newLine("else {");
    ++accu.indent;
    accu.newLine();
    ifFalse->codegen(accu, stage);
    accu.add("}");
    --accu.indent;
  }
  if (declaration)
    accu.newLine("}");
}

void ReturnStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("return ");
  expr->codegen(accu, stage);
  accu.add(";");
}

static string getFunctionCallName(const FunctionInfo& functionInfo, bool methodCall) {
  if (functionInfo.type.generatedConstructor && functionInfo.type.parentType)
    return functionInfo.type.parentType->getCodegenName();
  string typePrefix;
  auto functionTemplateParams = functionInfo.type.templateParams;
  if (functionInfo.type.parentType)
    typePrefix = functionInfo.type.parentType->getCodegenName() + "::";
  else if (!methodCall)
    typePrefix += "::";
  return typePrefix + functionInfo.getMangledName();
}

void FunctionCall::codegen(Accu& accu, CodegenStage) const {
  string suffix;
  string id = getFunctionCallName(*functionInfo, !!callType);
  bool voidConversion =
      functionInfo->type.builtinOperator && functionInfo->type.retVal == BuiltinType::VOID;
  if (voidConversion)
    accu.add("({");
  if (functionInfo->type.generatedConstructor) {
    accu.add(id + "{");
    suffix = "}";
  } else {
    accu.add(id + "(");
    suffix = ")";
  }
  bool extractPointer = callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER;
  for (auto& arg : arguments) {
    if (extractPointer) {
      accu.add("op_get_address(");
      if (destructorCall)
        accu.add("*get_temporary_holder(");
    }
    arg->codegen(accu, CodegenStage::define());
    if (extractPointer && destructorCall)
      accu.add(", &::" + destructorCall->getMangledName() + ")");
    if (extractPointer) {
      accu.add(")");
      extractPointer = false;
    }
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(suffix);
  if (voidConversion)
    accu.add("; void_value; })");
}

static string getFunctionSignatureName(const FunctionInfo& function) {
  string typePrefix;
  if (function.type.parentType)
    typePrefix = function.type.parentType->getName() + "::";
  return function.getMangledName();
}

static string getSignature(const FunctionInfo& functionInfo, const FunctionDefinition* definition) {
  string retVal;
  //if (!name.contains<ConstructorId>())
    retVal = functionInfo.type.retVal->getCodegenName() + " ";
  string ret = retVal + getFunctionSignatureName(functionInfo) + "(";
  for (int i = 0; i < functionInfo.type.params.size(); ++i) {
    auto& param = functionInfo.type.params[i];
    auto paramName = functionInfo.getParamName(i, definition);
    string argText = param->getCodegenName() + " " + paramName.value_or("") + ", ";
    if (functionInfo.id.contains<Operator>()) {
      if (auto p = param.dynamicCast<ReferenceType>()) {
        auto name = paramName ? *paramName + "_ptr" : "";
        argText = p->underlying->getCodegenName() + " const& " + name + ", ";
      }
      if (auto p = param.dynamicCast<MutableReferenceType>()) {
        auto name = paramName ? *paramName + "_ptr" : "";
        argText = p->underlying->getCodegenName() + "& " + name + ", ";
      }
    }
    ret.append(argText);
  }
  if (!functionInfo.type.params.empty()) {
    ret.pop_back();
    ret.pop_back();
  }
  ret.append(")");
  return ret;
}

void FunctionDefinition::handlePointerParamsInOperator(Accu& accu, const StatementBlock* thisBody) const {
  vector<string> ptrInits;
  for (int i = 0; i < functionInfo->type.params.size(); ++i) {
    auto& param = functionInfo->type.params[i];
    auto name = functionInfo->getParamName(i, this);
    if (name && functionInfo->id.contains<Operator>())
      if (param.dynamicCast<ReferenceType>() || param.dynamicCast<MutableReferenceType>())
        ptrInits.push_back("auto " + *name + " = &" + *name + "_ptr;");
  }
  if (!ptrInits.empty()) {
    accu.newLine("{");
    ++accu.indent;
    for (auto& elem : ptrInits)
      accu.newLine(elem);
    accu.newLine();
    thisBody->codegen(accu, CodegenStage::define());
    --accu.indent;
    accu.newLine("}");
  } else
    thisBody->codegen(accu, CodegenStage::define());
}

void FunctionDefinition::addStacktraceGenerator(Accu& accu, const StatementBlock* thisBody) const {
  accu.add("F_BEGIN");
  handlePointerReturnInOperator(accu, thisBody);
  accu.add("F_END(\"" + functionInfo->prettyString() + "\")");
}

void FunctionDefinition::handlePointerReturnInOperator(Accu& accu, const StatementBlock* thisBody) const {
  if (functionInfo->type.retVal.dynamicCast<ReferenceType>() ||
      functionInfo->type.retVal.dynamicCast<MutableReferenceType>()) {
    accu.newLine("{");
    ++accu.indent;
    accu.newLine("auto getRef = [&]");
    handlePointerParamsInOperator(accu, thisBody);
    accu.add(";");
    accu.newLine("return *getRef();");
    --accu.indent;
    accu.newLine("}");
  } else
    handlePointerParamsInOperator(accu, thisBody);
}

void FunctionDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (external || stage.isTypes)
    return;
  auto addInstance = [&](const FunctionInfo& functionInfo, StatementBlock* body,
      const vector<unique_ptr<Statement>>& destructors) {
    if (functionInfo.getMangledSuffix() && (wasUsed || functionInfo.isMainFunction() || (exported && !stage.isImport))) {
      if (!functionInfo.type.templateParams.empty())
        accu.add("inline ");
      accu.add(getSignature(functionInfo, this));
      if (body && stage.isDefine && (!stage.isImport || !templateInfo.params.empty())) {
        accu.add("{");
        CHECK(destructors.size() == functionInfo.type.params.size())
            << functionInfo.prettyString();
        for (int i = 0; i < functionInfo.type.params.size(); ++i)
          if (destructors[i]) {
            codegenDestructorCall(accu, stage, *destructors[i], *parameters[i].name);
            accu.newLine();
          }
        addStacktraceGenerator(accu, body);
        accu.newLine("}");
      } else {
        accu.add(";");
        accu.newLine("");
      }
      accu.newLine("");
      accu.newLine("");
    }
  };
  addInstance(*functionInfo, body.get(), destructorCalls);
  for (auto& instance : instances)
    if (accu.instances.count(instance.functionInfo->getWithoutRequirements().get())
        && instance.functionInfo->getMangledSuffix())
      addInstance(*instance.functionInfo, instance.body.get(), instance.destructorCalls);
}

constexpr const char* unionEnumeratorPrefix = "Enum_";
constexpr const char* unionEntryPrefix = "Union_";
constexpr const char* unionDiscriminatorName = "unionElem";

static void codegenUnion(set<const Type*>& visited, Accu& accu, const StructType* type) {
  for (auto& elem : type->alternatives)
    elem.type->codegenDefinition(visited, accu);
  auto name = *type->getMangledName();
  accu.add("struct " + name + " {");
  ++accu.indent;
  accu.newLine();
  accu.add("enum {");
  vector<string> typeNames;
  for (auto& subtype : type->alternatives)
    typeNames.push_back(subtype.name);
  accu.add(combine(transform(typeNames, [](const string& e){ return unionEnumeratorPrefix + e;}), ", ") + "} "
      + unionDiscriminatorName + ";");
  for (auto& alternative : type->alternatives) {
    string signature = alternative.name + "(";
    if (alternative.type != BuiltinType::VOID)
      signature += alternative.type->getCodegenName() + " elem";
    signature += ")";
    accu.newLine("static " + name + " " + signature + ";");
  }
  accu.newLine("union {");
  ++accu.indent;
  accu.newLine("bool dummy;");
  for (auto& alternative : type->alternatives)
    accu.newLine(alternative.type->getCodegenName() + " " + unionEntryPrefix + alternative.name + ";");
  --accu.indent;
  accu.newLine("};");
  auto visitBody = [&] {
    ++accu.indent;
    accu.newLine("switch (unionElem) {");
    ++accu.indent;
    for (auto& alternative : type->alternatives) {
      accu.newLine("case "s + unionEnumeratorPrefix + alternative.name + ":");
      ++accu.indent;
      accu.newLine("return std::forward<Visitor>(v)("s + unionEntryPrefix + alternative.name + ");");
      --accu.indent;
    }
    --accu.indent;
    accu.newLine("}");
    --accu.indent;
  };
  accu.newLine("template <typename Visitor>");
  accu.newLine("auto visit(Visitor&& v) const {");
  visitBody();
  accu.newLine("}");
  accu.newLine("template <typename Visitor>");
  accu.newLine("auto visit(Visitor&& v) {");
  visitBody();
  accu.newLine("}");
  accu.newLine(name + "(" + name + "&& o) { UnionHelper<" + name + ">::move(std::move(o), *this);  }");
  accu.newLine(name + "& operator = (" + name + "&& o) {UnionHelper<" + name + ">::assign(std::move(o), *this); return *this; }");
  accu.newLine("~" + name + "() { UnionHelper<" + name + ">::destroy(*this); }");
  accu.newLine("private:" + name + "() {}");
  --accu.indent;
  accu.newLine("};");
  accu.newLine();
}


void Type::codegenDefinitionImpl(set<const Type*>&, Accu&) const {
}

void Type::codegenDefinition(set<const Type*>& visited, Accu& accu) const {
  if (!visited.count(this)) {
    visited.insert(this);
    codegenDefinitionImpl(visited, accu);
  }
}

void ArrayType::codegenDefinitionImpl(set<const Type*>& visited, Accu& accu) const {
  underlying->codegenDefinition(visited, accu);
}

void EnumType::codegenDefinitionImpl(set<const Type*>&, Accu& accu) const {
  if (!external) {
    accu.add("enum class " + name + " {");
    ++accu.indent;
    for (auto& elem : elements)
      accu.newLine(elem + ",");
    --accu.indent;
    accu.newLine("};");
    accu.newLine("template<> struct EnumInfo<" + name + "> {");
    accu.newLine("  static const char* getString(" + name + " elem) {");
    accu.newLine("    switch (elem) {");
    for (auto& elem : elements)
      accu.newLine("      case " + name + "::" + elem + ": return \"" + elem + "\";");
    accu.newLine("    }");
    accu.newLine("  }");
    accu.newLine("};");
  }
}

void StructType::codegenDefinitionImpl(set<const Type*>& visited, Accu& accu) const {
  if (external)
    return;
  for (auto& instance : instances)
    instance->codegenDefinition(visited, accu);
  if (auto name = getMangledName()) {
    if (!alternatives.empty()) {
      codegenUnion(visited, accu, this);
      return;
    }
    for (auto& elem : members)
      elem.type->codegenDefinition(visited, accu);
    accu.add("struct " + *name);
    /*if (incomplete) {
      accu.add(";");
      accu.newLine();
      return;
    } else*/
      accu.add(" {");
    ++accu.indent;
    for (auto& member : members)
      accu.newLine(member.type->getCodegenName() + " " + member.name + ";");
    --accu.indent;
    accu.newLine("};");
    accu.newLine();
  }
}

static unique_ptr<StatementBlock> generateLambdaBody(Statement* body, const LambdaType& type) {
  auto ret = unique<StatementBlock>(body->codeLoc);
  for (auto& capture : type.captures) {
    auto memberExpr = cast<Expression>(MemberAccessExpression::getPointerAccess(body->codeLoc,
        unique<Variable>(IdentifierInfo(lambdaArgName, body->codeLoc)), capture.name));
    auto type = capture.type;
    if (capture.captureType == LambdaCaptureType::REFERENCE) {
      type = convertPointerToReference(type->removeReference());
      memberExpr = unique<UnaryExpression>(body->codeLoc, Operator::POINTER_DEREFERENCE, std::move(memberExpr));
    } else
      type = ReferenceType::get(type);
    auto decl = unique<VariableDeclaration>(body->codeLoc, none, capture.name, std::move(memberExpr));
    decl->realType = type;
    ret->elems.push_back(std::move(decl));
  }
  ret->elems.push_back(unique<ExternalStatement>(body));
  return ret;
}

using LambdaSet = unordered_set<const LambdaType*>;

void codegenLambdas(const LambdasSet& lambdas, Accu& accu, const AST& ast, CodegenStage stage,
    set<const Type*>& visited) {
  vector<unique_ptr<FunctionDefinition>> defs;
  for (const auto& lambda : lambdas)
    if (lambda->functionInfo->getMangledSuffix()) {
      if (stage.isTypes)
        lambda->codegenDefinition(visited, accu);
      const auto dummyIdent = IdentifierInfo("ignore", lambda->body->codeLoc);
      auto getLambdaBody = [&lambda, &dummyIdent](Statement* body) {
        auto def = unique<FunctionDefinition>(body->codeLoc, dummyIdent,
            lambda->functionInfo->id);
        def->body = generateLambdaBody(body, *lambda);
        def->parameters.push_back(FunctionParameter{def->body->codeLoc, dummyIdent, string(lambdaArgName), false, false});
        def->wasUsed = true;
        return def;
      };
      auto mainBody = getLambdaBody(lambda->body.get());
      mainBody->destructorCalls.emplace_back();
      const auto& functionType = lambda->functionInfo->type;
      for (int i = 1; i < functionType.params.size(); ++i) {
        mainBody->parameters.push_back(FunctionParameter{mainBody->body->codeLoc, dummyIdent,
            lambda->parameterNames[i - 1], false, false});
        if (auto call = lambda->destructorCalls[i - 1].get())
          mainBody->destructorCalls.push_back(unique<ExternalStatement>(call));
        else
          mainBody->destructorCalls.push_back(nullptr);
      }
      mainBody->functionInfo = FunctionInfo::getDefined(lambda->functionInfo->id, std::move(functionType),
          mainBody.get());
      defs.push_back(std::move(mainBody));
      if (lambda->destructor) {
        auto destructorBody = getLambdaBody(lambda->destructor.get());
        destructorBody->destructorCalls.emplace_back();
        destructorBody->functionInfo = FunctionInfo::getImplicit("destruct"s,
            FunctionType(BuiltinType::VOID, {PointerType::get(lambda->get_this().get())}, {}));
        destructorBody->body->elems.push_back(unique<ReturnStatement>(destructorBody->codeLoc));
        defs.push_back(std::move(destructorBody));
      }
    }
  for (auto& elem : defs) {
    elem->codegen(accu, stage);
  }
}

string codegen(const AST& ast, TypeRegistry& registry, const Context& context, const string& codegenInclude,
    bool includeLineNumbers) {
  Accu accu(includeLineNumbers);
  LambdaSet lambdas;
  FunctionCallVisitFun visitFun = [&](SFunctionInfo call) {
    call = call->getWithoutRequirements();
    if (!accu.instances.count(call.get())) {
      accu.instances.insert(call.get());
      if (!call->type.concept)
        if (auto def = call->getDefinition())
          if (!def->external && def->functionInfo != call) {
            for (auto& instance : def->instances)
              if (instance.functionInfo->getWithoutRequirements() == call) {
                instance.body->addFunctionCalls(visitFun);
                instance.body->addLambdas(lambdas);
                instance.body->addConceptTypes(accu.conceptTypes);
                for (auto& call : instance.destructorCalls)
                  if (call) {
                    call->addFunctionCalls(visitFun);
                    call->addLambdas(lambdas);
                    call->addConceptTypes(accu.conceptTypes);
                  }
                return;
              }
            FATAL;
          }
    }
  };
  for (auto& elem : ast.elems) {
    elem->addFunctionCalls(visitFun);
    elem->addLambdas(lambdas);
    elem->addConceptTypes(accu.conceptTypes);
  }
  accu.add("#include \"" + codegenInclude + "/all.h\"");
  accu.newLine();
  for (auto& l : lambdas)
    accu.newLine("struct " + l->getCodegenName() + ";");
  for (auto& t : registry.getAllStructs())
    if (!t->external) {
      if (auto name = t->getMangledName())
        accu.newLine("struct " + *name + ";");
      for (auto s : t->instances)
        if (auto name = s->getMangledName())
          accu.newLine("struct " + *name + ";");
    }
  for (auto& t : registry.getAllEnums())
    if (!t->external)
      accu.add("enum class " + *t->getMangledName() + ";");
  for (auto& elem : ast.elems) {
    elem->codegen(accu, CodegenStage::types());
  }
  set<const Type*> visitedTypes;
  for (auto& type : context.getAllTypes()) {
    if (context.isFullyDefined(type.get()))
      type->codegenDefinition(visitedTypes, accu);
  }
  codegenLambdas(lambdas, accu, ast, CodegenStage::types(), visitedTypes);
  for (auto& elem : ast.elems) {
    elem->codegen(accu, CodegenStage::declare());
  }
  codegenLambdas(lambdas, accu, ast, CodegenStage::declare(), visitedTypes);
  for (auto& elem : ast.elems) {
    elem->codegen(accu, CodegenStage::define());
  }
  codegenLambdas(lambdas, accu, ast, CodegenStage::define(), visitedTypes);
  for (auto& elem : ast.elems) {
    if (auto fun = dynamic_cast<const FunctionDefinition*>(elem.get()))
      if (fun->name == "main"s) {
        if (fun->parameters.empty())
          accu.add("#include \"" + codegenInclude + "/main_body.h\"");
        else
          accu.add("#include \"" + codegenInclude + "/main_body_args.h\"");
      }
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  expr->codegen(accu, stage);
  accu.add(";");
}

void UnionDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (stage.isDefine)
    for (auto& instance : concat({type.get()}, type->instances))
      if (auto name1 = instance->getMangledName())
        for (auto& alternative : instance->alternatives) {
          auto& name = *name1;
          string signature = alternative.name + "(";
          if (alternative.type != BuiltinType::VOID)
            signature += alternative.type->getCodegenName() + " elem";
          signature += ")";
          accu.add("inline " + name + " " + name + "::" + signature + " {");
          ++accu.indent;
          accu.newLine(name + " ret;");
          accu.newLine("ret."s + unionDiscriminatorName + " = " + unionEnumeratorPrefix + alternative.name + ";");
          if (!(alternative.type == BuiltinType::VOID))
            accu.newLine("new (&ret."s + unionEntryPrefix + alternative.name + ") " +
                alternative.type->getCodegenName() + "(std::move(elem));");
          accu.newLine("return ret;");
          --accu.indent;
          accu.newLine("}");
          accu.newLine("");
        }
}

void SwitchStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  switch (type) {
    case UNION:
      codegenUnion(accu);
      break;
    case ENUM:
      codegenEnum(accu);
      break;
  }
}

void SwitchStatement::codegenEnum(Accu& accu) const {
  accu.add("switch (");
  expr->codegen(accu, CodegenStage::define());
  accu.add(") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    for (auto& id : caseElem.ids)
      accu.newLine("case " + *targetType->getMangledName() + "::" + id + ":");
    accu.newLine("{");
    ++accu.indent;
    accu.newLine();
    caseElem.block->codegen(accu, CodegenStage::define());
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu, CodegenStage::define());
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}");
}

constexpr const char* unionTmpRef = "unionTmpRef";

void SwitchStatement::codegenUnion(Accu& accu) const {
  accu.add("{ auto&& "s + unionTmpRef + " = ");
  expr->codegen(accu, CodegenStage::define());
  accu.add(";");
  if (destructorCall)
    accu.add("auto " + getDestructorName(unionTmpRef) + " = deferDestruct([&]{ "
        + destructorCall->getMangledName() + "(&" + unionTmpRef + ");});");
  accu.newLine("switch ("s + unionTmpRef + "."s + unionDiscriminatorName + ") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    auto caseId = *getOnlyElement(caseElem.ids);
    accu.newLine("case "s + *targetType->getMangledName() + "::" + unionEnumeratorPrefix + caseId + ": {");
    ++accu.indent;
    if (!!caseElem.declaredVar)
      accu.newLine("auto&& "s + caseId + " = " + unionTmpRef + "." + unionEntryPrefix + caseId + ";");
    if (destructorCall)
      accu.newLine("auto& "s + getDestructorName(caseId) + " = " + getDestructorName(unionTmpRef) + ";");
    accu.newLine();
    caseElem.block->codegen(accu, CodegenStage::define());
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu, CodegenStage::define());
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}");
  accu.newLine("}");
}

void UnaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  if (functionInfo && !functionInfo->type.builtinOperator)
    if (auto opName = getCodegenName(op)) {
      accu.add(opName + *functionInfo->getMangledSuffix() + "("s);
      if (destructorCall)
        accu.add("*get_temporary_holder(");
      expr->codegen(accu, stage);
      if (destructorCall)
        accu.add(", &::" + destructorCall->getMangledName() + ")");
      accu.add(")");
      return;
    }
  accu.add(getString(op));
  accu.add("(");
  expr->codegen(accu, CodegenStage::define());
  accu.add(") ");
}

void EmbedStatement::codegen(Accu& accu, CodegenStage stage) const {
  if ((stage.isDefine && !isTopLevel) ||
      (!stage.isImport && stage.isTypes && isTopLevel) ||
      (stage.isImport && stage.isTypes && exported)) {
    if (!isTopLevel)
      accu.newLine("{");
    for (auto& r : replacements)
      accu.newLine((r.constant ? "constexpr auto " : "using ") + r.from + " = " + r.to + ";");
    accu.newLine(value);
    if (!isTopLevel)
      accu.newLine("}");
  }
  accu.newLine();
}

static string getLoopBreakLabel(int loopId) {
  return "break_loop_" + to_string(loopId);
}

void ForLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("{");
  init->codegen(accu, CodegenStage::define());
  accu.add("for (;");
  cond->codegen(accu, CodegenStage::define());
  accu.add(";");
  iter->codegen(accu, CodegenStage::define());
  accu.add(")");
  ++accu.indent;
  accu.newLine("{");
  body->codegen(accu, CodegenStage::define());
  accu.add("}}");
  --accu.indent;
  accu.newLine(getLoopBreakLabel(loopId) + ":;");
  accu.newLine();
}

void StaticForLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  for (auto& elem : unrolled) {
    accu.add("{");
    elem->codegen(accu, stage);
    accu.add("}");
  }
}

void RangedLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("{");
  ++accu.indent;
  accu.newLine("auto&& "s + *containerName + " = ");
  container->codegen(accu, stage);
  accu.add(";");
  accu.newLine();
  containerEnd->codegen(accu, stage);
  accu.newLine("{");
  init->codegen(accu, stage);
  accu.newLine("for (;");
  condition->codegen(accu, stage);
  accu.add(";");
  increment->codegen(accu, stage);
  accu.add(")");
  ++accu.indent;
  accu.newLine("{");
  body->codegen(accu, CodegenStage::define());
  accu.add("}");
  --accu.indent;
  --accu.indent;
  accu.newLine("}}");
  accu.newLine(getLoopBreakLabel(loopId) + ":;");
  accu.newLine();
}

void WhileLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("while (");
  cond->codegen(accu, CodegenStage::define());
  accu.add(")");
  ++accu.indent;
  accu.newLine("{");
  body->codegen(accu, CodegenStage::define());
  accu.add("}");
  --accu.indent;
  accu.newLine(getLoopBreakLabel(loopId) + ":;");
  accu.newLine();
}

void ImportStatement::codegen(Accu& accu, CodegenStage stage) const {
  if (ast) {
    stage.setImport();
    if (!accu.generated.count(make_pair(ast, stage))) {
      accu.generated.insert(make_pair(ast, stage));
      for (auto& elem : ast->elems)
        if (elem->exported)
          elem->codegen(accu, stage);
    }
  }
}

void EnumConstant::codegen(Accu& accu, CodegenStage) const {
  accu.add(enumType->getCodegenName() + "::" + enumElement);
}

static string getVTableFunName(const FunctionId id) {
  return id.visit(
      [](const string& s) {
        return s;
      },
      [](Operator op) {
        return getCodegenName(op);
      },
      [](auto&) -> string {
        fail();
      }
  );
}

static string getVTableName(const string& concept) {
  return concept + "_vtable";
}

void ConceptDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (!fatPointers.empty()) {
    auto addParams = [&] (const SFunctionInfo& fun) {
      for (auto& param : fun->type.params) {
        if (param->getMangledName())
          accu.add(param->getCodegenName());
        else if (param.dynamicCast<PointerType>())
          accu.add("void const*");
        else
          accu.add("void*");
        accu.add(",");
      }
      accu.pop_back();
    };
    for (auto& conceptType : conceptInstances)
      if (auto mangledName = conceptType->getMangledName())
        if (accu.conceptTypes.count(conceptType.get())) {
          const auto vTableName = getVTableName(*mangledName);
          auto functions = conceptType->getConceptFor(conceptType->concept->getParams()[0])->getContext().getAllFunctions();
          if (stage.isTypes)
            accu.newLine("struct " + vTableName + ";");
          else if (stage.isDefine) {
            accu.newLine("struct " + vTableName + " {");
            ++accu.indent;
            for (auto& fun : functions) {
              accu.newLine(fun->type.retVal->getCodegenName() + " (*" + getVTableFunName(fun->id) + ")(");
              addParams(fun);
              accu.add(");");
            }
            --accu.indent;
            accu.newLine("};");
            accu.newLine();
          }
          ErrorBuffer errors;
          for (auto fun : functions) {
            fun = replaceInFunction(conceptType->concept->getContext(), fun, conceptType->concept->getParams()[0], conceptType, errors);
            accu.newLine("inline " + fun->type.retVal->getCodegenName() + " " + fun->getMangledName() + "(");
            auto getArgName = [](int i) { return "_arg" + to_string(i); };
            string virtualArg;
            for (int i = 0; i < fun->type.params.size(); ++i) {
              auto& param = fun->type.params[i];
              accu.add((i > 0 ? ", " : "") + param->getCodegenName() + " " + getArgName(i));
              if (i > 0)
                virtualArg += ", ";
              if (param->removePointer() == conceptType)
                virtualArg = "return " + getArgName(i) + ".vTable->" + getVTableFunName(fun->id) + "(" + virtualArg + getArgName(i) + ".object";
              else
                virtualArg += getArgName(i);
            }
            CHECK(!virtualArg.empty());
            if (stage.isDefine) {
              accu.add(") {");
              ++accu.indent;
              accu.newLine(virtualArg + ");");
              --accu.indent;
              accu.newLine("}");
            } else
              accu.add(");");
            accu.newLine();
          }
          CHECK(errors.empty());
          if (stage.isDefine)
            for (auto& elem : fatPointers)
              if (elem.type->getMangledName()) {
                accu.newLine("static " + vTableName + " " + vTableName + "_" + *elem.type->getMangledName() + "{");
                ++accu.indent;
                for (int i = 0; i < functions.size(); ++i) {
                  accu.newLine("reinterpret_cast<" + functions[i]->type.retVal->getCodegenName() + " (*)(");
                  addParams(functions[i]);
                  accu.add(")>(&" + elem.vTable[i]->getMangledName() +"),");
                }
                --accu.indent;
                accu.newLine("};");
                accu.newLine();
              }
        }
  }
}

void BreakStatement::codegen(Accu& accu, CodegenStage) const {
  accu.add("goto " + getLoopBreakLabel(loopId) + ";");
}

void ContinueStatement::codegen(Accu& accu, CodegenStage) const {
  accu.add("continue;");
}

void ArrayLiteral::codegen(Accu& accu, CodegenStage stage) const {
  accu.add("make_array<" + type->getCodegenName() + ">(");
  for (auto& elem : contents) {
    elem->codegen(accu, stage);
    accu.add(", ");
  }
  if (!contents.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(")");
}

void LambdaExpression::codegen(Accu& a, CodegenStage) const {
  a.add(type->getCodegenName() + "{");
  for (auto& capture : captureInfo.captures) {
    switch (capture.type) {
      case LambdaCaptureType::MOVE:
        if (capture.hasConstructor)
          a.add("({" + getDestructorName(capture.name) + ".wasMoved = true; std::move(" + capture.name + ");}),");
        else
          a.add("std::move(" + capture.name + "),");
        break;
      case LambdaCaptureType::COPY:
        a.add("::copy(&" + capture.name + "),");
        break;
      case LambdaCaptureType::IMPLICIT_COPY:
        a.add("::implicit_copy(&" + capture.name + "),");
        break;
      case LambdaCaptureType::REFERENCE:
        a.add("&" + capture.name + ",");
        break;
    }
  } if (!captureInfo.captures.empty())
    a.pop_back();
  a.add("}");
}

void LambdaType::codegenDefinitionImpl(set<const Type*>& visited, Accu& a) const {
  a.add("struct " + getCodegenName() + " {");
  for (auto& capture : captures)
    a.newLine(capture.type->getCodegenName() + " " + capture.name + ";");
  a.newLine("};\n");
}

void CountOfExpression::codegen(Accu&, CodegenStage) const {
  FATAL << "Attempting to codegen countof expression";
}

void VariablePackElement::codegen(Accu& accu, CodegenStage) const {
  accu.add(*codegenName);
}

static void codegenMember(Accu& accu, CodegenStage stage, const nullable<SFunctionInfo>& destructorCall,
    bool isMainDestructor, const string& identifier, const Expression& lhs, bool isUnion) {
  accu.add("(");
  if (destructorCall) {
    if (isMainDestructor)
      accu.add("(*get_temporary_holder(");
    else
      accu.add("{auto&& tmp = ");
  }
  lhs.codegen(accu, stage);
  if (destructorCall) {
    if (isMainDestructor)
      accu.add(", &::" + destructorCall->getMangledName() + "))." + identifier + ")");
    else
      accu.add(";" + destructorCall->getMangledName() + "(&tmp); std::move(tmp)." + identifier +";})");
  } else
    accu.add(")."s + (isUnion ? unionEntryPrefix : "") + identifier);
}

void MemberAccessExpression::codegen(Accu& accu, CodegenStage stage) const {
  codegenMember(accu, stage, destructorCall, isMainDestructor, identifier, *lhs, isUnion);
}


void MemberIndexExpression::codegen(Accu& accu, CodegenStage stage) const {
  codegenMember(accu, stage, destructorCall, isMainDestructor, *memberName, *lhs, isUnion);
}

void TernaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  accu.add("(");
  condExpr->codegen(accu, stage);
  accu.add(")?(");
  e1->codegen(accu, stage);
  accu.add("):(");
  e2->codegen(accu, stage);
  accu.add(")");
}

void FatPointerConversion::codegen(Accu& accu, CodegenStage stage) const {
  if (argType.dynamicCast<PointerType>())
    accu.add("make_const_fat_ptr(");
  else if (argType.dynamicCast<MutablePointerType>())
    accu.add("make_fat_ptr(");
  else
    accu.add("make_fat_value(");
  arg->codegen(accu, stage);
  accu.add(", &" + getVTableName(*conceptType->getMangledName()) + "_" + *argType->removePointer()->getMangledName() + ")");
}

void UncheckedStatement::codegen(Accu& accu, CodegenStage s) const {
  elem->codegen(accu, s);
}

void AttributeDefinition::codegen(Accu&, CodegenStage) const {
}

void ExternalStatement::codegen(Accu& accu, CodegenStage stage) const {
  elem->codegen(accu, stage);
}

void StatementExpression::codegen(Accu& accu, CodegenStage stage) const {
  accu.add("({");
  for (auto& s : statements) {
    s->codegen(accu, stage);
    accu.add("; ");
  }
  value->codegen(accu, stage);
  accu.add(";})");
}

void MixinStatement::codegen(Accu& accu, CodegenStage stage) const {
  result->codegen(accu, stage);
}
