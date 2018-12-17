#include <typeinfo>

#include "codegen.h"
#include "ast.h"


struct Accu {
  public:
  Accu(bool l) : includeLineNumbers(l) {}

  enum Position {
    CURRENT,
    EMBED
  };

  void add(const string& s, Position pos = CURRENT) {
    buf[pos] += s;
  }

  void newLine(const string& s = "", Position pos = CURRENT) {
    const int indentSize = 2;
    buf[pos] += "\n" + string(indent * indentSize, ' ');
    buf[pos] += s;
  }

  void newLine(CodeLoc codeLoc) {
    buf[CURRENT] += "\n";
    if (includeLineNumbers)
      add("#line " + to_string(codeLoc.line) + " \"" + codeLoc.file + "\"\n");
  }

  void pop_back(Position pos = CURRENT) {
    buf[pos].pop_back();
  }

  string generate() {
    string ret;
    for (auto& elem : buf)
      ret += elem.second + "\n";
    return ret;
  }

  int indent = 0;
  map<Position, string> buf;
  bool includeLineNumbers;
};

void Constant::codegen(Accu& accu, CodegenStage) const {
  accu.add(value->getCodegenName());
}

void Variable::codegen(Accu& accu, CodegenStage) const {
  accu.add(*identifier.asBasicIdentifier());
}

void MoveExpression::codegen(Accu& accu, CodegenStage) const {
  accu.add("std::move(const_cast<" + type->getCodegenName() + "&>(" + identifier + "))");
}

void Expression::codegenDotOperator(Accu& accu, CodegenStage stage, Expression* leftSide) const {
  accu.add("(");
  leftSide->codegen(accu, stage);
  accu.add(").");
  codegen(accu, stage);
}

void BinaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  if (op == Operator::MEMBER_ACCESS) {
    expr[1]->codegenDotOperator(accu, stage, expr[0].get());
    return;
  }
  if (op == Operator::SUBSCRIPT && subscriptOpWorkaround) {
    accu.add("subscript_op(");
    expr[0]->codegen(accu, stage);
    accu.add(", ");
    expr[1]->codegen(accu, stage);
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
  if (op != Operator::MEMBER_ACCESS)
    accu.add("(");
  expr[1]->codegen(accu, stage);
  if (op != Operator::MEMBER_ACCESS)
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

void VariableDeclaration::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add(realType.get()->getCodegenName() + " ");
  if (!isMutable)
    accu.add("const ");
  accu.add(identifier);
  if (initExpr) {
    accu.add(" = ");
    initExpr->codegen(accu, stage);
  }
  accu.add(";");
}

void IfStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.newLine("if (");
  if (declaration) {
    declaration->codegen(accu, stage);
    if (!condition)
      accu.pop_back();
  }
  if (condition)
    condition->codegen(accu, stage);
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  ifTrue->codegen(accu, stage);
  --accu.indent;
  if (ifFalse) {
    accu.newLine("else");
    ++accu.indent;
    accu.newLine();
    ifFalse->codegen(accu, stage);
    --accu.indent;
  }
}

void ReturnStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("return");
  if (expr) {
    accu.add(" ");
    expr->codegen(accu, stage);
  }
  accu.add(";");
}

static string getFunctionCallName(const FunctionInfo& functionInfo, bool methodCall) {
  string typePrefix;
  auto functionTemplateParams = functionInfo.type.templateParams;
  if (functionInfo.type.parentType) {
    typePrefix = functionInfo.type.parentType->getCodegenName() + "::";
    if (functionInfo.type.externalMethod)
      if (auto structParent = functionInfo.type.retVal.dynamicCast<StructType>())
        functionTemplateParams = getSubsequence(functionTemplateParams, (int) structParent->templateParams.size());
  }
  else if (!methodCall)
    typePrefix += "::";
  return functionInfo.id.visit(
      [&](const string& s) { return typePrefix + s + joinTemplateParamsCodegen(functionTemplateParams); },
      [&](Operator op) {
        if (op == Operator::SUBSCRIPT)
          return "subscript_op"s;
        else
          return "operator "s + getString(op);
      },
      [&](ConstructorTag) { return functionInfo.type.retVal->getCodegenName(); }
  );
}

static void genFunctionCall(Accu& accu, const FunctionInfo& functionInfo,
    vector<Expression*> arguments, optional<MethodCallType> callType) {
  string prefix;
  string suffix;
  string id = getFunctionCallName(functionInfo, !!callType);
  if (functionInfo.id.contains<ConstructorTag>()) {
    prefix = id + "{";
    suffix = "}";
  } else {
    prefix = id + "(";
    suffix = ")";
  }
  accu.add(prefix);
  bool extractPointer = callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER;
  for (auto& arg : arguments) {
    if (extractPointer)
      accu.add("getAddress(");
    arg->codegen(accu, CodegenStage::define());
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
}

void FunctionCall::codegenDotOperator(Accu& accu, CodegenStage stage, Expression* leftSide) const {
  if (callType == MethodCallType::FUNCTION_AS_METHOD || callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER) {
    vector<Expression*> args {leftSide};
    append(args, extractRefs(arguments));
    genFunctionCall(accu, *functionInfo, args, callType);
  } else {
    accu.add("(");
    leftSide->codegen(accu, stage);
    accu.add(").");
    codegen(accu, stage);
  }
}

void FunctionCall::codegen(Accu& accu, CodegenStage) const {
  genFunctionCall(accu, *functionInfo, extractRefs(arguments), callType);
}

void FunctionCallNamedArgs::codegenDotOperator(Accu& accu, CodegenStage stage, Expression* leftSide) const {
  if (callType == MethodCallType::FUNCTION_AS_METHOD || callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER) {
    vector<Expression*> args {leftSide};
    append(args, transform(arguments, [](const auto& arg) { return arg.expr.get(); }));
    genFunctionCall(accu, *functionInfo, args, callType);
  } else {
    accu.add("(");
    leftSide->codegen(accu, stage);
    accu.add(").");
    codegen(accu, stage);
  }
}

void FunctionCallNamedArgs::codegen(Accu& accu, CodegenStage stage) const {
  genFunctionCall(accu, *functionInfo, transform(arguments, [](const auto& arg) { return arg.expr.get(); }), callType);
}

static void considerTemplateParams(Accu& accu, const vector<TemplateParameter>& params) {
  if (!params.empty()) {
    accu.add("template <");
    for (auto& param : params)
      if (param.type)
        accu.add(*param.type + " " + param.name + ", ");
      else
        accu.add("typename " + param.name + ", ");
    accu.pop_back();
    accu.pop_back();
    accu.add(">");
    accu.newLine();
  }
}

static string getFunctionSignatureName(const FunctionInfo& function) {
  string typePrefix;
  if (function.type.parentType)
    typePrefix = function.type.parentType->getName() + "::";
  return function.id.visit(
      [&](const string& s) { return s; },
      [&](Operator op) {
        if (op == Operator::SUBSCRIPT)
          return "subscript_op"s;
        else
          return "operator "s + getString(op);
      },
      [&](ConstructorTag) { return function.type.retVal->getName(false); }
  );
}

void FunctionDefinition::addSignature(Accu& accu, string structName) const {
  considerTemplateParams(accu, templateInfo.params);
  if (!structName.empty())
    structName += "::";
  string retVal;
  if (!name.contains<ConstructorId>())
    retVal = functionInfo->type.retVal->getCodegenName() + " ";
  string ret = retVal + structName + getFunctionSignatureName(*functionInfo) + "(";
  for (auto& param : functionInfo->type.params) {
    string argText = param.type->getCodegenName() + " " + param.name.value_or("") + ", ";
    if (functionInfo->id.contains<Operator>()) {
      if (auto p = param.type.dynamicCast<ReferenceType>()) {
        auto name = param.name ? *param.name + "_ptr" : "";
        argText = "const " + p->underlying->getCodegenName() + "& " + name + ", ";
      }
      if (auto p = param.type.dynamicCast<MutableReferenceType>()) {
        auto name = param.name ? *param.name + "_ptr" : "";
        argText = p->underlying->getCodegenName() + "& " + name + ", ";
      }
    }
    ret.append(argText);
  }
  if (!functionInfo->type.params.empty()) {
    ret.pop_back();
    ret.pop_back();
  }
  ret.append(")");
  accu.add(ret);
};

static void addInitializers(Accu& accu, const vector<FunctionDefinition::Initializer>& initializers) {
  if (!initializers.empty())
    accu.add(" : ");
  bool addComma = false;
  for (auto& elem : initializers) {
    if (addComma)
      accu.add(", ");
    addComma = true;
    accu.add(elem.paramName + "(");
    elem.expr->codegen(accu, CodegenStage::define());
    accu.add(")");
  }
  accu.add(" ");
}

void FunctionDefinition::handlePointerParamsInOperator(Accu& accu) const {
  vector<string> ptrInits;
  for (auto& param : functionInfo->type.params)
    if (param.name && functionInfo->id.contains<Operator>())
      if (param.type.dynamicCast<ReferenceType>() || param.type.dynamicCast<MutableReferenceType>())
        ptrInits.push_back("auto " + *param.name + " = &" + *param.name + "_ptr;");
  if (!ptrInits.empty()) {
    accu.newLine("{");
    ++accu.indent;
    for (auto& elem : ptrInits)
      accu.newLine(elem);
    accu.newLine();
    body->codegen(accu, CodegenStage::define());
    --accu.indent;
    accu.newLine("}");
  } else
    body->codegen(accu, CodegenStage::define());
}

void FunctionDefinition::handlePointerReturnInOperator(Accu& accu) const {
  if (functionInfo->type.retVal.dynamicCast<ReferenceType>() ||
      functionInfo->type.retVal.dynamicCast<MutableReferenceType>()) {
    accu.newLine("{");
    ++accu.indent;
    accu.newLine("auto getRef = [&]");
    handlePointerParamsInOperator(accu);
    accu.add(";");
    accu.newLine("return *getRef();");
    --accu.indent;
    accu.newLine("}");
  } else
    handlePointerParamsInOperator(accu);
}

void FunctionDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (external)
    return;
  addSignature(accu, "");
  if (body && stage.isDefine) {
    accu.newLine("");
    handlePointerReturnInOperator(accu);
  } else {
    accu.add(";");
    accu.newLine("");
  }
}

string codegen(const AST& ast, const string& codegenInclude, bool includeLineNumbers) {
  Accu accu(includeLineNumbers);
  accu.add("#include \"" + codegenInclude + "\"");
  accu.newLine();
  for (auto& elem : ast.elems) {
    elem->codegen(accu, CodegenStage::declare());
    accu.newLine();
  }
  for (auto& elem : ast.elems) {
    elem->codegen(accu, CodegenStage::define());
    accu.newLine();
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  expr->codegen(accu, stage);
  accu.add(";");
}

void StructDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (external)
    return;
  if (!stage.isDefine) {
    considerTemplateParams(accu, templateInfo.params);
    accu.add("struct " + name);
    if (incomplete) {
      accu.add(";");
      accu.newLine();
      return;
    } else
      accu.add(" {");
    ++accu.indent;
    for (auto& method : methods) {
      accu.newLine();
      method->addSignature(accu, "");
      accu.add(";");
    }
    for (auto& member : type->members)
      accu.newLine(member.type->getCodegenName() + " " + member.name + ";");
    --accu.indent;
    accu.newLine("};");
  }
  accu.newLine();
  for (auto& method : methods)
    if (stage.isDefine && (!stage.isImport  || !templateInfo.params.empty())) {
      considerTemplateParams(accu, templateInfo.params);
      method->addSignature(accu, name + joinTemplateParams(type->templateParams));
      if (method->functionInfo->id.contains<ConstructorTag>())
        addInitializers(accu, method->initializers);
      accu.newLine();
      method->body->codegen(accu, CodegenStage::define());
      accu.newLine();
    }
}

constexpr const char* variantEnumeratorPrefix = "Enum_";
constexpr const char* variantUnionEntryPrefix = "Union_";
constexpr const char* variantUnionElem = "unionElem";

void VariantDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (!stage.isDefine) {
    considerTemplateParams(accu, templateInfo.params);
    accu.add("struct " + name + " {");
    ++accu.indent;
    accu.newLine();
    accu.add("enum {");
    vector<string> typeNames;
    for (auto& subtype : elements)
      typeNames.push_back(subtype.name);
    accu.add(combine(transform(typeNames, [](const string& e){ return variantEnumeratorPrefix + e;}), ", ") + "} "
        + variantUnionElem + ";");
    for (auto& alternative : type->alternatives) {
      string signature = alternative.name + "(";
      if (alternative.type != ArithmeticType::VOID)
        signature += "const " + alternative.type->getCodegenName() + "& elem";
      signature += ")";
      string params = joinTemplateParams(type->templateParams);
      accu.newLine("static " + name + params + " " + signature + ";");
    }
    accu.newLine("union {");
    ++accu.indent;
    accu.newLine("bool dummy;");
    for (auto& alternative : type->alternatives) {
      if (alternative.type != ArithmeticType::VOID)
        accu.newLine(alternative.type->getCodegenName() + " " + variantUnionEntryPrefix + alternative.name + ";");
    }
    --accu.indent;
    accu.newLine("};");
    auto visitBody = [&] {
      ++accu.indent;
      accu.newLine("switch (unionElem) {");
      ++accu.indent;
      for (auto& alternative : type->alternatives) {
        if (alternative.type != ArithmeticType::VOID) {
          accu.newLine("case "s + variantEnumeratorPrefix + alternative.name + ":");
          ++accu.indent;
          accu.newLine("return std::forward<Visitor>(v)("s + variantUnionEntryPrefix + alternative.name + ");");
          --accu.indent;
        } else {
          accu.newLine("case "s + variantEnumeratorPrefix + alternative.name + ":");
          ++accu.indent;
          accu.newLine("return;");
          --accu.indent;
        }
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
    accu.newLine(name + "(const " + name + "& o) { VariantHelper<" + name + ">::copy(o, *this);  }");
    accu.newLine("~" + name + "() { VariantHelper<" + name + ">::destroy(*this); }");
    accu.newLine("private:" + name + "() {}");
    --accu.indent;
    accu.newLine("};");
    accu.newLine();
  }
  if (stage.isDefine && (!stage.isImport || !templateInfo.params.empty()))
    for (auto& alternative : type->alternatives) {
      string signature = alternative.name + "(";
      if (alternative.type != ArithmeticType::VOID)
        signature += "const " + alternative.type->getCodegenName() + "& elem";
      signature += ")";
      string params = joinTemplateParams(type->templateParams);
      considerTemplateParams(accu, templateInfo.params);
      accu.add(name + params + " " + name + params + "::" + signature + " {");
      ++accu.indent;
      accu.newLine(name + " ret;");
      accu.newLine("ret."s + variantUnionElem + " = " + variantEnumeratorPrefix + alternative.name + ";");
      if (!(alternative.type == ArithmeticType::VOID))
        accu.newLine("new (&ret."s + variantUnionEntryPrefix + alternative.name + ") " +
            alternative.type->getCodegenName() + "(elem);");
      accu.newLine("return ret;");
      --accu.indent;
      accu.newLine("}");
      accu.newLine("");
    }
}

constexpr const char* variantTmpRef = "variantTmpRef";

void SwitchStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  switch (type) {
    case VARIANT:
      codegenVariant(accu);
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
    accu.newLine("case " + subtypesPrefix + caseElem.id + ": {");
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

void SwitchStatement::codegenVariant(Accu& accu) const {
  accu.add("{ auto&& "s + variantTmpRef + " = ");
  expr->codegen(accu, CodegenStage::define());
  accu.add(";");
  accu.newLine("switch ("s + variantTmpRef + "."s + variantUnionElem + ") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    accu.newLine("case "s + subtypesPrefix + variantEnumeratorPrefix + caseElem.id + ": {");
    ++accu.indent;
    if (caseElem.varType == caseElem.VALUE)
      accu.newLine("auto&& "s + caseElem.id + " = " + variantTmpRef + "." + variantUnionEntryPrefix + caseElem.id + ";");
    else if (caseElem.varType == caseElem.POINTER)
      accu.newLine("auto "s + caseElem.id + " = &" + variantTmpRef + "." + variantUnionEntryPrefix + caseElem.id + ";");
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
  accu.newLine("}}");
}

void UnaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add(getString(op));
  accu.add("(");
  expr->codegen(accu, CodegenStage::define());
  accu.add(") ");
}

void EmbedStatement::codegen(Accu& accu, CodegenStage stage) const {
  if ((stage.isDefine && !isTopLevel) ||
      (!stage.isImport && !stage.isDefine && isTopLevel) ||
      (stage.isImport&& !stage.isDefine && isPublic))
    accu.newLine(value);
}

bool EmbedStatement::hasReturnStatement(const Context&) const {
  return true;
}

void ForLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("for (");
  init->codegen(accu, CodegenStage::define());
  cond->codegen(accu, CodegenStage::define());
  accu.add(";");
  iter->codegen(accu, CodegenStage::define());
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  body->codegen(accu, CodegenStage::define());
  --accu.indent;
  accu.newLine();
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
  accu.newLine();
  accu.newLine("for (");
  init->codegen(accu, stage);
  condition->codegen(accu, stage);
  accu.add(";");
  increment->codegen(accu, stage);
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  body->codegen(accu, CodegenStage::define());
  --accu.indent;
  --accu.indent;
  accu.newLine("}");
}

void WhileLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage.isDefine);
  accu.add("while (");
  cond->codegen(accu, CodegenStage::define());
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  body->codegen(accu, CodegenStage::define());
  --accu.indent;
  accu.newLine();
}

void ImportStatement::codegen(Accu& accu, CodegenStage stage) const {
  // ast can be null if import was already generated or is secondary and not public
  if (ast)
    for (auto& elem : ast->elems) {
      elem->codegen(accu, stage.setImport());
      accu.newLine("");
    }
}

void EnumDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (!stage.isDefine) {
    accu.add("enum class " + name + " {");
    ++accu.indent;
    for (auto& elem : elements)
      accu.newLine(elem + ",");
    --accu.indent;
    accu.newLine("};");
  }
}

void EnumConstant::codegen(Accu& accu, CodegenStage) const {
  accu.add(enumName + "::" + enumElement);
}

void ConceptDefinition::codegen(Accu&, CodegenStage) const {
}

void BreakStatement::codegen(Accu& accu, CodegenStage) const {
  accu.add("break;");
}

void ContinueStatement::codegen(Accu& accu, CodegenStage) const {
  accu.add("continue;");
}

void ArrayLiteral::codegen(Accu& accu, CodegenStage stage) const {
  accu.add("make_array(");
  for (auto& elem : contents) {
    elem->codegen(accu, stage);
    accu.add(", ");
  }
  accu.pop_back();
  accu.pop_back();
  accu.add(")");
}
