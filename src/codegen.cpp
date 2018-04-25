#include <typeinfo>

#include "codegen.h"
#include "ast.h"


struct Accu {
  public:

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
};

void Constant::codegen(Accu& accu, CodegenStage) const {
  if (type == ArithmeticType::STRING)
    accu.add("\"" + value + "\"");
  else if (type == ArithmeticType::CHAR)
    accu.add("'" + value + "'");
  else
    accu.add(value);
}

void Variable::codegen(Accu& accu, CodegenStage) const {
  accu.add(identifier);
}

void BinaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.add("(");
  e1->codegen(accu, stage);
  accu.add(") ");
  if (op == Operator::SUBSCRIPT)
    accu.add("[");
  else
    accu.add(getString(op) + " "s);
  if (op != Operator::MEMBER_ACCESS)
    accu.add("(");
  e2->codegen(accu, stage);
  if (op != Operator::MEMBER_ACCESS)
    accu.add(")");
  if (op == Operator::SUBSCRIPT)
    accu.add("]");
}

void StatementBlock::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.add("{");
  ++accu.indent;
  for (auto& s : elems) {
    accu.newLine();
    s->codegen(accu, stage);
  }
  --accu.indent;
  accu.newLine("}");
}

void VariableDeclaration::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.add(realType.get()->getName() + " " + identifier);
  if (initExpr) {
    accu.add(" = ");
    initExpr->codegen(accu, stage);
  }
  accu.add(";");
}

void IfStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.newLine("if (");
  cond->codegen(accu, stage);
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
  CHECK(stage == DEFINE);
  accu.add("return");
  if (expr) {
    accu.add(" ");
    expr->codegen(accu, stage);
  }
  accu.add(";");
}

static void genFunctionCall(Accu& accu, const IdentifierInfo& identifier, const FunctionType& functionType,
    vector<Expression*> arguments) {
  string prefix;
  string suffix;
  string id = identifier.parts.back().name +
      joinTemplateParams(functionType.templateParams);
  if (functionType.parentType)
    id = functionType.parentType->getName() + "::" + id;
  switch (functionType.callType) {
    case FunctionCallType::FUNCTION:
      prefix = id + "("; suffix = ")";
      break;
    case FunctionCallType::CONSTRUCTOR:
      prefix = id + "{"; suffix = "}";
      break;
  }
  accu.add(prefix);
  for (auto& arg : arguments) {
    arg->codegen(accu, Node::DEFINE);
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(suffix);
}

void FunctionCall::codegen(Accu& accu, CodegenStage) const {
  genFunctionCall(accu, identifier, *functionType, extractRefs(arguments));
}

void FunctionCallNamedArgs::codegen(Accu& accu, CodegenStage) const {
  genFunctionCall(accu, identifier, *functionType, transform(arguments, [](const auto& arg) { return arg.expr.get(); }));
}

static void considerTemplateParams(Accu& accu, const vector<TemplateParameter>& params) {
  if (!params.empty()) {
    accu.add("template <");
    for (auto& param : params)
      accu.add("typename " + param.name + ", ");
    accu.pop_back();
    accu.pop_back();
    accu.add(">");
    accu.newLine();
  }
}

string getFunctionName(const variant<string, Operator>& nameOrOp) {
  return nameOrOp.visit(
      [&](const string& s) {
        return s;
      },
      [&](Operator op) {
        return "operator "s + getString(op);
      }
  );
}

void FunctionDefinition::addSignature(Accu& accu, string structName) const {
  considerTemplateParams(accu, templateInfo.params);
  if (!structName.empty())
    structName += "::";
  string ret = functionType->retVal->getName() + " " + structName + getFunctionName(nameOrOp) + "(";
  for (auto& param : functionType->params)
    ret.append(param.type->getName() + " " + param.name + ", ");
  if (!parameters.empty()) {
    ret.pop_back();
    ret.pop_back();
  }
  ret.append(")");
  accu.add(ret);
};

void FunctionDefinition::codegen(Accu& accu, CodegenStage stage) const {
  addSignature(accu, "");
  if (stage == DECLARE || (stage == IMPORT && templateInfo.params.empty())) {
    accu.add(";");
    accu.newLine("");
  } else {
    accu.newLine("");
    body->codegen(accu, DEFINE);
  }
}

string codegen(const AST& ast) {
  Accu accu;
  accu.add(
        "#include \"codegen_includes/variant_helpers.h\"\n"
        "#include \"codegen_includes/lite_str.h\"\n"
        "using string = lite_str<>;");
  accu.newLine();
  for (auto& elem : ast.elems) {
    elem->codegen(accu, Node::DECLARE);
    accu.newLine();
  }
  for (auto& elem : ast.elems) {
    elem->codegen(accu, Node::DEFINE);
    accu.newLine();
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  expr->codegen(accu, stage);
  accu.add(";");
}

void StructDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (external)
    return;
  if (stage != DEFINE) {
    considerTemplateParams(accu, templateInfo.params);
    accu.add("struct " + name + " {");
    ++accu.indent;
    for (auto& method : methods) {
      accu.newLine();
      method->addSignature(accu, "");
      accu.add(";");
    }
    for (auto& member : type->getContext().getBottomLevelVariables())
      accu.newLine(type->getContext().getTypeOfVariable(member)->getUnderlying()->getName() + " " + member + ";");
    --accu.indent;
    accu.newLine("};");
  }
  accu.newLine();
  for (auto& method : methods)
    if (stage == DEFINE || ((!templateInfo.params.empty() || !method->templateInfo.params.empty()) && stage == IMPORT)) {
      considerTemplateParams(accu, templateInfo.params);
      method->addSignature(accu, name + joinTemplateParams(type->templateParams));
      accu.newLine();
      method->body->codegen(accu, DEFINE);
      accu.newLine();
    }
}

constexpr const char* variantEnumeratorPrefix = "Enum_";
constexpr const char* variantUnionEntryPrefix = "Union_";
constexpr const char* variantUnionElem = "unionElem";

void VariantDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (stage != DEFINE) {
    considerTemplateParams(accu, templateInfo.params);
    accu.add("struct " + name + " {");
    ++accu.indent;
    for (auto& method : methods) {
      accu.newLine();
      method->addSignature(accu, "");
      accu.add(";");
    }
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
        signature += "const " + alternative.type->getName() + "& elem";
      signature += ")";
      string params = joinTemplateParams(type->templateParams);
      accu.newLine("static " + name + params + " " + signature + ";");
    }
    accu.newLine("union {");
    ++accu.indent;
    accu.newLine("bool dummy;");
    for (auto& alternative : type->alternatives) {
      if (alternative.type != ArithmeticType::VOID)
        accu.newLine(alternative.type->getName() + " " + variantUnionEntryPrefix + alternative.name + ";");
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
          accu.newLine("std::forward<Visitor>(v)("s + variantUnionEntryPrefix + alternative.name + ");");
          accu.newLine("break;");
          --accu.indent;
        } else
          accu.newLine("case "s + variantEnumeratorPrefix + alternative.name + ": break;");
      }
      --accu.indent;
      accu.newLine("}");
      --accu.indent;
    };
    accu.newLine("template <typename Visitor>");
    accu.newLine("void visit(Visitor&& v) const {");
    visitBody();
    accu.newLine("}");
    accu.newLine("template <typename Visitor>");
    accu.newLine("void visit(Visitor&& v) {");
    visitBody();
    accu.newLine("}");
    accu.newLine(name + "(const " + name + "& o) { VariantHelper<" + name + ">::copy(o, *this);  }");
    accu.newLine("~" + name + "() { VariantHelper<" + name + ">::destroy(*this); }");
    accu.newLine("private:" + name + "() {}");
    --accu.indent;
    accu.newLine("};");
    accu.newLine();
  }
  for (auto& method : methods)
    if (stage == DEFINE || ((!templateInfo.params.empty() || !method->templateInfo.params.empty()) && stage == IMPORT)) {
      considerTemplateParams(accu, templateInfo.params);
      method->addSignature(accu, name + joinTemplateParams(type->templateParams));
      accu.newLine();
      method->body->codegen(accu, DEFINE);
      accu.newLine();
    }
  if (stage == DEFINE || (!templateInfo.params.empty() && stage == IMPORT))
    for (auto& alternative : type->alternatives) {
      string signature = alternative.name + "(";
      if (alternative.type != ArithmeticType::VOID)
        signature += "const " + alternative.type->getName() + "& elem";
      signature += ")";
      string params = joinTemplateParams(type->templateParams);
      considerTemplateParams(accu, templateInfo.params);
      accu.add(name + params + " " + name + params + "::" + signature + " {");
      ++accu.indent;
      accu.newLine(name + " ret;");
      accu.newLine("ret."s + variantUnionElem + " = " + variantEnumeratorPrefix + alternative.name + ";");
      if (!(alternative.type == ArithmeticType::VOID))
        accu.newLine("new (&ret."s + variantUnionEntryPrefix + alternative.name + ") " + alternative.type->getName() + "(elem);");
      accu.newLine("return ret;");
      --accu.indent;
      accu.newLine("}");
      accu.newLine("");
    }
}

constexpr const char* variantTmpRef = "variantTmpRef";

void SwitchStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
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
  expr->codegen(accu, DEFINE);
  accu.add(") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    accu.newLine("case " + subtypesPrefix + caseElem.id + ": {");
    ++accu.indent;
    accu.newLine();
    caseElem.block->codegen(accu, DEFINE);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu, DEFINE);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}");
}

void SwitchStatement::codegenVariant(Accu& accu) const {
  accu.add("{ auto&& "s + variantTmpRef + " = ");
  expr->codegen(accu, DEFINE);
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
    caseElem.block->codegen(accu, DEFINE);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu, DEFINE);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}}");
}

void UnaryExpression::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.add(getString(op));
  accu.add("(");
  expr->codegen(accu, DEFINE);
  accu.add(") ");
}

void EmbedStatement::codegen(Accu& accu, CodegenStage stage) const {
  if (stage == DEFINE || (isPublic && stage == IMPORT))
    accu.newLine(value);
}

bool EmbedStatement::hasReturnStatement(const Context&) const {
  return true;
}

void ForLoopStatement::codegen(Accu& accu, CodegenStage stage) const {
  CHECK(stage == DEFINE);
  accu.add("for (");
  init->codegen(accu, DEFINE);
  cond->codegen(accu, DEFINE);
  accu.add(";");
  iter->codegen(accu, DEFINE);
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  body->codegen(accu, DEFINE);
  --accu.indent;
  accu.newLine();
}

void ImportStatement::codegen(Accu& accu, CodegenStage stage) const {
  // ast can be null if import was already generated or is secondary and not public
  if (ast && stage != DEFINE)
    for (auto& elem : ast->elems) {
      elem->codegen(accu, IMPORT);
      accu.newLine("");
    }
}

void EnumDefinition::codegen(Accu& accu, CodegenStage stage) const {
  if (stage != DEFINE) {
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

void ConceptDefinition::codegen(Accu&, Node::CodegenStage) const {
}
