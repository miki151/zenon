#include <typeinfo>

#include "codegen.h"
#include "ast.h"

using namespace std;



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

void Constant::codegen(Accu& accu) const {
  switch (*type.getValueMaybe<ArithmeticType>()) {
    case ArithmeticType::STRING:
      accu.add("\"" + value + "\"");
      break;
    case ArithmeticType::CHAR:
      accu.add("'" + value + "'");
      break;
    default:
      accu.add(value);
      break;
  }
}

void Variable::codegen(Accu& accu) const {
  accu.add(identifier);
}

void BinaryExpression::codegen(Accu& accu) const {
  accu.add("(");
  e1->codegen(accu);
  accu.add(") ");
  if (op == Operator::SUBSCRIPT)
    accu.add("[");
  else
    accu.add(getString(op) + " "s);
  if (op != Operator::MEMBER_ACCESS)
    accu.add("(");
  e2->codegen(accu);
  if (op != Operator::MEMBER_ACCESS)
    accu.add(")");
  if (op == Operator::SUBSCRIPT)
    accu.add("]");
}

void StatementBlock::codegen(Accu& accu) const {
  accu.add("{");
  ++accu.indent;
  for (auto& s : elems) {
    accu.newLine();
    s->codegen(accu);
  }
  --accu.indent;
  accu.newLine("}");
}

void VariableDeclaration::codegen(Accu& accu) const {
  accu.add(getName(*realType) + " " + identifier);
  if (initExpr) {
    accu.add(" = ");
    initExpr->codegen(accu);
  }
  accu.add(";");
}

void IfStatement::codegen(Accu& accu) const {
  accu.newLine("if (");
  cond->codegen(accu);
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  ifTrue->codegen(accu);
  --accu.indent;
  if (ifFalse) {
    accu.newLine("else");
    ++accu.indent;
    accu.newLine();
    ifFalse->codegen(accu);
    --accu.indent;
  }
}

void ReturnStatement::codegen(Accu& accu) const {
  accu.add("return");
  if (expr) {
    accu.add(" ");
    expr->codegen(accu);
  }
  accu.add(";");
}

// identifier(args)
// identifier{args}
// variant { variant::enumId, { variant::identifier{args}}}


static string joinTemplateParams(const vector<string>& params) {
  if (params.empty())
    return "";
  else
    return "<" + combine(params, ", ") + ">";
}

static void genFunctionCall(Accu& accu, const IdentifierInfo& identifier, const FunctionType& functionType,
    vector<Expression*> arguments) {
  string prefix;
  string suffix;
  string id = identifier.parts.back().name +
      joinTemplateParams(transform(functionType.templateParams, [](const auto& arg) { return getName(arg); } ));
  if (functionType.parentType)
    id = getName(*functionType.parentType) + "::" + id;
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
    arg->codegen(accu);
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(suffix);
}

void FunctionCall::codegen(Accu& accu) const {
  genFunctionCall(accu, identifier, *functionType, extractRefs(arguments));
}

void FunctionCallNamedArgs::codegen(Accu& accu) const {
  genFunctionCall(accu, identifier, *functionType, transform(arguments, [](const auto& arg) { return arg.expr.get(); }));
}

static void considerTemplateParams(Accu& accu, const vector<string>& params) {
  if (!params.empty()) {
    accu.add("template <");
    for (auto& param : params)
      accu.add("typename " + param + ", ");
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
  considerTemplateParams(accu, templateParams);
  if (!structName.empty())
    structName += "::";
  string ret = getName(*functionType->retVal) + " " + structName + getFunctionName(nameOrOp) + "(";
  for (auto& param : functionType->params)
    ret.append(getName(*param.type) + " " + param.name + ", ");
  if (!parameters.empty()) {
    ret.pop_back();
    ret.pop_back();
  }
  ret.append(")");
  accu.add(ret);
};

void FunctionDefinition::codegen(Accu& accu) const {
  addSignature(accu, "");
  accu.newLine("");
  body->codegen(accu);
}

void FunctionDefinition::declare(Accu& accu) const {
  if (!templateParams.empty())
    codegen(accu);
  else {
    addSignature(accu, "");
    accu.add(";");
    accu.newLine("");
  }
}

string codegen(const AST& ast) {
  Accu accu;
  accu.add("#include \"codegen_includes/lite_str.h\"\n using string = lite_str<>;");
  accu.newLine();
  for (auto& elem : ast.elems) {
    elem->codegen(accu);
    accu.newLine();
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu) const {
  expr->codegen(accu);
  accu.add(";");
}

void StructDefinition::generate(Accu& accu, bool import) const {
  if (external)
    return;
  considerTemplateParams(accu, templateParams);
  accu.add("struct " + name + " {");
  ++accu.indent;
  for (auto& method : methods) {
    accu.newLine();
    method->addSignature(accu, "");
    accu.add(";");
  }
  for (auto& member : type->members)
    accu.newLine(getName(*member.type) + " " + member.name + ";");
  --accu.indent;
  accu.newLine("};");
  accu.newLine();
  for (auto& method : methods)
    if (!import || !templateParams.empty() || !method->templateParams.empty()) {
      considerTemplateParams(accu, templateParams);
      method->addSignature(accu, name + joinTemplateParams(templateParams));
      accu.newLine();
      method->body->codegen(accu);
      accu.newLine();
    }
}

void StructDefinition::codegen(Accu& accu) const {
  generate(accu, false);
}

void StructDefinition::declare(Accu& accu) const {
  generate(accu, true);
}

constexpr const char* variantEnumeratorPrefix = "Enum_";
constexpr const char* variantUnionEntryPrefix = "Union_";
constexpr const char* variantUnionElem = "unionElem";

void VariantDefinition::codegen(Accu& accu) const {
  generate(accu, false);
}

void VariantDefinition::generate(Accu& accu, bool import) const {
  Accu impls;
  considerTemplateParams(accu, templateParams);
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
  for (auto& elem : type->members) {
    string signature = elem.name + "(";
    if (!(elem.type == ArithmeticType::VOID))
      signature += "const " + getName(*elem.type) + "& elem";
    signature += ")";
    string params = joinTemplateParams(templateParams);
    accu.newLine("static " + name + params + " " + signature + ";");
    considerTemplateParams(impls, templateParams);
    impls.add(name + params + " " + name + params + "::" + signature + " {");
    ++impls.indent;
    impls.newLine(name + " ret;");
    impls.newLine("ret."s + variantUnionElem + " = " + variantEnumeratorPrefix + elem.name + ";");
    if (!(elem.type == ArithmeticType::VOID))
      impls.newLine("ret."s + variantUnionEntryPrefix + elem.name + " = elem;");
    impls.newLine("return ret;");
    --impls.indent;
    impls.newLine("}");
    impls.newLine("");
  }
  accu.newLine("union {");
  ++accu.indent;
  accu.newLine("bool dummy;");
  for (auto& elem : type->members)
    if (!(elem.type == ArithmeticType::VOID))
      accu.newLine(getName(*elem.type) + " " + variantUnionEntryPrefix + elem.name + ";");
  --accu.indent;
  accu.newLine("};");
  --accu.indent;
  accu.newLine("};");
  accu.newLine();
  for (auto& method : methods)
    if (!import || !templateParams.empty() || !method->templateParams.empty()) {
      considerTemplateParams(accu, templateParams);
      method->addSignature(accu, name + joinTemplateParams(templateParams));
      accu.newLine();
      method->body->codegen(accu);
      accu.newLine();
    }
  if (!import)
    accu.newLine(impls.generate());
}


void VariantDefinition::declare(Accu& accu) const {
  generate(accu, templateParams.empty());
}


constexpr const char* variantTmpRef = "variantTmpRef";

void SwitchStatement::codegen(Accu& accu) const {
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
  expr->codegen(accu);
  accu.add(") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    accu.newLine("case " + subtypesPrefix + caseElem.id + ": {");
    ++accu.indent;
    accu.newLine();
    caseElem.block->codegen(accu);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}");
}

void SwitchStatement::codegenVariant(Accu& accu) const {
  accu.add("{ auto&& "s + variantTmpRef + " = ");
  expr->codegen(accu);
  accu.add(";");
  accu.newLine("switch ("s + variantTmpRef + "."s + variantUnionElem + ") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    accu.newLine("case "s + subtypesPrefix + variantEnumeratorPrefix + caseElem.id + ": {");
    ++accu.indent;
    if (caseElem.declareVar) {
      accu.newLine("auto&& "s + caseElem.id + " = " + variantTmpRef + "." + variantUnionEntryPrefix + caseElem.id + ";");
    }
    accu.newLine();
    caseElem.block->codegen(accu);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  if (defaultBlock) {
    accu.newLine("default: {");
    ++accu.indent;
    accu.newLine();
    defaultBlock->codegen(accu);
    accu.newLine("break;");
    --accu.indent;
    accu.newLine("}");
  }
  --accu.indent;
  accu.newLine("}}");
}

void UnaryExpression::codegen(Accu& accu) const {
  accu.add(getString(op));
  accu.add("(");
  expr->codegen(accu);
  accu.add(") ");
}

void EmbedStatement::codegen(Accu& accu) const {
  accu.newLine(value);
}

void EmbedStatement::declare(Accu& accu) const {
  if (isPublic)
    codegen(accu);
}

bool EmbedStatement::hasReturnStatement(const State&) const {
  return true;
}

void ForLoopStatement::codegen(Accu& accu) const {
  accu.add("for (");
  init->codegen(accu);
  cond->codegen(accu);
  accu.add(";");
  iter->codegen(accu);
  accu.add(")");
  ++accu.indent;
  accu.newLine();
  body->codegen(accu);
  --accu.indent;
  accu.newLine();
}

void ImportStatement::codegen(Accu& accu) const {
  // ast can be null if import was already generated or is secondary and not public
  if (ast)
    for (auto& elem : ast->elems) {
      elem->declare(accu);
      accu.newLine("");
    }
}

void ImportStatement::declare(Accu& accu) const {
  codegen(accu);
}

void Statement::declare(Accu& accu) const {
  FATAL << "Can't declare statement " << typeid(*this).name();
}


void EnumDefinition::codegen(Accu& accu) const {
  accu.add("enum class " + name + " {");
  ++accu.indent;
  for (auto& elem : elements)
    accu.newLine(elem + ",");
  --accu.indent;
  accu.newLine("};");
}

void EnumDefinition::declare(Accu& accu) const {
  codegen(accu);
}

void EnumConstant::codegen(Accu& accu) const {
  accu.add(enumName + "::" + enumElement);
}
