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
  accu.add(value);
}

void Variable::codegen(Accu& accu) const {
  accu.add(identifier.toString());
}

void BinaryExpression::codegen(Accu& accu) const {
  accu.add("(");
  e1->codegen(accu);
  accu.add(") ");
  accu.add(getString(op));
  if (op != Operator::MEMBER_ACCESS)
    accu.add(" (");
  e2->codegen(accu);
  if (op != Operator::MEMBER_ACCESS)
    accu.add(")");
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


static void genFunctionCall(Accu& accu, IdentifierInfo identifier, FunctionCallType callType,
    vector<Expression*> arguments) {
  string prefix;
  string suffix;
  string id = identifier.toString();
  switch (callType) {
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
  genFunctionCall(accu, identifier, callType, extractRefs(arguments));
}

void FunctionCallNamedArgs::codegen(Accu& accu) const {
  genFunctionCall(accu, identifier, callType, transform(arguments, [](const auto& arg) { return arg.expr.get(); }));
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

void FunctionDefinition::addSignature(Accu& accu, string structName) const {
  considerTemplateParams(accu, templateParams);
  if (!structName.empty())
    structName += "::";
  string ret = returnType.toString() + " " + structName + name + "(";
  for (auto& param : parameters)
    ret.append(param.type.toString() + " " + param.name + ", ");
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

static string joinTemplateParams(const vector<string>& params) {
  if (params.empty())
    return "";
  else
    return "<" + combine(params, ", ") + ">";
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
  for (auto& member : members)
    accu.newLine(member.type.toString() + " " + member.name + ";");
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
  for (auto& subtype : elements)
    subtype.type.toString();
  accu.add("enum {");
  vector<string> typeNames;
  for (auto& subtype : elements)
    typeNames.push_back(subtype.name);
  accu.add(combine(transform(typeNames, [](const string& e){ return variantEnumeratorPrefix + e;}), ", ") + "} "
      + variantUnionElem + ";");
  for (auto& elem : elements) {
    string signature = elem.name + "(";
    if (!(elem.type == IdentifierInfo("void")))
      signature += "const " + elem.type.toString() + "& elem";
    signature += ")";
    string params = joinTemplateParams(templateParams);
    accu.newLine("static " + name + params + " " + signature + ";");
    considerTemplateParams(impls, templateParams);
    impls.add(name + params + " " + name + params + "::" + signature + " {");
    ++impls.indent;
    impls.newLine(name + " ret;");
    impls.newLine("ret."s + variantUnionElem + " = " + variantEnumeratorPrefix + elem.name + ";");
    if (!(elem.type == IdentifierInfo("void")))
      impls.newLine("ret."s + variantUnionEntryPrefix + elem.name + " = elem;");
    impls.newLine("return ret;");
    --impls.indent;
    impls.newLine("}");
    impls.newLine("");
  }
  accu.newLine("union {");
  ++accu.indent;
  accu.newLine("bool dummy;");
  for (auto& elem : elements)
    if (!(elem.type == IdentifierInfo("void")))
      accu.newLine(elem.type.toString() + " " + variantUnionEntryPrefix + elem.name + ";");
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
