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
  accu.add(type.toString() + " " + identifier);
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

void FunctionDefinition::codegen(Accu& accu) const {
  auto getPrototype = [this]() {
    string ret = returnType.toString() + " " + name + "(";
    for (auto& param : parameters)
      ret.append(param.type.toString() + " " + param.name + ", ");
    if (!parameters.empty()) {
      ret.pop_back();
      ret.pop_back();
    }
    ret.append(")");
    return ret;
  };
  considerTemplateParams(accu, templateParams);
  accu.add(getPrototype());
  accu.newLine("");
  body->codegen(accu);
}

string codegen(const AST& ast) {
  Accu accu;
  for (auto& elem : ast.elems) {
    elem->codegen(accu);
    accu.newLine();
//    accu.newLine();
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu) const {
  expr->codegen(accu);
  accu.add(";");
}

void StructDefinition::codegen(Accu& accu) const {
  considerTemplateParams(accu, templateParams);
  accu.add("struct " + name + " {");
  ++accu.indent;
  for (auto& member : members)
    accu.newLine(member.type.toString() + " " + member.name + ";");
  --accu.indent;
  accu.newLine("};");
}

void MemberAccessType::codegen(Accu& accu) const {
  accu.add(name);
}

constexpr const char* variantEnumeratorPrefix = "Enum_";
constexpr const char* variantUnionEntryPrefix = "Union_";
constexpr const char* variantUnionElem = "unionElem";

void VariantDefinition::codegen(Accu& accu) const {
  considerTemplateParams(accu, templateParams);
  accu.add("struct " + name + " {");
  ++accu.indent;
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
    accu.newLine("static " + name + " " + elem.name + "(");
    if (!(elem.type == IdentifierInfo("void")))
      accu.add("const " + elem.type.toString() + "& elem");
    accu.add(") {");
    ++accu.indent;
    accu.newLine(name + " ret;");
    accu.newLine("ret."s + variantUnionElem + " = " + variantEnumeratorPrefix + elem.name + ";");
    if (!(elem.type == IdentifierInfo("void")))
      accu.newLine("ret."s + variantUnionEntryPrefix + elem.name + " = elem;");
    accu.newLine("return ret;");
    --accu.indent;
    accu.newLine("}");
/*      accu.newLine(name + "(" + variantTagPrefix + elem.name + ", const " + elem.type.toString() + "& elem) : unionElem("
          + variantEnumeratorPrefix + elem.name + "), " + elem.name + "(elem) {}");

    accu.newLine(name + "(" + variantTagPrefix + elem.name + ", const " + elem.type.toString() + "& elem) : unionElem("
        + variantEnumeratorPrefix + elem.name + "), " + elem.name + "(elem) {}");*/
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
}

constexpr const char* variantTmpRef = "variantTmpRef";

void SwitchStatement::codegen(Accu& accu) const {
  accu.add("{ auto&& "s + variantTmpRef + " = ");
  expr->codegen(accu);
  accu.add(";");
  accu.newLine("switch ("s + variantTmpRef + "."s + variantUnionElem + ") {");
  ++accu.indent;
  for (auto& caseElem : caseElems) {
    accu.newLine("case "s + subtypesPrefix + variantEnumeratorPrefix + caseElem.id + ":");
    accu.add("{");
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
  expr->codegen(accu);
}


void EmbedStructDefinition::codegen(Accu&) const {
}


void EmbedStatement::codegen(Accu& accu) const {
  accu.newLine(value);
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
