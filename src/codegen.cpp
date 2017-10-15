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
  if (op != BinaryOperator::MEMBER_ACCESS)
    accu.add(" (");
  e2->codegen(accu);
  if (op != BinaryOperator::MEMBER_ACCESS)
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
  accu.add(type + " " + identifier);
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


void FunctionCall::codegen(Accu& accu) const {
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
    case FunctionCallType::VARIANT_CONSTRUCTOR_INTERNAL:
      prefix = combine(identifier.namespaces, "::") + "{ " + id + "{"; suffix = "}}";
      break;
    case FunctionCallType::VARIANT_CONSTRUCTOR_EXTERNAL:
      prefix = combine(identifier.namespaces, "::") + "{ " + identifier.name + "{"; suffix = "}}";
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

void FunctionCallNamedArgs::codegen(Accu& accu) const {
/*  accu.add(name + (constructor ? "{" : "("));
  for (auto& arg : arguments) {
    arg.expr->codegen(accu);
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(constructor ? "}" : ")");*/
}

void FunctionDefinition::codegen(Accu& accu) const {
  auto getPrototype = [this]() {
    string ret = returnType + " " + name + "(";
    for (auto& param : parameters)
      ret.append(param.type + " " + param.name + ", ");
    if (!parameters.empty()) {
      ret.pop_back();
      ret.pop_back();
    }
    ret.append(")");
    return ret;
  };
  if (embed) {
    accu.add(getPrototype() + ";");
  }
  Accu::Position pos = embed ? Accu::EMBED : Accu::CURRENT;
  accu.add(getPrototype(), pos);
  accu.newLine("", pos);
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
  accu.add("struct " + name + " {");
  ++accu.indent;
  for (auto& member : members)
    accu.newLine(member.type + " " + member.name + ";");
  --accu.indent;
  accu.newLine("};");
}

void MemberAccessType::codegen(Accu& accu) const {
  accu.add(name);
}

void EmbedBlock::codegen(Accu& accu) const {
  accu.add("{", Accu::EMBED);
  accu.newLine("", Accu::EMBED);
  accu.add(content, Accu::EMBED);
  accu.newLine("}", Accu::EMBED);
}


void EmbedInclude::codegen(Accu& accu) const {
  accu.newLine(path, Accu::EMBED);
}

constexpr const char* variantEnumeratorPrefix = "Enum_";
constexpr const char* variantUnionEntryPrefix = "Union_";
constexpr const char* variantUnionElem = "unionElem";

void VariantDefinition::codegen(Accu& accu) const {
  accu.add("struct " + name + " {");
  ++accu.indent;
  accu.newLine();
  for (auto& subtype : subtypes)
    subtype.type.visit(
        [&](const unique_ptr<StructDefinition>& s) {
          s->codegen(accu);
          accu.newLine();
        },
        [&](const auto&) {}
    );
  accu.add("enum {");
  vector<string> typeNames;
  for (auto& subtype : subtypes) {
    subtype.type.visit(
        [&](const string& type) {
          typeNames.push_back(type);
        },
        [&](const unique_ptr<StructDefinition>& s) {
          typeNames.push_back(s->name);
        }
    );
  }
  accu.add(combine(transform(typeNames, [](const string& e){ return variantEnumeratorPrefix + e;}), ", ") + "} "
      + variantUnionElem + ";");
  for (auto& t : typeNames) {
    accu.newLine(name + "(const " + t + "& elem) : unionElem(" + variantEnumeratorPrefix + t + "), " +
        variantUnionEntryPrefix + t + "(elem) {}");
  }
  accu.newLine("union {");
  ++accu.indent;
  for (auto& type : typeNames)
    accu.newLine(type + " " + variantUnionEntryPrefix + type + ";");
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
    accu.newLine("case "s + subtypesPrefix + variantEnumeratorPrefix + caseElem.type + ":");
    accu.add("{");
    ++accu.indent;
    if (caseElem.id) {
      accu.newLine("auto&& " + *caseElem.id + " = " + variantTmpRef + "." + variantUnionEntryPrefix + caseElem.type + ";");
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
