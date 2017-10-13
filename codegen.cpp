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
  accu.add(name);
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

void FunctionCall::codegen(Accu& accu) const {
  accu.add(name + (constructor ? "{" : "("));
  for (auto& arg : arguments) {
    arg->codegen(accu);
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(constructor ? "}" : ")");
}

void FunctionCallNamedArgs::codegen(Accu& accu) const {
  accu.add(name + (constructor ? "{" : "("));
  for (auto& arg : arguments) {
    arg.expr->codegen(accu);
    accu.add(", ");
  }
  if (!arguments.empty()) {
    accu.pop_back();
    accu.pop_back();
  }
  accu.add(constructor ? "}" : ")");
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
    accu.newLine();
  }
  return accu.generate();
}

void ExpressionStatement::codegen(Accu& accu) const {
  expr->codegen(accu);
  accu.add(";");
}

void StructDeclaration::codegen(Accu& accu) const {
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
