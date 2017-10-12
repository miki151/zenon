#include "codegen.h"
#include "ast.h"

using namespace std;



struct Accu {
  public:
  void add(const string& s) {
    buf += s;
  }

  void newLine(const string& s = "") {
    const int indentSize = 2;
    buf += "\n" + string(indent * indentSize, ' ');
    buf += s;
  }

  int indent = 0;
  string buf;
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
  accu.add(getString(op) + " ("s);
  e2->codegen(accu);
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
    accu.buf.pop_back();
    accu.buf.pop_back();
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
    accu.buf.pop_back();
    accu.buf.pop_back();
  }
  accu.add(constructor ? "}" : ")");
}

void FunctionDefinition::codegen(Accu& accu) const {
  accu.add(returnType + " " + name + "(");
  for (auto& param : parameters) {
    accu.add(param.type + " " + param.name);
    accu.add(", ");
  }
  if (!parameters.empty()) {
    accu.buf.pop_back();
    accu.buf.pop_back();
  }
  accu.add(")");
  accu.newLine();
  body->codegen(accu);
}

string codegen(const AST& ast) {
  Accu accu;
  for (auto& elem : ast.elems) {
    elem->codegen(accu);
    accu.newLine();
    accu.newLine();
  }
  return accu.buf;
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