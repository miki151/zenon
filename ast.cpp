#include "ast.h"

using namespace std;

Node::Node(CodeLoc l) : codeLoc(l) {}


BinaryExpression::BinaryExpression(CodeLoc loc, char o, unique_ptr<Expression> u1, unique_ptr<Expression> u2)
    : Expression(loc), op(o) {
  e1 = std::move(u1);
  e2 = std::move(u2);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<Expression> c, unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), cond(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, ArithmeticType t, string v) : Expression(l), type(t), value(v) {
  INFO << "Created constant \"" << v << "\" of type " << getName(t);
}

Variable::Variable(CodeLoc l, string n) : Expression(l), name(n) {}

FunctionCall::FunctionCall(CodeLoc l, string n) : Expression(l), name(n) {}

VariableDecl::VariableDecl(CodeLoc l, string t, string id) : Statement(l), type(t), identifier(id) {
  INFO << "Declared variable \"" << id << "\" of type \"" << t << "\"";
}

FunctionDefinition::FunctionDefinition(CodeLoc l, string r, string n) : TopLevelStatement(l), returnType(r), name(n) {}

ArithmeticType getType(CodeLoc loc, const string& name) {
  if (auto type = getType(name))
    return *type;
  else
    loc.error("Unrecognized type: " + name);
  return {};
}

ArithmeticType Constant::getType(const State&) const {
  return type;
}

ArithmeticType Variable::getType(const State& state) const {
  if (auto type = state.getType(name))
    return type->visit(
        [&](const FunctionType&) {
          codeLoc.error("Function name found in expression");
          return ArithmeticType::INT;
        },
        [&](const ArithmeticType t) {
          return t;
        }
    );
  else
    codeLoc.error("Undefined variable: " + name);
  return ArithmeticType::INT;
}


ArithmeticType FunctionCall::getType(const State& state) const {
  if (auto type = state.getType(name))
    return type->visit(
        [&](const FunctionType& t) {
          int numParams = (int) t.params.size();
          codeLoc.check(arguments.size() == numParams,
               "Expected " + to_string(numParams) + " arguments, got " + to_string(arguments.size()));
          for (int i = 0; i < numParams; ++i) {
            auto argType = arguments[i]->getType(state);
            auto paramType = t.params[i];
            arguments[i]->codeLoc.check(argType == paramType,
                "Function call argument " + to_string(i + 1) + " mismatch: expected \""s + getName(paramType) +
                "\", got \""s + getName(argType) + "\"");
          }
          return t.retVal;
        },
        [&](const ArithmeticType&) -> ArithmeticType {
          codeLoc.error("Trying to call an arithmetic type");
          return {};
        }
    );
  else
    codeLoc.error("Function not found: \"" + name + "\"");
  return {};
}

ArithmeticType BinaryExpression::getType(const State& state) const {
  switch (op) {
    case '+':
    case '-':
      e1->codeLoc.check(e1->getType(state) == ArithmeticType::INT, "Expected expression type: int");
      e2->codeLoc.check(e2->getType(state) == ArithmeticType::INT, "Expected expression type: int");
      return ArithmeticType::INT;
    case '<':
    case '>':
      e1->codeLoc.check(e1->getType(state) == ArithmeticType::INT, "Expected expression type: int");
      e2->codeLoc.check(e2->getType(state) == ArithmeticType::INT, "Expected expression type: int");
      return ArithmeticType::BOOL;
    default:
      FATAL << "Unrecognized operator: " + string(1, op);
      return ArithmeticType::INT;
  }
}

void StatementBlock::check(State& state) const {
  auto stateCopy = state;
  for (auto& s : elems) {
    s->check(stateCopy);
  }
}

void IfStatement::check(State& state) const {
  codeLoc.check(cond->getType(state) == ArithmeticType::BOOL, "Expected type bool inside if statement");
  ifTrue->check(state);
  if (ifFalse)
    ifFalse->check(state);
}

void VariableDecl::check(State& state) const {
  state.setType(identifier, getType(codeLoc, type));
}

void ReturnStatement::check(State& state) const {
  if (!expr)
    codeLoc.check(state.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(state);
    codeLoc.check(*state.getReturnType() == returnType,
        "Attempting to return value of type \""s + getName(returnType) +
         "\" in a function returning \""s + getName(*state.getReturnType()) + "\"");
  }
}

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

void FunctionDefinition::check(State& state) const {
  State stateCopy = state;
  if (auto returnType = getType(this->returnType)) {
    vector<ArithmeticType> params;
    for (auto& p : parameters)
      if (auto paramType = getType(p->type)) {
        stateCopy.setType(p->identifier, *paramType);
        params.push_back(*paramType);
      } else
        p->codeLoc.error("Unrecognized parameter type: \"" + p->type);
    state.setType(name, FunctionType { *returnType, params });
    stateCopy.setType(name, FunctionType { *returnType, params });
    stateCopy.setReturnType(*returnType);
    if (*returnType != ArithmeticType::VOID && !body->hasReturnStatement(state))
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType);
  body->check(stateCopy);
}

void correctness(const AST& ast) {
  State state;
  for (auto& elem : ast.elems) {
    elem->check(state);
  }
}

void Expression::check(State& s) const {
  getType(s);
}

Assignment::Assignment(CodeLoc l, string v, unique_ptr<Expression> e) : Statement(l), variable(v), expr(std::move(e)) {
  INFO << "Assigning to variable \"" + v + "\"";
}

void Assignment::check(State& state) const {
  if (auto varType = state.getType(variable))
    varType->visit(
        [&](ArithmeticType t) {
          auto exprType = expr->getType(state);
          codeLoc.check(t == exprType, "Assigning expression of type \""s + getName(exprType)
              + "\" to variable of type \"" + getName(t) + "\"");
        },
        [&](const FunctionType&) {
          codeLoc.error("Trying to assign to a function type");
        }
    );
  else
    codeLoc.error("Variable not found: \"" + variable + "\"");
}
