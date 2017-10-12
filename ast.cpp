#include "ast.h"
#include "type.h"

using namespace std;

Node::Node(CodeLoc l) : codeLoc(l) {}


BinaryExpression::BinaryExpression(CodeLoc loc, BinaryOperator o, unique_ptr<Expression> u1, unique_ptr<Expression> u2)
    : Expression(loc), op(o) {
  e1 = std::move(u1);
  e2 = std::move(u2);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<Expression> c, unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), cond(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, ArithmeticType t, string v) : Expression(l), type(t), value(v) {
  INFO << "Created constant \"" << v << "\" of type " << getName(Type(t));
}

Variable::Variable(CodeLoc l, string n) : Expression(l), name(n) {}

FunctionCall::FunctionCall(CodeLoc l, string n) : Expression(l), name(n) {
  INFO << "Function call " << n;
}

VariableDeclaration::VariableDeclaration(CodeLoc l, string t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  INFO << "Declared variable \"" << id << "\" of type \"" << t << "\"";
}

FunctionDefinition::FunctionDefinition(CodeLoc l, string r, string n) : Statement(l), returnType(r), name(n) {}

Type Constant::getType(const State&) {
  return type;
}

Type Variable::getType(const State& state) {
  if (auto ret = state.getTypeOfVariable(name))
    return *ret;
  else
    codeLoc.error("Undefined variable: " + name);
  return {};
}

Type FunctionCall::getType(const State& state) {
  if (auto type = state.getTypeOfVariable(name))
    return type->visit(
        [&](const FunctionType& t) {
          int numParams = (int) t.params.size();
          codeLoc.check(arguments.size() == numParams,
               "Expected " + to_string(numParams) + " arguments, got " + to_string(arguments.size()));
          for (int i = 0; i < numParams; ++i) {
            auto argType = arguments[i]->getType(state);
            auto& paramType = t.params[i];
            arguments[i]->codeLoc.check(canAssign(ReferenceType(*paramType.type), argType),
                "Function call argument " + to_string(i + 1) + " mismatch: expected \""s + getName(*paramType.type) +
                "\", got \""s + getName(argType) + "\"");
          }
          constructor = (t.target == FunctionType::CONSTRUCTOR);
          return *t.retVal;
        },
        [&](const auto&) -> Type {
          codeLoc.error("Trying to call a non-function type");
          return {};
        }
    );
  else
    codeLoc.error("Function not found: \"" + name + "\"");
  return {};
}

Type BinaryExpression::getType(const State& state) {
  auto leftType = e1->getType(state);
  auto rightType = e2->getType(state);
  return getOperationResult(e1->codeLoc, op, leftType, rightType);
}

void StatementBlock::check(State& state) {
  auto stateCopy = state;
  for (auto& s : elems) {
    s->check(stateCopy);
  }
}

void IfStatement::check(State& state) {
  auto condType = cond->getType(state);
  codeLoc.check(canConvert(condType, ArithmeticType::BOOL), "Expected a type convertible to bool inside if statement, got \""
      + getName(condType) + "\"");
  ifTrue->check(state);
  if (ifFalse)
    ifFalse->check(state);
}

void VariableDeclaration::check(State& state) {
  codeLoc.check(!state.getTypeFromString(identifier), "Variable \"" + identifier + "\" conflicts with an existing type");
  if (auto typeId = state.getTypeFromString(type)) {
    if (initExpr) {
      auto exprType = initExpr->getType(state);
      initExpr->codeLoc.check(canAssign(ReferenceType(*typeId), exprType), "Can't initialize variable of type \""
          + getName(*typeId) + "\" with value of type \"" + getName(exprType) + "\"");
    } else
      codeLoc.check(!requiresInitialization(*typeId), "Type \"" + getName(*typeId) + "\" requires initialization");
    state.setType(identifier, ReferenceType(*typeId));
  } else
    codeLoc.error("Type \"" + type + "\" not recognized");
}

void ReturnStatement::check(State& state) {
  if (!expr)
    codeLoc.check(state.getReturnType() && *state.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(state);
    codeLoc.check(canAssign(ReferenceType(*state.getReturnType()), returnType),
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

void FunctionDefinition::check(State& state) {
  State stateCopy = state;
  if (auto returnType = state.getTypeFromString(this->returnType)) {
    vector<FunctionType::Param> params;
    for (auto& p : parameters)
      if (auto paramType = state.getTypeFromString(p.type)) {
        stateCopy.setType(p.name, *paramType);
        params.push_back({p.name, *paramType});
      } else
        p.codeLoc.error("Unrecognized parameter type: \"" + p.type + "\"");
    auto type = FunctionType (FunctionType::TOP_LEVEL, *returnType, params );
    state.setType(name, type);
    stateCopy.setType(name, type);
    stateCopy.setReturnType(*returnType);
    if (*returnType != ArithmeticType::VOID && !body->hasReturnStatement(state))
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType);
  body->check(stateCopy);
}

void correctness(const AST& ast) {
  State state;
  state.addType("int", ArithmeticType::INT);
  state.addType("bool", ArithmeticType::BOOL);
  state.addType("void", ArithmeticType::VOID);
  for (auto& elem : ast.elems) {
    elem->check(state);
  }
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(State& state) {
  expr->getType(state);
}

StructDeclaration::StructDeclaration(CodeLoc l, string n) : Statement(l), name(n) {
}

void StructDeclaration::check(State& s) {
  codeLoc.check(!s.getTypeOfVariable(name), "Type \"" + name + "\" conflicts with an existing variable");
  codeLoc.check(!s.getTypeFromString(name), "Type \"" + name + "\" conflicts with an existing type");
  StructType type(name);
  for (auto& member : members) {
    INFO << "Struct member " << member.name << " " << member.type << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
    if (auto memberType = s.getTypeFromString(member.type))
      type.members.push_back({member.name, *memberType});
    else
      member.codeLoc.error("Type \"" + member.type + "\" not recognized");
  }
  s.addType(name, type);
  vector<FunctionType::Param> constructorParams;
  for (auto& member : type.members)
    constructorParams.push_back({member.name, *member.type});
  s.setType(name, FunctionType(FunctionType::CONSTRUCTOR, type, std::move(constructorParams)));
}

MemberAccessType::MemberAccessType(CodeLoc l, string n) : Expression(l), name(n) {}

Type MemberAccessType::getType(const State&) {
  return MemberAccess(name);
}

FunctionCallNamedArgs::FunctionCallNamedArgs(CodeLoc l, string n) : Expression(l), name(n) {}

Type FunctionCallNamedArgs::getType(const State& state) {
  if (auto type = state.getTypeOfVariable(name))
    return type->visit(
        [&](const FunctionType& t) {
          map<string, Type> toInitialize;
          set<string> initialized;
          map<string, int> paramIndex;
          int count = 0;
          for (auto& param : t.params) {
            toInitialize.insert({param.name, *param.type});
            paramIndex[param.name] = count++;
          }
          for (auto& elem : arguments) {
            elem.codeLoc.check(toInitialize.count(elem.name), "No parameter named \"" + elem.name
                + "\" in function \"" + name + "\"");
            elem.codeLoc.check(!initialized.count(elem.name), "Parameter \"" + elem.name + "\" listed more than once");
            auto exprType = elem.expr->getType(state);
            auto paramType = toInitialize.at(elem.name);
            elem.codeLoc.check(canAssign(ReferenceType(paramType), exprType), "Can't initialize parameter \"" + elem.name
                + "\" of type \"" + getName(paramType) + "\" with expression of type \"" + getName(exprType) + "\"");
            initialized.insert(elem.name);
          }
          vector<string> notInitialized;
          for (auto& elem : toInitialize)
            if (!initialized.count(elem.first))
              notInitialized.push_back("\"" + elem.first + "\"");
          codeLoc.check(notInitialized.empty(), "Function parameters: " + combine(notInitialized) + " were not initialized" );
          sort(arguments.begin(), arguments.end(),
              [&](const Argument& m1, const Argument& m2) { return paramIndex[m1.name] < paramIndex[m2.name]; });
          constructor = (t.target == FunctionType::CONSTRUCTOR);
          return *t.retVal;
        },
        [&](const auto&) -> Type {
          codeLoc.error("Trying to a non-function type");
          return {};
        }
    );
  else
    codeLoc.error("Function not found: \"" + name + "\"");
  return {};
}
