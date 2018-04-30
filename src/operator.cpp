#include "operator.h"
#include "type.h"
#include "code_loc.h"
#include "ast.h"

const static vector<pair<string, Operator>> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  // if a binary op has the same symbol as unary it needs to come before it.
  {"+", Operator::PLUS},
  {"+", Operator::PLUS_UNARY},
  {"*", Operator::MULTIPLY},
  {"*", Operator::POINTER_DEREFERENCE},
  {"-", Operator::MINUS},
  {"-", Operator::MINUS_UNARY},
  {"=", Operator::ASSIGNMENT},
  {".", Operator::MEMBER_ACCESS},
  {"&", Operator::GET_ADDRESS},
  {"[]", Operator::SUBSCRIPT}
};

optional<Operator> getOperator(const string& s) {
  for (auto& elem : operators)
    if (elem.first == s)
      return elem.second;
  return none;
}

vector<string> getAllOperators() {
  vector<string> ret;
  for (auto& elem : operators)
    ret.push_back(elem.first);
  return ret;
}

const char* getString(Operator op) {
  for (auto& elem : operators)
    if (elem.second == op)
      return elem.first.c_str();
  FATAL << "Unrecognized operator " << (int) op;
  return nullptr;
}

int getPrecedence(Operator op) {
  switch (op) {
    case Operator::ASSIGNMENT:
      return 1;
    case Operator::EQUALS:
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      return 2;
    case Operator::PLUS:
    case Operator::PLUS_UNARY:
    case Operator::MINUS:
    case Operator::MINUS_UNARY:
      return 3;
    case Operator::MULTIPLY:
      return 4;
    case Operator::POINTER_DEREFERENCE:
      return 5;
    case Operator::GET_ADDRESS:
      return 6;
    case Operator::SUBSCRIPT:
      return 7;
    case Operator::MEMBER_ACCESS:
      return 8;
  }
}

bool isRightAssociative(Operator op) {
  switch (op) {
    case Operator::ASSIGNMENT:
      return true;
    default:
      return false;
  }
}

bool canOverload(Operator op, int numArguments) {
  switch (op) {
    case Operator::SUBSCRIPT:
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::MULTIPLY:
      return numArguments == 1;
    case Operator::POINTER_DEREFERENCE:
    case Operator::PLUS_UNARY:
    case Operator::MINUS_UNARY:
      return numArguments == 0;
    default:
      return false;
  }
}

optional<Operator> getUnary(Operator op) {
  switch (op) {
    case Operator::PLUS:
      return Operator::PLUS_UNARY;
    case Operator::MINUS:
      return Operator::MINUS_UNARY;
    case Operator::GET_ADDRESS:
      return op;
    case Operator::MULTIPLY:
      return Operator::POINTER_DEREFERENCE;
    default:
      return none;
  }
}

SType getUnaryOperationResult(CodeLoc codeLoc, Operator op, SType right) {
  if (auto fun = right->getContext().getOperatorType(op))
    return fun->retVal;
  else
    codeLoc.error("Can't apply unary operator: " + quote(getString(op)) + " to type: " + quote(right->getName()));
}

SType getOperationResult(CodeLoc codeLoc, Operator op, const Context& context, Expression& leftExpr, Expression& rightExpr) {
  auto left = leftExpr.getType(context);
  auto right = [&]() { return rightExpr.getType(context); };
  switch (op) {
    case Operator::MEMBER_ACCESS: {
      if (auto rightType = rightExpr.getDotOperatorType(&leftExpr, context)) {
        if (!left.dynamicCast<ReferenceType>())
          rightType = rightType->getUnderlying();
        return rightType.get();
      } else
        codeLoc.error("Bad use of operator " + quote("."));
    }
    default:
      if (auto fun = left->getContext().getOperatorType(op)) {
        return instantiateFunction(*fun, codeLoc, {}, {right()}, {codeLoc}).get().retVal;
      } else
        codeLoc.error("Can't apply operator: " + quote(getString(op)) + " to types: " +
            quote(left->getName()) + " and " + quote(right()->getName()));
  }
}
