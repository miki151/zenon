#include "operator.h"
#include "type.h"
#include "code_loc.h"
#include "ast.h"

const static vector<pair<string, Operator>> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  {"+", Operator::PLUS},
  {"*", Operator::MULTIPLY},
// order is important so * gets initially parsed as multiply and is replaced later if found to be a unary op.
  {"*", Operator::POINTER_DEREFERENCE},
  {"-", Operator::MINUS},
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
    case Operator::MINUS:
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

optional<Operator> getUnary(Operator op) {
  switch (op) {
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::GET_ADDRESS:
      return op;
    case Operator::MULTIPLY:
      return Operator::POINTER_DEREFERENCE;
    default:
      return none;
  }
}

SType getUnaryOperationResult(CodeLoc codeLoc, Operator op, SType right) {
  auto underlying = right->getUnderlying();
  switch (op) {
    case Operator::GET_ADDRESS:
    case Operator::POINTER_DEREFERENCE:
      if (auto fun = right->getContext().getOperatorType(op))
        return fun->retVal;
      else
        break;
    case Operator::PLUS:
    case Operator::MINUS:
      codeLoc.check(underlying == ArithmeticType::INT, "Expected type: " + quote("int") + ", got: " + quote(right->getName()));
      return right;
    default:
      break;
  }
  codeLoc.error("Can't apply unary operator: " + quote(getString(op)) + " to type: " + quote(right->getName()));
}

static nullable<SType> getOperationResult(Operator op, SType underlyingOperands) {
  switch (op) {
    case Operator::MULTIPLY:
    case Operator::PLUS:
    case Operator::MINUS:
      if (underlyingOperands == ArithmeticType::INT)
        return ArithmeticType::INT;
      if (underlyingOperands == ArithmeticType::STRING)
        return ArithmeticType::STRING;
      return nullptr;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      if (underlyingOperands == ArithmeticType::INT)
        return ArithmeticType::BOOL;
      return nullptr;
    case Operator::EQUALS:
      if (underlyingOperands == ArithmeticType::INT || underlyingOperands == ArithmeticType::BOOL
           || underlyingOperands == ArithmeticType::STRING || underlyingOperands == ArithmeticType::CHAR)
        return ArithmeticType::BOOL;
      return nullptr;
    default:
      return nullptr;
  }
}

SType getOperationResult(CodeLoc codeLoc, Operator op, const Context& context, Expression& leftExpr, Expression& rightExpr) {
  auto left = leftExpr.getType(context);
  auto right = [&]() { return rightExpr.getType(context); };
  switch (op) {
    case Operator::MEMBER_ACCESS: {
      if (auto rightType = rightExpr.getDotOperatorType(left->getContext(), context)) {
        if (!left.dynamicCast<ReferenceType>())
          rightType = rightType->getUnderlying();
        return rightType.get();
      } else
        codeLoc.error("Bad use of operator " + quote("."));
    }
    case Operator::SUBSCRIPT: {
      if (auto t = left->getContext().getOperatorType(Operator::SUBSCRIPT)) {
        codeLoc.check(right()->getUnderlying() == t->params.at(0).type,
            "Expected expression of type " + quote(t->params.at(0).type->getName()) + " inside operator " + quote("[]"));
        return t->retVal;
      } else
        codeLoc.error("Type " + quote(left->getUnderlying()->getName()) + " doesn't support operator " + quote("[]"));
    }
    case Operator::ASSIGNMENT:
      if (left->getUnderlying() == right()->getUnderlying() && left.dynamicCast<ReferenceType>())
        return left;
      else
        codeLoc.error("Can't assign " + quote(right()->getName()) + " to " + quote(left->getName()));
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::MULTIPLY:
    case Operator::PLUS:
    case Operator::EQUALS:
    case Operator::MINUS: {
      auto rightType = right();
      auto operand = left->getUnderlying();
      if (operand == rightType->getUnderlying())
        if (auto res = getOperationResult(op, operand))
          return res.get();
      codeLoc.error("Unsupported operation: " + quote(left->getName()) + " " + getString(op)
          + " " + quote(rightType->getName()));
    }
    case Operator::POINTER_DEREFERENCE:
      codeLoc.error("Pointer-dereference is not a binary operator");
    case Operator::GET_ADDRESS:
      codeLoc.error("Address-of is not a binary operator");
  }
}
