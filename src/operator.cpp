#include "operator.h"
#include "type.h"
#include "code_loc.h"
#include "ast.h"

const static unordered_map<string, Operator> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  {"+", Operator::PLUS},
  {"*", Operator::MULTIPLY},
  {"-", Operator::MINUS},
  {"=", Operator::ASSIGNMENT},
  {".", Operator::MEMBER_ACCESS},
  {"&", Operator::GET_ADDRESS},
  {"[]", Operator::SUBSCRIPT}
};

optional<Operator> getOperator(const string& s) {
  if (operators.count(s))
    return operators.at(s);
  else
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
    case Operator::GET_ADDRESS:
      return 5;
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

bool isUnary(Operator op) {
  switch (op) {
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::GET_ADDRESS:
    case Operator::MULTIPLY:
      return true;
    default:
      return false;
  }
}

Type getUnaryOperationResult(CodeLoc codeLoc, Operator op, const Type& right) {
  auto underlying = getUnderlying(right);
  CHECK(isUnary(op));
  switch (op) {
    case Operator::MULTIPLY:
      if (auto pointer = underlying.getReferenceMaybe<PointerType>())
        return ReferenceType(*pointer->underlying);
      else
        codeLoc.error("Can't apply dereference operator to type " + quote(getName(right)));
      break;
    case Operator::GET_ADDRESS:
      if (auto reference = right.getReferenceMaybe<ReferenceType>())
        return PointerType(*reference->underlying);
      else
        codeLoc.error("Can't apply address-of operator to type " + quote(getName(right)));
      break;
    case Operator::PLUS:
    case Operator::MINUS:
      codeLoc.check(underlying == ArithmeticType::INT, "Expected type: " + quote("int") + ", got: " + quote(getName(right)));
      break;
    default:
      break;
  }
  return right;
}

static optional<Type> getOperationResult(Operator op, const Type& underlyingOperands) {
  switch (op) {
    case Operator::MULTIPLY:
    case Operator::PLUS:
    case Operator::MINUS:
      if (underlyingOperands == ArithmeticType::INT)
        return Type(ArithmeticType::INT);
      if (underlyingOperands == ArithmeticType::STRING)
        return Type(ArithmeticType::STRING);
      return none;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      if (underlyingOperands == ArithmeticType::INT)
        return Type(ArithmeticType::BOOL);
      return none;
    case Operator::EQUALS:
      if (underlyingOperands == ArithmeticType::INT || underlyingOperands == ArithmeticType::BOOL
           || underlyingOperands == ArithmeticType::STRING || underlyingOperands == ArithmeticType::CHAR)
        return Type(ArithmeticType::BOOL);
      return none;
    default:
      return none;
  }
}

static State getStringTypeContext() {
  State ret;
  ret.addFunction("size"s, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT, {}, {}));
  ret.addFunction(Operator::SUBSCRIPT, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::CHAR,
      {{"index", ArithmeticType::INT}}, {}));
  return ret;
}

static State getTypeContext(CodeLoc codeLoc, const Type& type, const char* op) {
  return type.visit(
      [&](const StructType& t) {
        return t.getContext();
      },
      [&](ArithmeticType t) {
        if (t == ArithmeticType::STRING)
          return getStringTypeContext();
        codeLoc.error("Type " + quote(getName(t)) + " doesn't support operator " + quote(op));
        return State();
      },
      [&] (const auto&) {
        codeLoc.error("Type " + quote(getName(getUnderlying(type))) + " doesn't support operator " + quote(op));
        return State();
      }
  );
}

Type getOperationResult(CodeLoc codeLoc, Operator op, const State& state, Expression& leftExpr, Expression& rightExpr) {
  auto left = leftExpr.getType(state);
  auto right = [&]() { return rightExpr.getType(state); };
  switch (op) {
    case Operator::MEMBER_ACCESS: {
      auto context = getTypeContext(codeLoc, getUnderlying(left), ".");
      if (auto rightType = rightExpr.getDotOperatorType(context, state)) {
        if (!left.contains<ReferenceType>())
          rightType = getUnderlying(*rightType);
        return *rightType;
      } else
        codeLoc.error("Bad use of operator " + quote("."));
      return {};
    }
    case Operator::SUBSCRIPT: {
      auto context = getTypeContext(codeLoc, getUnderlying(left), "[]");
      if (auto t = context.getOperatorType(Operator::SUBSCRIPT)) {
        codeLoc.check(getUnderlying(right()) == *t->params.at(0).type,
            "Expected expression of type " + quote(getName(*t->params.at(0).type)) + " inside operator " + quote("[]"));
        return *t->retVal;
      }
      codeLoc.error("Type " + quote(getName(left)) + " doesn't support operator " + quote("[]"));
      return {};
    }
    case Operator::ASSIGNMENT:
      if (getUnderlying(left) == getUnderlying(right()) && left.contains<ReferenceType>())
        return left;
      else {
        codeLoc.error("Can't assign " + quote(getName(right())) + " to " + quote(getName(left)));
        return {};
      }
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::MULTIPLY:
    case Operator::PLUS:
    case Operator::EQUALS:
    case Operator::MINUS: {
      auto rightType = right();
      auto operand = getUnderlying(left);
      if (operand == getUnderlying(rightType))
        if (auto res = getOperationResult(op, operand))
          return *res;
      codeLoc.error("Unsupported operation: " + quote(getName(left)) + " " + getString(op)
          + " " + quote(getName(rightType)));
      return {};
    }
    case Operator::GET_ADDRESS:
      codeLoc.error("Address-of is not a binary operator");
      return {};
  }
}
