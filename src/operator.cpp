#include "operator.h"
#include "type.h"
#include "code_loc.h"
#include "ast.h"

const static vector<pair<string, Operator>> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  // if a binary op has the same symbol as unary it needs to come before it.
  {"++", Operator::INCREMENT},
  {"+", Operator::PLUS},
  {"+", Operator::PLUS_UNARY},
  {"+=", Operator::INCREMENT_BY},
  {"*", Operator::MULTIPLY},
  {"*", Operator::POINTER_DEREFERENCE},
  {"*=", Operator::MULTIPLY_BY},
  {"/", Operator::DIVIDE},
  {"/=", Operator::DIVIDE_BY},
  {"--", Operator::DECREMENT},
  {"-", Operator::MINUS},
  {"%", Operator::MODULO},
  {"-", Operator::MINUS_UNARY},
  {"-=", Operator::DECREMENT_BY},
  {"=", Operator::ASSIGNMENT},
  {".", Operator::MEMBER_ACCESS},
  {"->", Operator::POINTER_MEMBER_ACCESS},
  {"&", Operator::GET_ADDRESS},
  {"[]", Operator::SUBSCRIPT},
  {"!", Operator::LOGICAL_NOT},
  {"&&", Operator::LOGICAL_AND},
  {"||", Operator::LOGICAL_OR},
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
    case Operator::INCREMENT_BY:
    case Operator::DECREMENT_BY:
    case Operator::MULTIPLY_BY:
    case Operator::DIVIDE_BY:
      return 1;
    case Operator::LOGICAL_OR:
      return 2;
    case Operator::LOGICAL_AND:
      return 3;
    case Operator::EQUALS:
      return 4;
    case Operator::LOGICAL_NOT:
      return 5;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      return 6;
    case Operator::PLUS:
    case Operator::PLUS_UNARY:
    case Operator::MINUS:
    case Operator::MINUS_UNARY:
      return 7;
    case Operator::MODULO:
    case Operator::DIVIDE:
    case Operator::MULTIPLY:
      return 8;
    case Operator::DECREMENT:
      return 9;
    case Operator::INCREMENT:
      return 10;
    case Operator::POINTER_DEREFERENCE:
      return 11;
    case Operator::GET_ADDRESS:
      return 12;
    case Operator::SUBSCRIPT:
      return 13;
    case Operator::MEMBER_ACCESS:
    case Operator::POINTER_MEMBER_ACCESS:
      return 14;
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
    case Operator::SUBSCRIPT:
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULO:
    case Operator::EQUALS:
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::INCREMENT_BY:
    case Operator::DECREMENT_BY:
    case Operator::MULTIPLY_BY:
    case Operator::DIVIDE_BY:
    case Operator::ASSIGNMENT:
    case Operator::MEMBER_ACCESS:
    case Operator::POINTER_MEMBER_ACCESS:
    case Operator::LOGICAL_AND:
    case Operator::LOGICAL_OR:
      return false;
    case Operator::POINTER_DEREFERENCE:
    case Operator::PLUS_UNARY:
    case Operator::INCREMENT:
    case Operator::DECREMENT:
    case Operator::LOGICAL_NOT:
    case Operator::MINUS_UNARY:
    case Operator::GET_ADDRESS:
      return true;
  }
}


bool canOverload(Operator op, int numArgs) {
  return canOverload(op) && ((numArgs == 1 && isUnary(op)) || (numArgs == 2 && !isUnary(op)));
}

bool canOverload(Operator op) {
  switch (op) {
    case Operator::SUBSCRIPT:
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULO:
    case Operator::EQUALS:
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::INCREMENT_BY:
    case Operator::DECREMENT_BY:
    case Operator::MULTIPLY_BY:
    case Operator::DIVIDE_BY:
    case Operator::POINTER_DEREFERENCE:
    case Operator::PLUS_UNARY:
    case Operator::INCREMENT:
    case Operator::DECREMENT:
    case Operator::LOGICAL_NOT:
    case Operator::MINUS_UNARY:
      return true;
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
    case Operator::MULTIPLY:
      return Operator::POINTER_DEREFERENCE;
    case Operator::LOGICAL_NOT:
    case Operator::GET_ADDRESS:
    case Operator::INCREMENT:
    case Operator::DECREMENT:
      return op;
    default:
      return none;
  }
}

static string getError(Operator op, const vector<CompileTimeValue>& args) {
  if (args.size() == 2)
    return "Bad arguments to operator " + quote(getString(op)) + ": " +
        quote(toString(args[0])) + ", " + quote(toString(args[1]));
  else
    return "Bad argument to operator " + quote(getString(op)) + ": " + quote(toString(args[0]));
}

template <typename Arg1, typename Arg2, typename Operation>
WithError<CompileTimeValue> tryTypes(Operator op, const vector<CompileTimeValue>& args, Operation operation) {
  if (auto value1 = args[0].getValueMaybe<Arg1>())
    if (auto value2 = args[1].getValueMaybe<Arg2>())
      return CompileTimeValue(operation(*value1, *value2));
  return getError(op, args);
}

template <typename Arg, typename Operation>
WithError<CompileTimeValue> tryType(Operator op, const vector<CompileTimeValue>& args, Operation operation) {
  if (auto value1 = args[0].getValueMaybe<Arg>())
    return CompileTimeValue(operation(*value1));
  return getError(op, args);
}

template <typename Operation>
WithError<CompileTimeValue> tryArithmetic(Operator op, const vector<CompileTimeValue>& args, Operation operation) {
  if (auto res = tryTypes<int, int>(op, args, operation))
    return res;
  if (auto res = tryTypes<int, double>(op, args, operation))
    return res;
  if (auto res = tryTypes<double, int>(op, args, operation))
    return res;
  return tryTypes<double, double>(op, args, operation);
}

template <typename Operation>
WithError<CompileTimeValue> tryArithmeticUnary(Operator op, const vector<CompileTimeValue>& args, Operation operation) {
  if (auto res = tryType<int>(op, args, operation))
    return res;
  return tryType<double>(op, args, operation);
}

WithError<CompileTimeValue> eval(Operator op, vector<CompileTimeValue> args) {
  if (args.size() == 2 && isUnary(op))
    return "Too many arguments to unary operator"s;
  if (args.size() == 1 && !isUnary(op))
    return "Too few arguments to binary operator"s;
  switch (op) {
    case Operator::ASSIGNMENT:
    case Operator::INCREMENT_BY:
    case Operator::DECREMENT_BY:
    case Operator::MULTIPLY_BY:
    case Operator::DIVIDE_BY:
      return "Compile-time expression has value " + quote(ArithmeticType::VOID->getName());
    case Operator::LOGICAL_OR:
      return tryTypes<bool, bool>(op, args, [](bool b1, bool b2){ return b1 || b2; });
    case Operator::LOGICAL_AND:
      return tryTypes<bool, bool>(op, args, [](bool b1, bool b2){ return b1 && b2; });
    case Operator::EQUALS:
      if (args[0].index() == args[1].index())
        return CompileTimeValue(args[0] == args[1]);
      else
        return getError(op, args);
    case Operator::LOGICAL_NOT:
      return tryType<bool>(op, args, [](bool b){ return !b; });
    case Operator::LESS_THAN:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 < v2; });
    case Operator::MORE_THAN:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 > v2; });
    case Operator::PLUS:
      if (auto res = tryArithmetic(op, args, [](auto v1, auto v2) { return v1 + v2; }))
        return res;
      return tryTypes<string, string>(op, args, [](auto v1, auto v2) { return v1 + v2; });
    case Operator::PLUS_UNARY:
      return tryArithmeticUnary(op, args, [](auto v) { return v; });
    case Operator::MINUS:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 - v2; });
    case Operator::MINUS_UNARY:
      return tryArithmeticUnary(op, args, [](auto v) { return -v; });
    case Operator::MODULO:
      return tryTypes<int, int>(op, args, [](int v1, int v2){ return v1 % v2; });
    case Operator::DIVIDE:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 / v2; });
    case Operator::MULTIPLY:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 * v2; });
    case Operator::DECREMENT:
    case Operator::INCREMENT:
    case Operator::POINTER_DEREFERENCE:
    case Operator::GET_ADDRESS:
    case Operator::SUBSCRIPT:
    case Operator::MEMBER_ACCESS:
    case Operator::POINTER_MEMBER_ACCESS:
      return "Can't evaluate operation at compile-time " + quote(getString(op));
  }
}
