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
  {"*", Operator::MULTIPLY},
  {"*", Operator::POINTER_DEREFERENCE},
  {"--", Operator::DECREMENT},
  {"-", Operator::MINUS},
  {"-", Operator::MINUS_UNARY},
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

bool canOverload(Operator op, int numArguments) {
  switch (op) {
    case Operator::SUBSCRIPT:
    case Operator::PLUS:
    case Operator::MINUS:
    case Operator::MULTIPLY:
    case Operator::EQUALS:
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      return numArguments == 2;
    case Operator::POINTER_DEREFERENCE:
    case Operator::PLUS_UNARY:
    case Operator::INCREMENT:
    case Operator::DECREMENT:
    case Operator::LOGICAL_NOT:
    case Operator::MINUS_UNARY:
      return numArguments == 1;
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
