#include "operator.h"

const static unordered_map<string, Operator> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  {"+", Operator::PLUS},
  {"*", Operator::MULTIPLY},
  {"-", Operator::MINUS},
  {"=", Operator::ASSIGNMENT},
  {".", Operator::MEMBER_ACCESS},
  {"&", Operator::GET_ADDRESS}
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
    case Operator::MEMBER_ACCESS:
      return 6;
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
