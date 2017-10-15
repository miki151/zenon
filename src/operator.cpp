#include "operator.h"

const static unordered_map<string, Operator> operators {
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  {"+", Operator::PLUS},
  {"-", Operator::MINUS},
  {"=", Operator::ASSIGNMENT},
  {".", Operator::MEMBER_ACCESS}
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
    case Operator::MEMBER_ACCESS:
      return 4;
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
      return true;
    default:
      return false;
  }
}
