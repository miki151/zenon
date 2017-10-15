#include "binary_operator.h"

const static unordered_map<string, BinaryOperator> operators {
  {"<", BinaryOperator::LESS_THAN},
  {">", BinaryOperator::MORE_THAN},
  {"==", BinaryOperator::EQUALS},
  {"+", BinaryOperator::PLUS},
  {"-", BinaryOperator::MINUS},
  {"=", BinaryOperator::ASSIGNMENT},
  {".", BinaryOperator::MEMBER_ACCESS}
};

optional<BinaryOperator> getBinaryOperator(const string& s) {
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

const char* getString(BinaryOperator op) {
  for (auto& elem : operators)
    if (elem.second == op)
      return elem.first.c_str();
  FATAL << "Unrecognized operator " << (int) op;
  return nullptr;
}

int getPrecedence(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::ASSIGNMENT:
      return 1;
    case BinaryOperator::EQUALS:
    case BinaryOperator::LESS_THAN:
    case BinaryOperator::MORE_THAN:
      return 2;
    case BinaryOperator::PLUS:
    case BinaryOperator::MINUS:
      return 3;
    case BinaryOperator::MEMBER_ACCESS:
      return 4;
  }
}

bool isRightAssociative(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::ASSIGNMENT:
      return true;
    default:
      return false;
  }
}
