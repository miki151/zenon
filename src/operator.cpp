#include "operator.h"
#include "type.h"
#include "code_loc.h"
#include "ast.h"

const static vector<pair<string, Operator>> operators {
  {"<=", Operator::LESS_OR_EQUAL},
  {">=", Operator::MORE_OR_EQUAL},
  {"<", Operator::LESS_THAN},
  {">", Operator::MORE_THAN},
  {"==", Operator::EQUALS},
  {"!=", Operator::NOT_EQUAL},
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
    case Operator::NOT_EQUAL:
    case Operator::EQUALS:
      return 4;
    case Operator::LOGICAL_NOT:
      return 5;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
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
    case Operator::NOT_EQUAL:
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
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

template <typename Arg1, typename Arg2, typename Operation>
nullable<SCompileTimeValue> tryTypes(Operator op, const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto value1 = args[0]->value.getValueMaybe<Arg1>())
    if (auto value2 = args[1]->value.getValueMaybe<Arg2>())
      return CompileTimeValue::get(operation(*value1, *value2));
  return nullptr;
}

template <typename Arg, typename Operation>
nullable<SCompileTimeValue> tryType(Operator op, const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto value1 = args[0]->value.getValueMaybe<Arg>())
    return CompileTimeValue::get(operation(*value1));
  return nullptr;
}

template <typename Operation>
nullable<SCompileTimeValue> tryArithmetic(Operator op, const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto res = tryTypes<int, int>(op, args, operation))
    return res;
  if (auto res = tryTypes<int, double>(op, args, operation))
    return res;
  if (auto res = tryTypes<double, int>(op, args, operation))
    return res;
  return tryTypes<double, double>(op, args, operation);
}

template <typename Operation>
nullable<SCompileTimeValue> tryArithmeticUnary(Operator op, const vector<SCompileTimeValue>& args, Operation operation) {
  return tryType<int>(op, args, operation);
}

static nullable<SCompileTimeValue> evalNonTemplate(Operator op, vector<SCompileTimeValue> args) {
  CHECK((args.size() == 2 && !isUnary(op)) || (args.size() == 1 && isUnary(op)));
  switch (op) {
    case Operator::ASSIGNMENT:
    case Operator::INCREMENT_BY:
    case Operator::DECREMENT_BY:
    case Operator::MULTIPLY_BY:
    case Operator::DIVIDE_BY:
      return nullptr;
    case Operator::LOGICAL_OR:
      return tryTypes<bool, bool>(op, args, [](bool b1, bool b2) { return b1 || b2; });
    case Operator::LOGICAL_AND:
      return tryTypes<bool, bool>(op, args, [](bool b1, bool b2) { return b1 && b2; });
    case Operator::EQUALS:
      return tryArithmetic(op, args, [](auto v1, auto v2) { return v1 == v2; });
      /*if (args[0].index() == args[1].index())
        return SCompileTimeValue(args[0] == args[1]);
      else
        return getError(op, args);*/
    case Operator::LOGICAL_NOT:
      return tryType<bool>(op, args, [](bool b) { return !b; });
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
      return tryTypes<int, int>(op, args, [](int v1, int v2) { return v1 % v2; });
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
      return nullptr;
    case Operator::POINTER_MEMBER_ACCESS:
    case Operator::NOT_EQUAL:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
      FATAL << "This operator should have been rewritten";
      fail();
  }
}

static SCompileTimeValue getExampleValue(SType type) {
  if (ArithmeticType::INT == type)
    return CompileTimeValue::get(0);
  if (ArithmeticType::BOOL == type)
    return CompileTimeValue::get(false);
  if (ArithmeticType::DOUBLE == type)
    return CompileTimeValue::get(0.0);
  if (ArithmeticType::STRING == type)
    return CompileTimeValue::get(""s);
  if (ArithmeticType::CHAR == type)
    return CompileTimeValue::get('a');
  //if (ArithmeticType::VOID == type)
  fail();
}

nullable<SType> eval(Operator op, vector<SType> args1) {
  vector<SCompileTimeValue> args;
  for (auto& arg1 : args1)
    if (auto arg = arg1.dynamicCast<CompileTimeValue>())
      args.push_back(arg);
    else
      return nullptr;
  bool wasTemplate = false;
  for (auto& arg : args)
    if (auto templateValue = arg->value.getReferenceMaybe<CompileTimeValue::TemplateValue>()) {
      arg = getExampleValue(templateValue->type);
      wasTemplate = true;
    }
  auto ret = evalNonTemplate(op, args);
  if (!ret)
    return nullptr;
  if (wasTemplate)
    ret = CompileTimeValue::get(CompileTimeValue::TemplateValue{ret->getType(), "Template derived value"});
  return (SType) ret.get();
}

const char* getCodegenName(Operator op) {
  switch (op) {
    case Operator::PLUS:
      return "op_plus";
    case Operator::SUBSCRIPT:
      return "op_subscript";
    case Operator::MINUS:
      return "op_minus";
    case Operator::MULTIPLY:
      return "op_multiply";
    case Operator::DIVIDE:
      return "op_divide";
    case Operator::MODULO:
      return "op_module";
    case Operator::EQUALS:
      return "op_equals";
    case Operator::LESS_THAN:
      return "op_less_than";
    case Operator::MORE_THAN:
      return "op_more_than";
    case Operator::INCREMENT_BY:
      return "op_increment_by";
    case Operator::DECREMENT_BY:
      return "op_increment_by";
    case Operator::MULTIPLY_BY:
      return "op_multiply_by";
    case Operator::DIVIDE_BY:
      return "op_divide_by";
    case Operator::POINTER_DEREFERENCE:
      return "op_pointer_deref";
    case Operator::PLUS_UNARY:
      return "op_plus_unary";
    case Operator::INCREMENT:
      return "op_increment";
    case Operator::DECREMENT:
      return "op_decrement";
    case Operator::LOGICAL_NOT:
      return "op_logical_not";
    case Operator::MINUS_UNARY:
      return "op_minus_unary";
    default:
      return nullptr;
  }
}
