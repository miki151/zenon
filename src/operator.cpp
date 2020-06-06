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
  {"&", Operator::GET_ADDRESS},
  {"[]", Operator::SUBSCRIPT},
  {"!", Operator::LOGICAL_NOT},
  {"&&", Operator::LOGICAL_AND},
  {"||", Operator::LOGICAL_OR},
  {"?", Operator::MAYBE},
  {"??", Operator::VALUE_OR},
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
    case Operator::MAYBE:
      return 2;
    case Operator::LOGICAL_OR:
      return 3;
    case Operator::LOGICAL_AND:
      return 4;
    case Operator::NOT_EQUAL:
    case Operator::EQUALS:
      return 5;
    case Operator::LOGICAL_NOT:
      return 6;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
      return 7;
    case Operator::PLUS:
    case Operator::PLUS_UNARY:
    case Operator::MINUS:
    case Operator::MINUS_UNARY:
      return 8;
    case Operator::MODULO:
    case Operator::DIVIDE:
    case Operator::MULTIPLY:
      return 9;
    case Operator::VALUE_OR:
      return 10;
    case Operator::DECREMENT:
      return 11;
    case Operator::INCREMENT:
      return 12;
    case Operator::POINTER_DEREFERENCE:
      return 13;
    case Operator::GET_ADDRESS:
      return 14;
    case Operator::SUBSCRIPT:
      return 15;
  }
}

bool isRightAssociative(Operator op) {
  switch (op) {
    case Operator::MAYBE:
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
    case Operator::LOGICAL_AND:
    case Operator::LOGICAL_OR:
    case Operator::VALUE_OR:
    case Operator::MAYBE:
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
    case Operator::POINTER_DEREFERENCE:
    case Operator::PLUS_UNARY:
    case Operator::INCREMENT:
    case Operator::DECREMENT:
    case Operator::LOGICAL_NOT:
    case Operator::MINUS_UNARY:
    case Operator::VALUE_OR:
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

template <typename Arg1>
static optional<Arg1> tryReference(const CompileTimeValue& arg) {
  if (auto value = arg.value.getValueMaybe<Arg1>())
    return value;
  if (auto ref = arg.value.getReferenceMaybe<CompileTimeValue::ReferenceValue>())
    if (auto value = ref->value->value.getValueMaybe<Arg1>())
      return value;
  return none;
}

template <typename Arg1, typename Arg2, typename Operation>
nullable<SCompileTimeValue> tryTypes(const vector<SCompileTimeValue>& args, Operation operation) {
  auto value1 = tryReference<Arg1>(*args[0]);
  auto value2 = tryReference<Arg2>(*args[1]);
  if (value1 && value2)
    return CompileTimeValue::get(operation(*value1, *value2));
  return nullptr;
}

template <typename Arg, typename Operation>
nullable<SCompileTimeValue> tryType(const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto value1 = tryReference<Arg>(*args[0]))
    return CompileTimeValue::get(operation(*value1));
  return nullptr;
}

template <typename Operation>
nullable<SCompileTimeValue> tryArithmetic(const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto res = tryTypes<int, int>(args, operation))
    return res;
  if (auto res = tryTypes<int, double>(args, operation))
    return res;
  if (auto res = tryTypes<double, int>(args, operation))
    return res;
  return tryTypes<double, double>(args, operation);
}

template <typename Operation>
nullable<SCompileTimeValue> tryArithmeticUnary(const vector<SCompileTimeValue>& args, Operation operation) {
  return tryType<int>(args, operation);
}

template <typename Operation>
nullable<SCompileTimeValue> tryReferenceUnary(const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto ref = args[0]->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>()) {
    if (auto res = tryArithmeticUnary({ref->value}, operation)) {
      ref->value = res.get();
      return res;
    }
  }
  return nullptr;
}

template <typename Operation>
nullable<SCompileTimeValue> tryReferenceBinary(const vector<SCompileTimeValue>& args, Operation operation) {
  if (auto ref = args[0]->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>()) {
    if (auto res = tryArithmetic({ref->value, args[1]}, operation)) {
      ref->value = res.get();
      return res;
    }
  }
  return nullptr;
}

static nullable<SCompileTimeValue> evalNonTemplate(Operator op, vector<SCompileTimeValue> args) {
  CHECK((args.size() == 2 && !isUnary(op)) || (args.size() == 1 && isUnary(op)));
  switch (op) {
    case Operator::ASSIGNMENT:
      return tryReferenceBinary(args, [](auto value1, auto value2) { return value2; });
    case Operator::MULTIPLY_BY:
      return tryReferenceBinary(args, [](auto value1, auto value2) { return value1 * value2; });
    case Operator::DIVIDE_BY:
      return tryReferenceBinary(args, [](auto value1, auto value2) { return value1 / value2; });
    case Operator::DECREMENT_BY:
      return tryReferenceBinary(args, [](auto value1, auto value2) { return value1 - value2; });
    case Operator::INCREMENT_BY:
      return tryReferenceBinary(args, [](auto value1, auto value2) { return value1 + value2; });
    case Operator::DECREMENT:
      return tryReferenceUnary(args, [](auto value) { return value - 1; });
    case Operator::INCREMENT:
      return tryReferenceUnary(args, [](auto value) { return value + 1; });
    case Operator::LOGICAL_OR:
      return tryTypes<bool, bool>(args, [](bool b1, bool b2) { return b1 || b2; });
    case Operator::LOGICAL_AND:
      return tryTypes<bool, bool>(args, [](bool b1, bool b2) { return b1 && b2; });
    case Operator::LOGICAL_NOT:
      return tryType<bool>(args, [](bool b) { return !b; });
    case Operator::LESS_THAN: {
      return tryArithmetic(args, [](auto v1, auto v2) { return v1 < v2; });
    }
    case Operator::MORE_THAN:
      return tryArithmetic(args, [](auto v1, auto v2) { return v1 > v2; });
    case Operator::PLUS:
      if (auto res = tryArithmetic(args, [](auto v1, auto v2) { return v1 + v2; }))
        return res;
      return tryTypes<string, string>(args, [](auto v1, auto v2) { return v1 + v2; });
    case Operator::PLUS_UNARY:
      return tryArithmeticUnary(args, [](auto v) { return v; });
    case Operator::MINUS:
      return tryArithmetic(args, [](auto v1, auto v2) { return v1 - v2; });
    case Operator::MINUS_UNARY:
      return tryArithmeticUnary(args, [](auto v) { return -v; });
    case Operator::MODULO:
      return tryTypes<int, int>(args, [](int v1, int v2) { return v1 % v2; });
    case Operator::DIVIDE:
      return tryArithmetic(args, [](auto v1, auto v2) { return v1 / v2; });
    case Operator::MULTIPLY:
      return tryArithmetic(args, [](auto v1, auto v2) { return v1 * v2; });
    case Operator::POINTER_DEREFERENCE:
    case Operator::GET_ADDRESS:
    case Operator::SUBSCRIPT:
    case Operator::VALUE_OR:
    case Operator::MAYBE:
      return nullptr;
    case Operator::NOT_EQUAL:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
    case Operator::EQUALS:
      FATAL << "This operator should have been rewritten";
      fail();
  }
}

static SCompileTimeValue getExampleValue(SType type) {
  if (BuiltinType::INT == type)
    return CompileTimeValue::get(0);
  if (BuiltinType::BOOL == type)
    return CompileTimeValue::get(false);
  if (BuiltinType::DOUBLE == type)
    return CompileTimeValue::get(0.0);
  if (BuiltinType::STRING == type)
    return CompileTimeValue::get(""s);
  if (BuiltinType::CHAR == type)
    return CompileTimeValue::get('a');
  if (auto ref = type.dynamicCast<MutableReferenceType>())
    return CompileTimeValue::getReference(getExampleValue(ref->removeReference()));
  //if (BuiltinType::VOID == type)
  fail();
}

WithEvalError<SType> eval(Operator op, vector<SType> args1) {
  if (op == Operator::EQUALS && args1.size() == 2) {
    auto result = args1[0]->getMangledName() && args1[1]->getMangledName()
        ? CompileTimeValue::get(args1[0] == args1[1])
        : CompileTimeValue::get(CompileTimeValue::TemplateExpression{op, args1, BuiltinType::BOOL});
    return SType(std::move(result));
  }
  vector<SCompileTimeValue> args;
  for (auto& arg1 : args1) {
    if (auto arg = arg1.dynamicCast<CompileTimeValue>())
      args.push_back(arg);
    else
      return EvalError::noEval();
  }
  auto argsOrig = args;
  bool wasTemplate = false;
  for (auto& arg : args)
    if (!arg->getMangledName()) {
      wasTemplate = true;
      arg = getExampleValue(arg->getType());
    }
  auto ret = evalNonTemplate(op, args);
  if (!ret)
    return EvalError::noEval();
  if (wasTemplate)
    ret = CompileTimeValue::get(CompileTimeValue::TemplateExpression{op, args1, ret->getType()});
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
    case Operator::GET_ADDRESS:
      return "op_get_address";
    case Operator::VALUE_OR:
      return "op_value_or";
    default:
      return nullptr;
  }
}

string getPrettyString(Operator op, vector<SType> args) {
  if (isUnary(op))
    return getString(op) + args[0]->getName();
  else
    return args[0]->getName() + getString(op) + args[1]->getName();
}
