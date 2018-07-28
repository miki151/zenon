#include "compile_time_value.h"
#include "type.h"

string toString(const CompileTimeValue& value) {
  return value.visit(
      [](int v) {  return to_string(v); },
      [](double v) {  return to_string(v); },
      [](bool v) {  return v ? "true" : "false"; },
      [](char v) {  return string(1, v); },
      [](string v) {  return v; }
  );
}

SType getType(const CompileTimeValue& value) {
  return value.visit(
      [](int v) {  return ArithmeticType::INT; },
      [](double v) {  return ArithmeticType::DOUBLE; },
      [](bool v) {  return ArithmeticType::BOOL; },
      [](char v) {  return ArithmeticType::CHAR; },
      [](string v) {  return ArithmeticType::STRING; }
  );
}
