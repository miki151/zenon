#include "compile_time_value.h"

string toString(const CompileTimeValue& value) {
  return value.visit(
      [](int v) {  return to_string(v); },
      [](double v) {  return to_string(v); },
      [](bool v) {  return v ? "true" : "false"; },
      [](char v) {  return "\'"s + v + "\'"; },
      [](string v) {  return "\""s + v + "\"_lstr"; }
  );
}
