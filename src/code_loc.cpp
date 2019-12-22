#include "code_loc.h"
#include "debug.h"

CodeLoc::CodeLoc(string f, int l, int c) : file(f), line(l), column(c) {}

CodeLoc::CodeLoc() {}

ErrorLoc CodeLoc::getError(string s) const {
  return ErrorLoc{*this, s};
}

CodeLoc CodeLoc::plus(int numLines, int numColumns) {
  return CodeLoc(file, line + numLines, column + numColumns);
}

string CodeLoc::toString() const {
  return file + ": " + "Line " + to_string(line + 1) + ", column "  + to_string(column + 1);
}

bool CodeLoc::operator <(const CodeLoc& l) const {
  return std::forward_as_tuple(file, line, column) < std::forward_as_tuple(l.file, l.line, l.column);
}

void merge(ErrorLocBuffer& errors, const ErrorBuffer& errors2, CodeLoc l) {
  for (auto& e : errors2)
    errors.push_back(l.getError(e));
}

EvalError EvalError::noEval() {
  return EvalError{false, ""};
}

EvalError EvalError::withError(string error) {
  return EvalError{true, error};
}
