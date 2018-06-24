#include "code_loc.h"
#include "debug.h"

CodeLoc::CodeLoc(string f, int l, int c) : file(f), line(l), column(c) {}

CodeLoc::CodeLoc() {}

void CodeLoc::error(const string& e) const {
  ERROR << file << ": " << "Line " << line + 1 << ", column " << column + 1 << ": " << e;
  exit(-1);
}

void CodeLoc::check(bool b, const string& e) const {
  if (!b)
    error(e);
}

void CodeLoc::checkNoError(optional<string> e) const {
  if (e)
    error(*e);
}

ErrorLoc CodeLoc::getError(string s) const {
  return ErrorLoc{*this, s};
}

CodeLoc CodeLoc::plus(int numLines, int numColumns) {
  return CodeLoc(file, line + numLines, column + numColumns);
}

void ErrorLoc::execute() const {
  loc.error(error);
}
