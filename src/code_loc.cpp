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
