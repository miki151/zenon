#include "code_loc.h"
#include "debug.h"

CodeLoc::CodeLoc(int l, int c) : line(l), column(c) {}

CodeLoc::CodeLoc() {}

void CodeLoc::error(const string& e) const {
  ERROR << "Line " << line << ", column " << column << ": " << e;
}

void CodeLoc::check(bool b, const string& e) const {
  if (!b)
    error(e);
}
