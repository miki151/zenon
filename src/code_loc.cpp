#include "code_loc.h"
#include "debug.h"

CodeLoc::CodeLoc(string f, int l, int c) : file(f), line(l), column(c) {}

CodeLoc::CodeLoc() {}

void CodeLoc::error(const string& e) const {
  ERROR << file << ": " << "Line " << line << ", column " << column << ": " << e;
  exit(-1);
}

void CodeLoc::check(bool b, const string& e) const {
  if (!b)
    error(e);
}
