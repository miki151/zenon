#pragma once

#include "stdafx.h"

struct CodeLoc {
  CodeLoc(string file, int l, int c);
  CodeLoc();
  void error(const string&) const;
  void check(bool, const string&) const;
  string file;
  int line;
  int column;
};
