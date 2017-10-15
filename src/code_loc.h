#pragma once

#include "stdafx.h"

struct CodeLoc {
  CodeLoc(int l, int c);
  CodeLoc();
  void error(const string&) const;
  void check(bool, const string&) const;
  int line;
  int column;
};
