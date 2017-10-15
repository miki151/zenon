#pragma once

#include "stdafx.h"
#include "code_loc.h"

struct CompileError {
  CodeLoc codeLoc;
  string message;
};
