#pragma once

#include "stdafx.h"

class AST;

extern string codegen(const AST&, const string& codegenInclude);
