#pragma once

#include "stdafx.h"

class AST;
class Context;

extern string codegen(const AST&, const Context& context, const string& codegenInclude, bool includeLineNumbers);
