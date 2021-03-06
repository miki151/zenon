#pragma once

#include "stdafx.h"

class AST;
class Context;
class TypeRegistry;

extern string codegen(const AST&, TypeRegistry& registry, const Context& context, const string& codegenInclude,
    bool includeLineNumbers);
