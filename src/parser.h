#pragma once

#include "token.h"

struct AST;
struct Statement;
extern WithErrorLine<AST> parse(Tokens);
extern WithErrorLine<unique_ptr<Statement>> parseStatement(Tokens&, bool topLevel);
