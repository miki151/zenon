#pragma once

#include "token.h"

struct AST;

extern WithErrorLine<AST> parse(Tokens);
