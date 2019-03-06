#pragma once
#include <string>
#include "token.h"

extern WithErrorLine<Tokens> lex(const string& input, CodeLoc initialPos, const string& eofTokenValue);
