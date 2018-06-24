#pragma once
#include <string>
#include "token.h"

extern Tokens lex(const string& input, CodeLoc initialPos, const string& eofTokenValue);
