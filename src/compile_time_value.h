#pragma once

#include "util.h"

using CompileTimeValue = variant<int, bool, double, char, string>;

extern string toString(const CompileTimeValue&);
struct Type;
using SType = shared_ptr<Type>;

extern SType getType(const CompileTimeValue&);
