#pragma once

#include "stdafx.h"
#include "operator.h"

struct ConstructorId { bool operator < (ConstructorId) const { return false; }};
using FunctionName = variant<string, Operator, ConstructorId>;
