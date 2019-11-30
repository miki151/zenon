#pragma once

#include "code_loc.h"
#include "operator.h"

enum class LambdaCaptureType {
  IMPLICIT_COPY,
  MOVE,
  REFERENCE,
  COPY
};

struct LambdaCapture {
  string name;
  SType type;
};

struct LambdaCaptureInfo {
  struct Var {
    string name;
    CodeLoc codeLoc;
    LambdaCaptureType type;
  };
  vector<Var> captures;
  vector<LambdaCapture> implicitCaptures;
  optional<LambdaCaptureType> defaultCapture;
  const Var* find(const string& var) const;
};
