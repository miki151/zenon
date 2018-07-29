#pragma once

#include "stdafx.h"
#include "hashing.h"
#include "util.h"
#include "token.h"

struct Expression;

struct IdentifierInfo {
  struct IdentifierPart {
    string name;
    vector<IdentifierInfo> templateArguments;
    string toString() const;
    bool operator == (const IdentifierPart&) const;
    HASH_ALL(name, templateArguments)
  };
  explicit IdentifierInfo(string name, CodeLoc);
  IdentifierInfo(IdentifierPart, CodeLoc);
  IdentifierInfo();
  IdentifierInfo getWithoutFirstPart() const;
  optional<string> asBasicIdentifier() const;
  vector<IdentifierPart> parts;
  enum PointerType {
    MUTABLE,
    CONST
  };
  struct ArraySize {
    shared_ptr<Expression> expr;
  };
  vector<variant<PointerType, ArraySize>> pointerOrArray;
  CodeLoc codeLoc;
  string toString() const;
  bool operator == (const IdentifierInfo&) const;
  HASH_ALL(parts)
};
