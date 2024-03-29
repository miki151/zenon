#pragma once

#include "stdafx.h"
#include "hashing.h"
#include "util.h"
#include "token.h"

struct Expression;

struct IdentifierInfo;
using TemplateParameterInfo = variant<shared_ptr<Expression>, IdentifierInfo>;

struct IdentifierInfo {
  struct IdentifierPart {
    string name;
    vector<TemplateParameterInfo> templateArguments;
    CodeLoc codeLoc;
    string toString() const;
    bool operator == (const IdentifierPart&) const;
    bool variadic = false;
    //HASH_ALL(name, templateArguments)
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
  struct Optional {
  };
  vector<variant<PointerType, ArraySize, Optional, IdentifierInfo>> typeOperator;
  CodeLoc codeLoc;
  shared_ptr<Expression> typeExpression;
  string prettyString() const;
  bool operator == (const IdentifierInfo&) const;
  //HASH_ALL(parts)
};
