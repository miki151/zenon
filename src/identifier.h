#pragma once

#include "stdafx.h"
#include "hashing.h"
#include "util.h"
#include "token.h"

struct IdentifierInfo {
  struct IdentifierPart {
    string name;
    vector<IdentifierInfo> templateParams;
    string toString() const;
    bool operator == (const IdentifierPart&) const;
    HASH_ALL(name, templateParams)
  };
  IdentifierInfo(string name);
  IdentifierInfo(IdentifierPart);
  //IdentifierInfo(vector<string> namespaces, string name, vector<IdentifierInfo> templateParams = {});
  static IdentifierInfo parseFrom(Tokens&);
  vector<IdentifierPart> parts;
  CodeLoc codeLoc;
  string toString() const;
  bool operator == (const IdentifierInfo&) const;
  HASH_ALL(parts)
  private:
  IdentifierInfo();
};
