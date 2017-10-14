#pragma once

#include "stdafx.h"
#include "hashing.h"
#include "util.h"
#include "token.h"

struct IdentifierInfo {
  IdentifierInfo(string name);
  IdentifierInfo(vector<string> namespaces, string name);
  static IdentifierInfo parseFrom(Token& idToken, Tokens&);
  vector<string> namespaces;
  string name;
  string toString() const;
  bool operator == (const IdentifierInfo&) const;
  HASH_ALL(name, namespaces)
  private:
  IdentifierInfo();
};
