#pragma once

#include "stdafx.h"

struct Type;
using SType = shared_ptr<Type>;

class IdentifierType {
  public:
  IdentifierType(string name);
  IdentifierType(SType);
  IdentifierType replace(SType from, SType to) const;
  struct Part {
    variant<string, SType> name;
  };
  IdentifierType getWithoutFirstPart() const;
  vector<Part> parts;
  string prettyString() const;

  private:
  IdentifierType() {}
};
