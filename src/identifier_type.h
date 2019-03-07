#pragma once

#include "stdafx.h"
#include "code_loc.h"

struct Type;
using SType = shared_ptr<Type>;

class IdentifierType {
  public:
  IdentifierType(string name);
  IdentifierType(SType);
  IdentifierType replace(SType from, SType to, ErrorBuffer&) const;
  struct Part {
    variant<string, SType> name;
  };
  IdentifierType getWithoutFirstPart() const;
  vector<Part> parts;
  string prettyString() const;

  private:
  IdentifierType() {}
};
