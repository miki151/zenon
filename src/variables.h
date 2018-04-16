#pragma once

#include "stdafx.h"
#include "util.h"

struct Type;
using SType = shared_ptr<Type>;

class Variables {
  public:
  nullable<SType> getType(const string&) const;
  void add(const string& ident, SType);
  const vector<string>& getNames() const;
  void replace(SType from, SType to);

  void merge(const Variables&);

  private:
  map<string, SType> vars;
  vector<string> varsList;
};
