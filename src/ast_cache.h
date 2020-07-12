#pragma once

#include "ast.h"

class ASTCache {
  public:
  WithErrorLine<AST*> getAST(const string& path);
  bool hasAST(const string& path) const;

  private:
  map<string, AST> astMap;
};
