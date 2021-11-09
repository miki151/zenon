#pragma once

#include "ast.h"

class ASTCache {
  public:
  WithErrorLine<AST*> getAST(const string& path);

  private:
  map<string, AST> astMap;
};
