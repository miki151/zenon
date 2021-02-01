#pragma once

#include "ast.h"

class ASTCache {
  public:
  WithErrorLine<AST*> getAST(const string& path);
  bool hasASTInUnit(const string& path) const;
  void clearUnitCache();

  private:
  map<string, AST> astMap;
  map<string, AST> astUnitMap;
};
