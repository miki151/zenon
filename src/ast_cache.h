#pragma once

#include "ast.h"

class ASTCache {
  public:
  WithErrorLine<AST*> getAST(const string& path);
  void setContent(const string& path, string content);
  
  private:
  map<string, AST> astMap;
  const string& getContent(const string& path);
  map<string, string> content;
};
