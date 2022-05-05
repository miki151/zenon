#include "ast_cache.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"

const string& ASTCache::getContent(const string& path) {
  if (!content.count(path))
    content[path] = readFromFile(path.c_str())->value;
  return content.at(path);
}

void ASTCache::setContent(const string& path, string s) {
  content[path] = std::move(s);
  astMap.erase(path);
}

WithErrorLine<AST*> ASTCache::getAST(const string& path) {
  if (!astMap.count(path)) {
    const string& program = getContent(path);
    //INFO << "Parsing:\n\n" << program;
    auto tokens = TRY(lex(program, CodeLoc(path, 0, 0), "end of file"));
    auto ast = TRY(parse(tokens));
    astMap.insert(make_pair(std::move(path), std::move(ast)));
  }
  return &astMap.at(path);
}

