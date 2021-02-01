#include "ast_cache.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"

WithErrorLine<AST*> ASTCache::getAST(const string& path) {
  if (!astMap.count(path)) {
    auto program = readFromFile(path.c_str());
    if (!program)
      ErrorLog.get() << program.get_error();
    INFO << "Parsing:\n\n" << program->value;
    auto tokens = TRY(lex(program->value, CodeLoc(path, 0, 0), "end of file"));
    auto ast = TRY(parse(tokens));
    astMap.insert(make_pair(std::move(path), std::move(ast)));
  }
  if (!astUnitMap.count(path))
    astUnitMap.insert(make_pair(std::move(path), astMap.at(path).clone()));
  return &astUnitMap.at(path);
}

bool ASTCache::hasASTInUnit(const std::string& path) const {
  return astUnitMap.count(path);
}

void ASTCache::clearUnitCache() {
  astUnitMap.clear();
}
