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
  return &astMap.at(path);
}

