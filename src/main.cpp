#include <fstream>
#include "debug.h"

#include "token.h"
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "codegen.h"
#include "correctness.h"
#include "reader.h"

using namespace std;

int main(int argc, char* argv[]) {
  FatalLog.addOutput(DebugOutput::crash());
  FatalLog.addOutput(DebugOutput::toStream(std::cerr));
  InfoLog.addOutput(DebugOutput::toStream(std::cerr));
  ErrorLog.addOutput(DebugOutput::exitProgram());
  ErrorLog.addOutput(DebugOutput::toStream(std::cerr));
  if (argc < 2)
    return -1;
  auto path = argv[1];
  string program = readFromFile(path, none);
  INFO << "Parsing:\n\n" << program;
  auto tokens = lex(program, path);
/*  for (auto& token : tokens)
    token.visit(
        [](const Number& n) {
          INFO << "Number (" << n.value << ")";
        },
        [](const Identifier& i) {
          INFO << "Identifier (" << i.name << ")";
        },
        [](const Keyword& k) {
          INFO << "Keyword " << k.value;
        },
        [](const Comment& c) {
          INFO << "Comment (" << c.value << ")";
        }
    );*/
  auto ast = parse(tokens);
  correctness(ast);
  cerr << codegen(ast);
  cout << codegen(ast);
}
