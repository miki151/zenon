#include <fstream>
#include "debug.h"

#include "token.h"
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "codegen.h"
#include "correctness.h"
#include "reader.h"
#include "ProgramOptions.h"

using namespace std;

static po::parser getCommandLineFlags() {
  po::parser flags;
  flags["help"].abbreviation('h').description("Print help");
  flags["o"].type(po::string).description("Binary output path.");
  flags["cpp"].type(po::string).fallback("g++").description("C++ compiler path (default: g++).");
  flags["c"].description("Do not link binary.");
  flags[""].type(po::string).description("Path to the input program.");
  return flags;
}

static int compileCpp(string command, const string& program, const string& output, bool link) {
  command += " -xc++ - -std=c++14 ";
  if (!link)
    command += "-c ";
  command += "-o " + output;
  FILE* p = popen(command.c_str(), "w");
  fwrite(program.c_str(), 1, program.size(), p);
  string out;
  return pclose(p) / 256;
}

int main(int argc, char* argv[]) {
  po::parser flags = getCommandLineFlags();
  if (!flags.parseArgs(argc, argv))
    return -1;
  if (flags["help"].was_set() || argc == 1) {
    std::cout << flags << endl;
    return 0;
  }
  FatalLog.addOutput(DebugOutput::crash());
  FatalLog.addOutput(DebugOutput::toStream(std::cerr));
  ofstream log("log.out");
  InfoLog.addOutput(DebugOutput::toStream(log));
  ErrorLog.addOutput(DebugOutput::exitProgram(1));
  ErrorLog.addOutput(DebugOutput::toStream(std::cerr));
  for (auto pathElem : flags[""]) {
    auto path = pathElem.string;
    string program = readFromFile(path.c_str(), none);
    INFO << "Parsing:\n\n" << program;
    auto tokens = lex(program, path);
    auto ast = parse(tokens);
    correctness(ast);
    auto cppCode = codegen(ast);
    log << cppCode;
    if (flags["o"].was_set()) {
      if (compileCpp(flags["cpp"].get().string, cppCode, flags["o"].get().string, !flags["c"].was_set())) {
        cerr << "C++ compilation failed, which is a Zenon bug :(\n\n";
        cerr << cppCode << endl;
        return 2;
      }
    } else
      cout << cppCode << endl;
  }
}
