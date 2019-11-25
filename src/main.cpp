#include <fstream>
#include "debug.h"

#include "token.h"
#include "lexer.h"
#include "parser.h"
#include "ast.h"
#include "codegen.h"
#include "reader.h"
#include "ProgramOptions.h"
#include "lsp.h"
#include "type_registry.h"

auto installDir = INSTALL_DIR;

static po::parser getCommandLineFlags() {
  po::parser flags;
  flags["help"].abbreviation('h').description("Print help");
  flags["lsp"].description("Start language server");
  flags["o"].type(po::string).description("Binary output path.");
  flags["cpp"].type(po::string).fallback("g++").description("C++ compiler path (default: g++).");
  flags["c"].description("Do not link binary.");
  flags["codegen"].type(po::string).description("Directory to output generated C++ code for the whole project.");
  flags["l"].type(po::string).multi().description("Linking flags.");
  flags[""].type(po::string).description("Path to the input program.");
  return flags;
}

static int compileCpp(string command, const string& program, const string& output) {
  command += " -xc++ - -std=c++17 -Wno-trigraphs -c -o " + output;
  cerr << command << endl;
  FILE* p = popen(command.c_str(), "w");
  fwrite(program.c_str(), 1, program.size(), p);
  string out;
  return pclose(p) / 256;
}

static int linkObjs(const string& cmd, const vector<string>& objs, const string& output, const vector<string>& flags) {
  auto command = cmd + " " + combine(objs, " ") + " -o " + output + " " + combine(flags, " ");
  return system(command.data());
}

void initLogging(ofstream& logFile) {
  FatalLog.addOutput(DebugOutput::crash());
  FatalLog.addOutput(DebugOutput::toStream(std::cerr));
  InfoLog.addOutput(DebugOutput::toStream(logFile));
  ErrorLog.addOutput(DebugOutput::exitProgram(1));
  ErrorLog.addOutput(DebugOutput::toStream(std::cerr));
}

[[noreturn]] static void exitWithError(CodeLoc loc, const string& e) {
  ErrorLog.get() << loc.toString() << ": " << e;
  exit(-1);
}

template <typename T>
T getOrCompileError(WithErrorLine<T> value) {
  if (value)
    return std::move(*value);
  else
    exitWithError(value.get_error().loc, value.get_error().error);
}

int main(int argc, char* argv[]) {
  po::parser flags = getCommandLineFlags();
  if (!flags.parseArgs(argc, argv))
    return -1;
  if (flags["help"].was_set() || argc == 1) {
    std::cout << flags << endl;
    return 0;
  }
  ofstream logFile("log.out");
  initLogging(logFile);
  if (flags["lsp"].was_set()) {
    startLsp(installDir);
    return 0;
  }
  optional<string> codegenAll;
  if (flags["codegen"].was_set())
    codegenAll = flags["codegen"].get().string;
  auto gccCmd = flags["cpp"].get().string;
  bool fullCompile = !flags["c"].was_set() || codegenAll;
  bool printCpp = !flags["o"].was_set() && !codegenAll;
  vector<string> linkFlags;
  for (int i = 0; i < flags["l"].count(); ++i)
    linkFlags.push_back("-l" + flags["l"].get(i).string);
  vector<ModuleInfo> toCompile;
  set<string> finished;
  for (auto pathElem : flags[""]) {
    std::error_code error;
    toCompile.push_back({fs::canonical(pathElem.string, error), false});
    if (error) {
      std::cerr << "Error opening " << pathElem.string << ": " << error.message() << std::endl;
      return -1;
    }
  }
  vector<string> objFiles;
  auto buildDir = ".build_cache";
  if (!fs::is_directory(buildDir))
    fs::create_directory(buildDir);
  while (!toCompile.empty()) {
    auto path = toCompile.begin()->path;
    bool builtInModule = toCompile.begin()->builtIn;
    cerr << "Compiling " << path << endl;
    toCompile.removeIndexPreserveOrder(0);
    finished.insert(path);
    auto program = readFromFile(path.c_str());
    if (!program)
      ErrorLog.get() << program.get_error();
    INFO << "Parsing:\n\n" << program->value;
    auto tokens = getOrCompileError(lex(program->value, CodeLoc(path, 0, 0), "end of file"));
    auto ast = getOrCompileError(parse(tokens));
    TypeRegistry typeRegistry;
    auto primaryContext = createPrimaryContext(&typeRegistry);
    auto context = Context::withParent(primaryContext);
    auto imported = getOrCompileError(correctness(path, ast, context, primaryContext, {installDir}, builtInModule));
    auto cppCode = codegen(ast, context, installDir + "/codegen_includes/"s, !printCpp);
    if (printCpp) {
      cout << cppCode << endl;
      return 0;
    } else
      logFile << cppCode;
    if (fullCompile)
      for (auto& import : imported)
        if (!finished.count(import.path)) {
          toCompile.push_back(import);
          finished.insert(import.path);
        }
    if (codegenAll)
      ofstream(*codegenAll + "/"s + to_string(std::hash<string>()(path)) + ".cpp") << cppCode;
    else {
      auto objFile = fullCompile
          ? buildDir + "/"s + to_string(std::hash<string>()(cppCode + gccCmd)) + ".znn.o"
          : flags["o"].get().string;
      if ((!fullCompile || !fs::exists(objFile)) && compileCpp(gccCmd, cppCode, objFile)) {
        cerr << "C++ compilation failed, which is a Zenon bug :(\n\n" << endl;
        //cerr << cppCode << endl;
        return 2;
      } else if (fullCompile)
        objFiles.push_back(objFile);
    }
  }
  if (!objFiles.empty())
    if (linkObjs(gccCmd, objFiles, flags["o"].get().string, linkFlags) != 0) {
      cerr << "Linking failed" << endl;
      return 2;
    }
}
