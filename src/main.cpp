#include "stdafx.h"
#include <fstream>
#include "debug.h"

#include "ast.h"
#include "codegen.h"
#include "ProgramOptions.h"
#include "lsp.h"
#include "type_registry.h"
#include "import_cache.h"
#include "ast_cache.h"

auto installDir = INSTALL_DIR;

static po::parser getCommandLineFlags() {
  po::parser flags;
  flags["help"].abbreviation('h').description("Print help");
  flags["lsp"].description("Start language server");
  flags["print"].description("Print C++ output.");
  flags["run"].description("Run the resulting program.");
  flags["o"].type(po::string).description("Binary output path.");
  flags["cpp"].type(po::string).fallback("g++").description("C++ compiler path (default: g++).");
  flags["linker_opts"].type(po::string).description("Additional linker options.");
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
  return pclose(p) / 256;
}

static int runProgram(string path) {
  FILE* p = popen(path.c_str(), "w");
  return pclose(p) / 256;
}

static int linkObjs(const string& cmd, const vector<string>& objs, const string& output, const vector<string>& flags) {
  auto command = cmd + " " + combine(objs, " ") + " -o " + output + " " + combine(flags, " ");
  cerr << command << endl;
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

static void checkCompileError(JustError<ErrorLoc> value) {
  if (!value)
    exitWithError(value.get_error().loc, value.get_error().error);
}

static string getCodegenAllFileName(string s) {
  for (auto& c : s)
    if (c == '/')
      c = '_';
  return s;
}

static string getBinaryName(const string& sourceFile) {
  if (endsWith(sourceFile, ".znn"))
    return sourceFile.substr(0, sourceFile.size() - 4);
  return sourceFile + ".bin";
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
  auto linkCmd = gccCmd;
  if (flags["linker_opts"].was_set())
    linkCmd += " " + flags["linker_opts"].get().string;
  bool fullCompile = !flags["c"].was_set() || codegenAll;
  bool printCpp = flags["print"].was_set() && !codegenAll;
  string binaryOutput = [&] {
    if (flags["o"].was_set())
      return flags["o"].get().string;
    else if (flags["run"].was_set())
      return string(tmpnam(nullptr));
    else
      return getBinaryName(flags[""].begin()->string);
  }();
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
  ImportCache importCache;
  TypeRegistry typeRegistry;
  ASTCache astCache;
  const auto primaryContext = createPrimaryContext(&typeRegistry);
  {
    auto initialAST = getOrCompileError(astCache.getAST(fs::canonical(toCompile.begin()->path)));
    for (auto& elem : initialAST->elems)
      checkCompileError(elem->registerTypes(primaryContext, &typeRegistry, astCache, {installDir}));
  }
  struct CodegenElem {
    AST* ast;
    string path;
  };
  vector<CodegenElem> codegenElems;
  while (!toCompile.empty()) {
    auto path = string(fs::canonical(toCompile.begin()->path));
    bool builtInModule = toCompile.begin()->builtIn;
    cerr << "Compiling " << path << endl;
    toCompile.removeIndexPreserveOrder(0);
    finished.insert(path);
    auto ast = getOrCompileError(astCache.getAST(path));
    if (builtInModule)
      importCache.setBuiltIn();
    auto context = primaryContext.getChild(true);
    auto imported = getOrCompileError(correctness(path, *ast, context, primaryContext, importCache, builtInModule));
    if (builtInModule)
      importCache.popBuiltIn();
    codegenElems.push_back(CodegenElem{ast, path});
    if (fullCompile)
      for (auto& import : imported)
        if (!finished.count(import.path)) {
          toCompile.push_back(import);
          finished.insert(import.path);
        }
  }
  for (auto& elem : codegenElems) {
    auto cppCode = codegen(*elem.ast, typeRegistry, installDir + "/codegen_includes/"s, !printCpp);
    if (cppCode.empty())
      continue;
    if (printCpp) {
      cout << cppCode << endl;
      return 0;
    }/* else
      logFile << cppCode;*/
    if (codegenAll)
      ofstream(*codegenAll + "/"s + getCodegenAllFileName(elem.path) + ".cpp") << cppCode;
    else {
      auto objFile = fullCompile
          ? buildDir + "/"s + to_string(std::hash<string>()(cppCode + gccCmd)) + ".znn.o"
          : binaryOutput;
      if ((!fullCompile || !fs::exists(objFile)) && compileCpp(gccCmd, cppCode, objFile)) {
        cerr << "C++ compilation failed, which is a Zenon bug :(\n\n" << endl;
        //cerr << cppCode << endl;
        return 2;
      } else if (fullCompile)
        objFiles.push_back(objFile);
    }
  }
  if (!objFiles.empty())
    if (linkObjs(linkCmd, objFiles, binaryOutput, linkFlags) != 0) {
      cerr << "Linking failed" << endl;
      return 2;
    }
  if (flags["run"].was_set()) {
    cout << "Process exited with value:" << runProgram(binaryOutput) << "\n";
    remove(binaryOutput.data());
  }
}
