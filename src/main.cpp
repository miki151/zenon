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
#include "language_index.h"
#include "nlohmann/json.hpp"
#include "reader.h"

using nlohmann::json;

#ifndef INSTALL_DIR
#define INSTALL_DIR "."
#endif

auto installDir = INSTALL_DIR;

static po::parser getCommandLineFlags() {
  po::parser flags;
  flags["help"].abbreviation('h').description("Print help");
  flags["lsp"].description("Start language server");
  flags["print"].description("Print C++ output.");
  flags["run"].description("Run the resulting program.");
  flags["output"].abbreviation('o').type(po::string).description("Binary output path.");
  flags["cpp"].type(po::string).description("C++ compiler path (default: g++).");
  flags["codegen"].type(po::string).description("Directory to output generated C++ code for the whole project.");
  flags["cpp_libs"].type(po::string).multi().description("Linking flags for the C++ compiler.");
  flags["cpp_includes"].type(po::string).multi().description("Include directories for the C++ compiler.");
  flags["zenon_libs"].type(po::string).multi().description("Import directories.");
  flags["optimize"].abbreviation('O').type(po::string).description("Pass the -O flag to the C++ compiler.");
  flags["debug"].abbreviation('g').description("Pass the -g flag to the C++ compiler.");  
  flags[""].type(po::string).description("Path to the input program.");
  return flags;
}

static int compileCpp(string command, const string& program, const string& output, const string& flags) {
  command += " -xc++ - -std=c++17 -fpermissive -Wno-unused-value -Wno-trigraphs -c " + flags + " -o   " + output;
  cerr << command << endl;
  FILE* p = popen(command.c_str(), "w");
  fwrite(program.c_str(), 1, program.size(), p);
  return pclose(p) / 256;
}

static int runProgram(string path) {
  FILE* p = popen(path.c_str(), "w");
  return pclose(p) / 256;
}

static int linkObjs(const string& cmd, const vector<string>& objs, const string& output, const string& flags) {
  auto command = cmd + " " + combine(objs, " ") + " -o " + output + " " + flags;
  cerr << command << endl;
  return system(command.data());
}

void initLogging() {
  FatalLog.addOutput(DebugOutput::crash());
  FatalLog.addOutput(DebugOutput::toStream(std::cerr));
  //InfoLog.addOutput(DebugOutput::toStream(logFile));
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

struct ZenonFlags {
  ZenonFlags(po::parser flags, const string& path) : flags(std::move(flags)) {
    if (auto content = readFromFile(path.data()))
      json = json::parse(content->value);
  }

  po::parser flags;
  json json;

  bool getBoolean(const string& name) {
    return flags[name].was_set() || (json.contains(name) && !!json[name].get<bool>());
  }

  optional<string> getString(const string& name) {
    if (flags[name].was_set())
      return flags[name].get().string;
    if (json.contains(name))
      return json[name].get<string>();
    return none;
  }

  vector<string> getVector(const string& name) {
    vector<string> ret;
    if (flags[name].was_set())
      for (int i = 0; i < flags[name].count(); ++i)
        ret.push_back(flags[name].get(i).string);
    else
      for (auto elem : json[name])
        ret.push_back(elem.get<string>());
    return ret;
  }

  optional<string> getMainFile() {
    if (flags[""].was_set())
      return flags[""].get().string;
    if (json.contains("input"))
      return json["input"].get<string>();
    return none;
  }
};

int main(int argc, char* argv[]) {
  po::parser flags2 = getCommandLineFlags();
  if (!flags2.parseArgs(argc, argv))
    return -1;
  if (flags2["help"].was_set()) {
    std::cout << flags2 << endl;
    return 0;
  }
  initLogging();
  auto lsp = flags2["lsp"].was_set();
  ZenonFlags flags(std::move(flags2), "zenon_flags.json");
  auto cppLibs = flags.getVector("cpp_libs");
  auto cppIncludes = flags.getVector("cpp_includes");
  vector<string> importDirs = {installDir};
  auto libs = flags.getVector("zenon_libs");
  while (!libs.empty()) {
    auto lib = libs.front();
    libs.removeIndex(0);
    importDirs.push_back(lib);
    ZenonFlags flagsRec(po::parser{}, lib + "/zenon_flags.json");
    cppLibs.append(flagsRec.getVector("cpp_libs"));
    cppIncludes.append(flagsRec.getVector("cpp_includes"));
    for (auto& lib : flagsRec.getVector("zenon_libs"))
      if (!libs.contains(lib))
        libs.push_back(lib);
  }
  if (lsp) {
    startLsp(importDirs);
    return 0;
  }
  string linkFlags = combine(cppLibs.transform([](auto elem) { return "-l" + elem; }), " ");
  string cppFlags = combine(cppIncludes.transform([](auto elem) { return "-I" + elem; }), " ");
  optional<string> codegenAll = flags.getString("codegen");
  auto gccCmd = flags.getString("cpp").value_or("g++");
  if (auto f = flags.getString("optimize"))
    gccCmd += " -O" + *f;
  if (flags.getBoolean("debug"))
    gccCmd += " -g";
  auto linkCmd = gccCmd;
  bool printCpp = flags.getBoolean("print");
  string input;
  if (auto s = flags.getMainFile())
    input = *s;
  else {
    std::cerr << "No output specified" << std::endl;
    return -1;
  }
  string binaryOutput = [&] {
    if (auto path = flags.getString("output"))
      return *path;
    else if (flags.getBoolean("run"))
      return string(tmpnam(nullptr));
    else
      return getBinaryName(input);
  }();
  vector<ModuleInfo> toCompile;
  set<string> finished;
  std::error_code error;
  toCompile.push_back({fs::canonical(input, error), false});
  if (error) {
    std::cerr << "Error opening " << input << ": " << error.message() << std::endl;
    return -1;
  }
  vector<string> objFiles;
  auto buildDir = ".build_cache";
  if (!fs::is_directory(buildDir))
    fs::create_directory(buildDir);
  ImportCache importCache;
  TypeRegistry typeRegistry;
  ASTCache astCache;
  LanguageIndex languageIndex;
  const auto primaryContext = createPrimaryContext(&typeRegistry, &languageIndex);
  {
    auto initialAST = getOrCompileError(astCache.getAST(fs::canonical(toCompile.begin()->path)));
    for (auto& elem : initialAST->elems)
      checkCompileError(elem->registerTypes(primaryContext, &typeRegistry, astCache, importDirs));
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
    }
    if (codegenAll)
      ofstream(*codegenAll + "/"s + getCodegenAllFileName(elem.path) + ".cpp") << cppCode;
    else {
      auto objFile = buildDir + "/"s + to_string(std::hash<string>()(cppCode + gccCmd)) + ".znn.o";
      if (!fs::exists(objFile) && !!compileCpp(gccCmd, cppCode, objFile, cppFlags)) {
        std::cerr << "C++ compilation failed:\n\n" <<   std::endl;
        return -1;
      }
      objFiles.push_back(objFile);
    }
  }
  if (!objFiles.empty())
    if (linkObjs(linkCmd, objFiles, binaryOutput, linkFlags) != 0) {
      cerr << "Linking failed" << endl;
      return 2;
    }
  if (flags.getBoolean("run")) {
    cout << "Process exited with value:" << runProgram(fs::canonical(binaryOutput)) << "\n";
    remove(binaryOutput.data());
  }
}
