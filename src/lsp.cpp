#include <iostream>
#include <fstream>
#include <cassert>
#include <optional>
#include <regex>

#include "lsp.h"
#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "type_registry.h"
#include "import_cache.h"
#include "ast_cache.h"
#include "nlohmann/json.hpp"
#include "language_index.h"

using nlohmann::json;

void output(const string& s) {
  cout << "Content-Length: " << s.size() << "\r\n\r\n" << s << std::flush;
  cerr << "Writing: [" << s << "]" << endl;
}

void eat(istream& i, char c) {
  CHECK(i.peek() == c) << "Expected \"" << c << " got " << i.peek() << "\"";
  i.ignore(1);
}

void eat(istream& i, const string& s) {
  for (char c : s)
    eat(i, c);
}

bool readHeaderLine(int& v) {
  if (std::cin.peek() == '\r') {
    eat(std::cin, "\r\n");
    return false;
  }
  eat(std::cin, "Content-");
  if (std::cin.peek() == 'L') {
    eat(std::cin, "Length: ");
    std::cin >> v;
  } else {
    // We assume 'Content-Type: '
    eat(std::cin, "Type: ");
    string type;
    while (std::cin.peek() != '\r')
      type += std::cin.get();
  }
  // Assume good delimeters
  eat(std::cin, "\r\n");
  return true;
}

int readHeader() {
  int v;
  while (readHeaderLine(v));
  return v;
}

string readInput() {
  int cnt = readHeader();
  cerr << "count: " << cnt << endl;
  static char buf[1000000];
  std::cin.read(buf, cnt);
  buf[cnt] = 0;
  return buf;
}


string getHeader(const int& id) {
  return "{"
     "\"id\":" + to_string(id)  + ","
     "\"jsonrpc\":\"2.0\","
     "\"result\":{"
         "\"capabilities\":{"
              "\"codeActionProvider\":false,"
              "\"colorProvider\":false,"
              "\"definitionProvider\":false,"
              //"\"signatureHelpProvider\":true,"
              "\"documentFormattingProvider\":false,"
              "\"documentHighlightProvider\":true,"
              "\"documentRangeFormattingProvider\":false,"
              "\"documentSymbolProvider\":false,"
              "\"foldingRangeProvider\":false,"
              "\"hoverProvider\":false,"
              "\"implementationProvider\":false,"
              "\"referencesProvider\":false,"
              "\"renameProvider\":false,"
              "\"textDocumentSync\":1,"
              "\"typeDefinitionProvider\":false,"
              "\"workspace\":{"
                  "\"workspaceFolders\":{"
                      "\"changeNotifications\":false,"
                      "\"supported\":false"
                  "}"
              "},"
              "\"workspaceSymbolProvider\":false"
          "}"
      "}"
  "}";
}

json diagnosticsMessage(const string& path, json messages) {
  return json {
      {"jsonrpc", "2.0"},
      {"method", "textDocument/publishDiagnostics"},
      {"params", {
          {"diagnostics", std::move(messages)},
          {"uri", "file://" + path}
      }}
  };
}

json toJson(const CodeLoc& l) {
  return json { 
      {"start", {
          {"character", l.column},
          {"line", l.line}
      }},
      {"end", {
          {"character", l.column + 5},
          {"line", l.line}
      }},
  };
}

void replace(string& subject, const string& search, const string& replace) {
  size_t pos = 0;
  while((pos = subject.find(search, pos)) != std::string::npos) {
   subject.replace(pos, search.length(), replace);
   pos += replace.length();
  }
}

void compile(const Context& primaryContext, ImportCache& importCache, ASTCache& astCache, LanguageIndex* languageIndex,
    const vector<string>& importDirs, string path) {
  path = fs::canonical(path);
  json diagnostics = json::array();
  auto add = [&](string msg, CodeLoc begin) {
    cerr << begin.toString() << ": " << msg <<endl;
    if (begin.file != path)
      return;
    diagnostics.push_back(json {
      {"message", msg},
      {"range", toJson(begin)},
      {"severity", 1}
    });
  };
  if (auto ast = astCache.getAST(path)) {
    for (auto& elem : (*ast)->elems)
      if (auto res2 = elem->registerTypes(primaryContext, primaryContext.typeRegistry, astCache, importDirs); !res2) {
        add(res2.get_error().error, res2.get_error().loc);
        output(diagnosticsMessage(path, std::move(diagnostics)).dump());
        return;
      }
    auto context = primaryContext.getChild(true);
    if (auto res2 = correctness(path, **ast, context, primaryContext, importCache, false); !res2)
      add(res2.get_error().error, res2.get_error().loc);
  } else
    add(ast.get_error().error, ast.get_error().loc);
  output(diagnosticsMessage(path, diagnostics).dump());
}

json getLocation(CodeLoc elem) {
  return json {
      {"uri", "file://" + elem.file},
      {"range", {
          {"start", {
              {"character", elem.column},
              {"line", elem.line}
          }}
      }}
  };
}

json getResult(int id, json result) {
  return json {
    {"id", id},
    {"jsonrpc", "2.0"},
    {"result", std::move(result)}
  };
}

CodeLoc getCodeLoc(const json& j) {
  return CodeLoc(
      j["textDocument"]["uri"].get<string>().substr(7),
      j["position"]["line"].get<int>(),
      j["position"]["character"].get<int>()
  );
}

void startLsp(const vector<string>& importDirs) {
  map<string, string> allContent;
  set<string> compiled;
  LanguageIndex languageIndex;
  ASTCache astCache;
  TypeRegistry typeRegistry;
  ImportCache importCache;
  auto primaryContext = createPrimaryContext(&typeRegistry, &languageIndex);
  auto clearAST = [&] {
    languageIndex = LanguageIndex{};
    importCache = ImportCache{};
    typeRegistry = TypeRegistry{};
    astCache = ASTCache{};
    primaryContext = createPrimaryContext(&typeRegistry, &languageIndex);
    for (auto& elem : allContent)
      astCache.setContent(elem.first, elem.second);
    compiled.clear();
  };
  for (auto dir : importDirs)
    cerr << "Dir " << dir << endl;
  while (1) {
    auto message = readInput();
    json input;
    try {
      input = json::parse(message);
    } catch (nlohmann::detail::parse_error e) {
      cerr << e.what() << endl;
      continue;
    }
    if (!input.contains("method"))
      continue;
    auto method = input["method"].get<string>();
    if (method == "initialize") {
      auto id = input["id"].get<int>();
      output(getHeader(id));
    } else
    if (method == "initialized") {
      cerr << "Initialized!" << endl;
    } else
    if (method == "textDocument/didOpen") {
      auto path = input["params"]["textDocument"]["uri"].get<string>().substr(7);
      compile(primaryContext, importCache, astCache, &languageIndex, importDirs, path);
      compiled.insert(path);
    } else
    if (method == "textDocument/didChange") {
      auto path = input["params"]["textDocument"]["uri"].get<string>().substr(7);
      auto content = input["params"]["contentChanges"][0]["text"].get<string>();
      allContent[path] = content;
      clearAST();
      compile(primaryContext, importCache, astCache, &languageIndex, importDirs, path);
      compiled.insert(path);
    } else 
    if (method == "textDocument/definition") {
      auto codeLoc = getCodeLoc(input["params"]);
      if (compiled.insert(codeLoc.file).second)
        compile(primaryContext, importCache, astCache, &languageIndex, importDirs, codeLoc.file);
      int id = input["id"].get<int>();
      auto results = json::array();
      for (auto& elem : languageIndex.getTarget(codeLoc))
        results.push_back(getLocation(elem));
      output(getResult(id, std::move(results)).dump());
    } else
    if (method == "textDocument/references") {
      auto codeLoc = getCodeLoc(input["params"]);
      if (compiled.insert(codeLoc.file).second)
        compile(primaryContext, importCache, astCache, &languageIndex, importDirs, codeLoc.file);
      int id = input["id"].get<int>();
      auto results = json::array();
      for (auto& elem : languageIndex.getReferences(codeLoc))
        results.push_back(getLocation(elem));
      output(getResult(id, std::move(results)).dump());
    } else
    if (method == "textDocument/signatureHelp") {
      auto codeLoc = getCodeLoc(input["params"]);
      if (compiled.insert(codeLoc.file).second)
        compile(primaryContext, importCache, astCache, &languageIndex, importDirs, codeLoc.file);
      int id = input["id"].get<int>();
      auto results = json::array();
      for (auto& elem : languageIndex.getSignature(codeLoc))
        results.push_back(json {
            {"label", elem}
        });
      output(getResult(id, {{"signatures", std::move(results)}}).dump());
    }
  }
}
