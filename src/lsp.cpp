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
              "\"documentFormattingProvider\":false,"
              "\"documentHighlightProvider\":true,"
              "\"documentRangeFormattingProvider\":false,"
              "\"documentSymbolProvider\":false,"
              "\"foldingRangeProvider\":true,"
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

void printDiagnostics(const vector<string>& importDirs, string path, string content) {
  ASTCache astCache;
  astCache.setContent(path, std::move(content));
  path = fs::canonical(path);
  TypeRegistry typeRegistry;
  json diagnostics = json::array();
  auto add = [&](string msg, CodeLoc begin) {
    if (begin.file != path)
      return;
    diagnostics.push_back(json {
      {"message", msg},
      {"range", toJson(begin)},
      {"severity", 1}
    });
  };
  const auto primaryContext = createPrimaryContext(&typeRegistry);
  if (auto ast = astCache.getAST(path)) {
    for (auto& elem : (*ast)->elems)
      if (auto res2 = elem->registerTypes(primaryContext, &typeRegistry, astCache, importDirs); !res2) {
        add(res2.get_error().error, res2.get_error().loc);
        output(diagnosticsMessage(path, std::move(diagnostics)).dump());
        return;
      }
    ImportCache importCache;
    auto context = primaryContext.getChild(true);
    if (auto res2 = correctness(path, **ast, context, primaryContext, importCache, false); !res2)
      add(res2.get_error().error, res2.get_error().loc);
  } else
    add(ast.get_error().error, ast.get_error().loc);
  output(diagnosticsMessage(path, diagnostics).dump());
}

void startLsp(const vector<string>& importDirs) {
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
      printDiagnostics(importDirs, path, input["params"]["textDocument"]["text"].get<string>());
    } else
    if (method == "textDocument/didChange") {
      auto path = input["params"]["textDocument"]["uri"].get<string>().substr(7);
      printDiagnostics(importDirs, path, input["params"]["contentChanges"][0]["text"].get<string>());
    }
  }
}
