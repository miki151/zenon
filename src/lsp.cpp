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

void output(const string& s) {
  cout << "Content-Length: " << s.size() << "\r\n\r\n" << s << std::flush;
  cerr << "Writing: [" << s << "]" << endl;
}

optional<string> findValue(const string& s, const string& key) {
  cerr << "Searching for [" << key << "]" << endl;
  auto ind = s.find("\"" + key + "\"");
  if (ind == string::npos)
    return none;
  ind += key.size() + 3;
  bool withQuotes = false;
  if (s[ind] == '\"') {
    ++ind;
    withQuotes = true;
  }
  auto end = ind;
  if (withQuotes)
    while (1) {
      cerr << "With quotes " << end << endl;
      end = s.find("\"", end + 1);
      CHECK(end != string::npos);
      if (s[end - 1] != '\\')
        break;
    }
  else {
    cerr << "No quotes " << end << endl;
    end = s.find(",", ind + 1);
    if (end == string::npos)
      end = s.find("}", ind + 1);
    CHECK(end != string::npos);
  }
  cerr << ind << " " << end << endl;
  auto ret = s.substr(ind, end - ind);
  cerr << "Value [" + key + "] = [" + ret + "]" << endl;
  return ret;
}

static char buf[1000000];

string readData(int cnt, ostream& outCopy) {
  std::cin.read(buf, cnt);
  buf[cnt] = 0;
  outCopy << buf;
  outCopy.flush();
  return buf;
}

void eat(istream& i, char c, ostream& outCopy) {
  CHECK(i.peek() == c) << "Expected \"" << c << "\"";
  i.ignore(1);
  outCopy << c;
  outCopy.flush();
}

void eat(istream& i, const string& s, ostream& outCopy) {
  for (char c : s)
    eat(i, c, outCopy);
}

bool readHeaderLine(int& v, ostream& outCopy) {
  if (std::cin.peek() == '\r') {
    eat(std::cin, "\r\n", outCopy);
    return false;
  }
  eat(std::cin, "Content-", outCopy);
  if (std::cin.peek() == 'L') {
    eat(std::cin, "Length: ", outCopy);
    std::cin >> v;
    outCopy << v;
  } else {
    // We assume 'Content-Type: '
    eat(std::cin, "Type: ", outCopy);
    string type;
    while (std::cin.peek() != '\r')
      type += std::cin.get();
  }
  // Assume good delimeters
  eat(std::cin, "\r\n", outCopy);
  return true;
}

int readHeader(ostream& outCopy) {
  int v;
  while (readHeaderLine(v, outCopy));
  return v;
}

string readInput(ostream& outCopy) {
  int cnt = readHeader(outCopy);
  cerr << "count: " << cnt << endl;
  string s = readData(cnt, outCopy);
  cerr << "Read: [" << s << "]" << endl;
  return s;
}


string getHeader(const string& id) {
  return "{"
     "\"id\":" + id  + ","
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

string diagnosticsToJson(const string& path, const string& messages) {
  return "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"diagnostics\":["
      + messages + "],\"uri\":\"file://" + path + "\"  }}";
}

string toJson(const CodeLoc& l) {
  return "{\"character\":" + to_string(l.column) + ",\"line\":" + to_string(l.line) + "}";
}

void replace(string& subject, const string& search, const string& replace) {
  size_t pos = 0;
  while((pos = subject.find(search, pos)) != std::string::npos) {
   subject.replace(pos, search.length(), replace);
   pos += replace.length();
  }
}

string escape(string text) {
  replace(text, "\"", "\\\"");
  replace(text, "\n", "\\n");
  return text;
}

static string deEscape(string contents) {
  string ret;
  auto is = [&](int& index, const string& c) {
    if (index + c.size() <= contents.size() && contents.substr(index, c.size()) == c) {
      index += c.size();
      return true;
    } else
      return false;
  };
  int i = 0;
  while (i < contents.size()) {
    if (is(i, "\\\\"))
      ret += "\\";
    else if (is(i, "\\n"))
      ret += "\n";
    else if (is(i, "\\\""))
      ret += "\"";
    else {
      ret.push_back(contents[i]);
      ++i;
    }
  }
  return ret;
}

void printDiagnostics(const vector<string>& importDirs, const string& text, const string& path) {
  cerr << "Compiling ["  << text << "]" << endl;
  auto tokens = lex(text, CodeLoc(path, 0, 0), "end of file");
  string res;
  bool wasAdded = false;
  auto add = [&](string msg, CodeLoc begin) {
    if (begin.file != path)
      return;
    auto end = begin;
    end.column = begin.column + 5;
    if (wasAdded)
      res += ",";
    wasAdded = true;
    res += "{\"message\":\"" + escape(msg) + "\", \"range\":{\"start\":" + toJson(begin) + ",\"end\":" + toJson(end) + "}, \"severity\":1}";
  };
  if (!tokens) {
    cerr << "Lexing error" << endl;
    add(tokens.get_error().error, tokens.get_error().loc);
  } else {
    cerr << "Lexed " << tokens->getSize() << " tokens" << endl;
    auto ast = parse(*tokens);
    if (!ast) {
      cerr << "Parsing error" << endl;
      add(ast.get_error().error, ast.get_error().loc);
    } else {
      cerr << "Parsed " << ast->elems.size() << " top level statements" << endl;
      TypeRegistry typeRegistry;
      auto primaryContext = createPrimaryContext(&typeRegistry);
      ImportCache importCache;
      ASTCache astCache;
      for (auto& elem : ast->elems)
        if (auto res2 = elem->registerTypes(primaryContext, &typeRegistry, astCache, importDirs); !res2) {
          add(res2.get_error().error, res2.get_error().loc);
          output(diagnosticsToJson(path, res));
          return;
        }
      auto context = primaryContext.getChild();
      auto imported = correctness(fs::system_complete(path), *ast, context, primaryContext, importCache, false);
      if (!imported) {
        cerr << "Type error" << endl;
        add(imported.get_error().error, imported.get_error().loc);
      }
    }
  }
  output(diagnosticsToJson(path, res));
}

void startLsp(const vector<string>& importDirs) {
  ofstream log("/home/michal/lsp.log");
  cerr.rdbuf(log.rdbuf());
  cerr << "Starting log" << endl;
  for (auto dir : importDirs)
    cerr << "Dir " << dir << endl;
  ofstream outCopy("/home/michal/lsp.out");
  while (1) {
    auto input = readInput(outCopy);
    auto method = findValue(input, "method");
    if (method)
      cerr << "Got method: " << *method << endl;
    if (method == "initialize"s) {
      auto id = *findValue(input, "id");
      output(getHeader(id));
    } else
    if (method == "initialized"s) {
      cerr << "Initialized!" << endl;
    } else
    if (method == "textDocument/didOpen"s || method == "textDocument/didChange"s) {
      auto path = *findValue(input, "uri");
      CHECK(path.substr(0, 7) == "file://");
      path = path.substr(7);
      cerr << "Opened " << path << endl;
      printDiagnostics(importDirs, deEscape(*findValue(input, "text")), path);
    }
  }
}
