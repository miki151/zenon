#include <iostream>
#include <fstream>
#include <cassert>
#include <optional>
#include <regex>

#include "lsp.h"
#include "ast.h"
#include "lexer.h"
#include "parser.h"

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

string readData(int cnt) {
  char buf[10000];
  std::cin.read(buf, cnt);
  buf[cnt] = 0;
  return buf;
}

void eat(istream& i, char c) {
  assert(i.peek() == c);
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
  string s = readData(cnt);
  cerr << "Read: [" << s << "]" << endl;
  return s;
}


string getHeader(const string& id) {
  return "{"
     "\"id\":\"" + id  + "\","
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

const auto diagnosticsPrefix = "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"diagnostics\":[";
const auto diagnosticsSuffix = "],\"uri\":\"file:///home/michal/zenon/input.znn\"  }}";

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

void printDiagnostics(const char* installDir, const string& text, const string& path) {
  cerr << "Compiling ["  << text << "]" << endl;
  auto tokens = lex(text, CodeLoc(path, 0, 0), "end of file");
  string res = diagnosticsPrefix;
  bool wasAdded = false;
  auto add = [&](string msg, CodeLoc begin) {
    auto end = begin;
    end.column = begin.column + 5;
    end.line = begin.line + 1;
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
      auto context = createNewContext();
      auto imported = correctness(*ast, context, {installDir}, false);
      if (!imported) {
        cerr << "Type error" << endl;
        add(imported.get_error().error, imported.get_error().loc);
      }
    }
  }
  res += diagnosticsSuffix;
  output(res);
}

void startLsp(const char* installDir) {
  ofstream out("/home/michal/lsp.out");
  cerr.rdbuf(out.rdbuf());
  cerr << "Starting log" << endl;
  auto welcome = readInput();
  auto id = *findValue(welcome, "id");
  output(getHeader(id));
  cerr << "Awaiting initialized" << endl;
  assert(findValue(readInput(), "method") == "initialized"s);
  cerr << "Got initialized" << endl;
  while (1) {
    auto input = readInput();
    auto method = findValue(input, "method");
    if (method == "textDocument/didOpen"s || method == "textDocument/didChange"s) {
      auto path = *findValue(input, "uri");
      cerr << "Opened " << path << endl;
      printDiagnostics(installDir, deEscape(*findValue(input, "text")), "file.znn");
    }
  }
}
