#include <iostream>
#include <string>
#include <regex>
#include <iterator>
#include <vector>
#include <algorithm>
#include "token.h"
#include "debug.h"
#include "binary_operator.h"
#include "util.h"

using namespace std;

static string getKeywords() {
  string keywords;
  for (auto& k : getAllKeywords()) {
    if (!isalpha(k[0]))
      keywords.append("\\");
    keywords.append(k);
    if (all_of(k.begin(), k.end(), [](char c) { return isalpha(c); }))
      keywords.append("\\b");
    keywords.append("|");
  }
  keywords.pop_back();
  return keywords;
}

static string getOperators() {
  string ret;
  auto ops = getAllOperators();
  sort(ops.begin(), ops.end(), [](const string& o1, const string& o2) { return o1.size() > o2.size(); });
  for (auto& k : ops) {
    ret.append("\\");
    ret.append(k);
    ret.append("|");
  }
  ret.pop_back();
  return ret;
}

Tokens lex(const string& input) {
  string idLetterFirst = "_a-zA-Z";
  string idLetter = idLetterFirst + "0-9";
  vector<pair<string, function<optional<Token>(const string&)>>> v {
      {"#.*\n", [](const string&) -> optional<Token> { return Token(Unknown{});}},
      {"//.*\n", [](const string&) -> optional<Token> { return none;}},
      {getKeywords(), [](const string& s) -> optional<Token> { return Token(getKeyword(s));}},
      {getOperators(), [](const string& s) -> optional<Token> {
          return Token(*getBinaryOperator(s));}},
      {"[0-9]+" , [](const string&) -> optional<Token> { return Token(Number{}); } } ,
      {"[" + idLetterFirst + "][" + idLetter + "]*" , [](const string&) -> optional<Token> { return Token(Identifier{}); }},
  };
  INFO << "Lexing expressions:";
  for (auto& elem : v)
    INFO << elem.first;
  vector<int> lines(input.size());
  vector<int> columns(input.size());
  int currentLine = 1;
  int currentColumn = 1;
  for (int i = 0; i < input.size(); ++i) {
    lines[i] = currentLine;
    columns[i] = currentColumn;
    if (input[i] == '\n') {
      ++currentLine;
      currentColumn = 1;
    } else
      ++currentColumn;
  }
  string reg;

  for(auto const& x : v)
    reg += "(" + x.first + ")|"; // parenthesize the submatches

  reg.pop_back();

  regex re(reg);
  auto words_begin = sregex_iterator(input.begin(), input.end(), re);
  auto words_end = sregex_iterator();

  vector<Token> ret;
  int lastPos = 0;
  for (auto it = words_begin; it != words_end; ++it) {
    for (int index = 0 ; index < it->size(); ++index)
      if (!it->str(index + 1).empty()) { // determine which submatch was matched
        string matched = it->str();
        string skipped = input.substr(lastPos, it->position() - lastPos);
        lastPos += matched.size() + skipped.size();
        auto codeLoc = CodeLoc(lines[lastPos - 1], columns[lastPos - 1]);
        if (!all_of(skipped.begin(), skipped.end(), [](char c) { return isspace(c); })) {
          ret.push_back(Unknown{});
          ret.back().value = skipped;
          ret.back().codeLoc = CodeLoc(lines[lastPos - 1 - skipped.size()], columns[lastPos - 1 - skipped.size()]);
          INFO << "Skipped " << quote(skipped);
        }
        if (auto token = v[index].second(matched)) {
          ret.push_back(*token);
          ret.back().codeLoc = codeLoc;
          ret.back().value = matched;
        }
        INFO << "Matched " << quote(matched) << " with rule " << index;
        break;
      }
  }
  return Tokens(ret);
}
