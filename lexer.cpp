#include <iostream>
#include <string>
#include <regex>
#include <iterator>
#include <vector>
#include <algorithm>
#include "token.h"
#include "debug.h"

using namespace std;

Tokens lex(const string& input) {
  string idLetterFirst = "_a-zA-Z";
  string idLetter = idLetterFirst + "0-9";
  string keywords;
  for (auto& k : Keyword::getAll()) {
    if (k.size() == 1)
      keywords.append("\\");
    keywords.append(k);
    if (all_of(k.begin(), k.end(), [](char c) { return isalpha(c); }))
      keywords.append("\\b");
    keywords.append("|");
  }
  keywords.pop_back();
  vector<pair<string, function<optional<Token>(const string&)>>> v {
      {"#.*\n", [](const string&) -> optional<Token> { return none;}},
      {keywords, [](const string& s) -> optional<Token> { return Token(Keyword::get(s));}},
      {"\\-|\\+|\\<|\\>|\\=", [](const string& s) -> optional<Token> { return Token(Operator{s.front()});}},
      {"[0-9]+" , [](const string& s) -> optional<Token> { return Token(Number{s}); } } ,
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
        if (auto token = v[index].second(matched))
          ret.push_back(*token);
        auto codeLoc = CodeLoc(lines[lastPos - 1], columns[lastPos - 1]);
        ret.back().codeLoc = codeLoc;
        if (!all_of(skipped.begin(), skipped.end(), [](char c) { return isspace(c); }))
          codeLoc.error("Unrecognized input: \"" + skipped + "\"");
        ret.back().value = matched;
        INFO << "Matched \"" << matched << "\" with rule " << index;
        break;
      }
  }
  return Tokens(ret);
}
