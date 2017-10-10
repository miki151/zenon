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
  vector<pair<string, function<optional<Token>(const string&)>>> v {
      {"#.+\n", [](const string&) -> optional<Token> { return none;}},
      {"if|else|return|\\(|\\)|\\{|\\}|;|\\,|true|false",
           [](const string& s) -> optional<Token> { return Token(Keyword::get(s));}},
      {"\\-|\\+|\\<|\\>", [](const string& s) -> optional<Token> { return Token(Operator{s.front()});}},
      {"[0-9]+" , [](const string& s) -> optional<Token> { return Token(Number{s}); } } ,
      {"[_|a-z|A-Z][_|a-z|A-Z|0-9]*" , [](const string&) -> optional<Token> { return Token(Identifier{}); }},
  };
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
        if (it->position() > lastPos && !all_of(skipped.begin(), skipped.end(), [](char c) { return isspace(c); }))
          FATAL << "Unrecognized input: \"" << skipped << "\"";
        lastPos += matched.size() + skipped.size();
        if (auto token = v[index].second(matched))
          ret.push_back(*token);
        ret.back().codeLoc = CodeLoc(lines[lastPos - 1], columns[lastPos - 1]);
        ret.back().value = matched;
        INFO << "Matched " << matched;
        break;
      }
  }
  return Tokens(ret);
}
