#include <iostream>
#include <string>
#include <iterator>
#include <vector>
#include <algorithm>
#include "token.h"
#include "debug.h"
#include "operator.h"
#include "util.h"


static string getKeywords() {
  string keywords;
  auto all = getAllKeywords();
  sort(all.begin(), all.end(), [](const string& o1, const string& o2) { return o1.size() > o2.size(); });
  for (auto& k : all) {
    for (auto& c : k) {
      if (!isalpha(c))
      keywords.append("\\");
      keywords.push_back(c);
    }
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
    for (char c : k)
      ret += "\\" + string(1, c);
    ret.append("|");
  }
  ret.pop_back();
  return ret;
}

WithErrorLine<Tokens> lex(const string& input, CodeLoc initialPos, const string& eofTokenValue) {
  string idLetterFirst = "a-zA-Z";
  string idLetter = idLetterFirst + "0-9_";
  vector<pair<string, function<optional<Token>(const string&)>>> v {
      {"//.*", [](const string&) -> optional<Token> { return none;}},
      {"/\\*[\\s\\S]*?\\*/", [](const string&) -> optional<Token> { return none;}},
      {getKeywords(), [](const string& s) -> optional<Token> { return Token(getKeyword(s));}},
      {getOperators(), [](const string& s) -> optional<Token> {
          return Token(*getOperator(s));}},
      {"\"(?:[^\"\\\\]|\\\\.)*?\"" , [](const string&) -> optional<Token> { return Token(StringToken{}); } },
      {"'.'" , [](const string&) -> optional<Token> { return Token(CharToken{}); } },
      {"[0-9]+\\.[0-9]+" , [](const string&) -> optional<Token> { return Token(RealNumber{}); } },
      {"[0-9]+" , [](const string&) -> optional<Token> { return Token(Number{}); } },
      {"[" + idLetterFirst + "][" + idLetter + "]*" , [](const string&) -> optional<Token> { return Token(IdentifierToken{}); }},
  };
  INFO << "Lexing expressions:";
  for (auto& elem : v)
    INFO << elem.first;
  vector<int> lines(input.size());
  vector<int> columns(input.size());
  int currentLine = 0;
  int currentColumn = 0;
  for (int i = 0; i < input.size(); ++i) {
    lines[i] = currentLine;
    columns[i] = currentColumn;
    if (input[i] == '\n') {
      ++currentLine;
      currentColumn = 0;
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
  struct EmbedBlock {
    string value;
    int numBrackets;
    CodeLoc codeLoc;
  };
  optional<EmbedBlock> embedBlock;
  for (auto it = words_begin; it != words_end; ++it) {
    for (int index = 0 ; index < it->size(); ++index)
      if (!it->str(index + 1).empty()) { // determine which submatch was matched
        string matched = it->str();
        string skipped = input.substr(lastPos, it->position() - lastPos);
        auto tokenPos = lastPos + skipped.size();
        lastPos += matched.size() + skipped.size();
        auto codeLoc = initialPos.plus(lines[tokenPos], columns[tokenPos]);
        auto token = v[index].second(matched);
        if (embedBlock) {
          if (embedBlock->numBrackets == 0 && !(token == Token(Keyword::OPEN_BLOCK)))
            return codeLoc.getError("Expected " + quote("{") + " after " + quote("embed"));
          if (token == Token(Keyword::OPEN_BLOCK)) {
            if (embedBlock->numBrackets > 0)
              embedBlock->value.append(skipped + matched);
            ++embedBlock->numBrackets;
          } else
          if (token == Token(Keyword::CLOSE_BLOCK)) {
            --embedBlock->numBrackets;
            if (embedBlock->numBrackets > 0)
              embedBlock->value.append(skipped + matched);
          } else
            embedBlock->value.append(skipped + matched);
          if (embedBlock->numBrackets == 0) {
            ret.push_back(EmbedToken{});
            ret.back().value = embedBlock->value;
            ret.back().codeLoc = embedBlock->codeLoc;
            embedBlock = none;
          }
          break;
        } else
        if (v[index].second(matched) == Token(Keyword::EMBED)) {
          embedBlock = EmbedBlock { "", 0, codeLoc };
          break;
        }
        if (!all_of(skipped.begin(), skipped.end(), [](char c) { return isspace(c); }))
          return codeLoc.getError("Unrecognized token: " + quote(skipped));
        if (auto token = v[index].second(matched)) {
          ret.push_back(*token);
          ret.back().codeLoc = codeLoc;
          ret.back().value = process(*token, matched);
        }
        INFO << "Matched " << quote(matched) << " with rule " << index;
        break;
      }
  }
  Token eof = EofToken{};
  eof.value = eofTokenValue;
  eof.codeLoc = initialPos.plus(currentLine, currentColumn);
  ret.push_back(eof);
  return Tokens(ret);
}

