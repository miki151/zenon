#include "util.h"

string combine(const vector<string>& adj, const std::string& separator) {
  string res;
  for (int i = 0; i < adj.size(); ++i) {
    if (i > 0)
      res.append(separator);
    res.append(adj[i]);
  }
  return res;
}

std::string quote(const string& s) {
  return "\"" + s + "\"";
}

vector<string> split(const string& s, const std::set<char>& delim) {
  if (s.empty())
    return {};
  int begin = 0;
  vector<string> ret;
  for (int i = 0; i < s.size() + 1; ++i)
    if (i == s.size() || delim.count(s[i])) {
      string tmp = s.substr(begin, i - begin);
      ret.push_back(tmp);
      begin = i + 1;
    }
  return ret;
}

string getParentPath(const string& s) {
  for (int i = s.size() - 1; i >= 0; --i)
    if (s[i] == '/')
      return s.substr(0, i);
  return ".";
}

bool endsWith(const string& s, const string& suffix) {
  return s.size() >= suffix.size() && contains(s, suffix, s.size() - suffix.size());
}

bool startsWith(const string& s, const string& prefix) {
  return contains(s, prefix, 0);
}

bool contains(const string& a, const string& substring, unsigned long index) {
  if (a.size() - index < substring.size())
    return false;
  for (auto i = index; i < index + substring.size(); ++i)
    if (a[i] != substring[i - index])
      return false;
  return true;
}
