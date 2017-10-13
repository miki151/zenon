#include "util.h"

string combine(const vector<string>& adj) {
  string res;
  for (int i = 0; i < adj.size(); ++i) {
    if (i > 0)
      res.append(", ");
    res.append(adj[i]);
  }
  return res;
}

std::string quote(const string& s) {
  return "\"" + s + "\"";
}
