#include "util.h"

string combine(const vector<string>& adj, bool commasOnly) {
  string res;
  for (int i = 0; i < adj.size(); ++i) {
    if (i == adj.size() - 1 && i > 0 && !commasOnly)
      res.append(" and ");
    else if (i > 0)
      res.append(", ");
    res.append(adj[i]);
  }
  return res;
}
