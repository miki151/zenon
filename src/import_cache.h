#pragma once

#include "util.h"
#include "context.h"

class ImportCache {
  public:

  void insert(string path, Context context);
  vector<string> getAllImports() const;
  bool contains(string path) const;
  const Context& getContext(string path) const;
  const vector<string>& getCurrentImports() const;
  bool isCurrentlyImported(string path) const;
  void pushCurrentImport(string path);
  void popCurrentImport();

  private:
  struct CacheElem {
    string path;
    Context context;
  };
  vector<CacheElem> cache;
  vector<string> current;
};
