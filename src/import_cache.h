#pragma once

#include "util.h"
#include "context.h"

struct ModuleInfo;

class ImportCache {
  public:
  ImportCache();
  void insert(string path, Context context, bool builtIn);
  vector<ModuleInfo> getAllImports() const;
  void setBuiltIn();
  void popBuiltIn();
  bool contains(string path) const;
  const Context& getContext(string path) const;
  const vector<string>& getCurrentImports() const;
  bool isCurrentlyImported(string path) const;
  void pushCurrentImport(string path, bool isBuiltIn);
  void popCurrentImport(bool isBuiltIn);
  bool currentlyInImport() const;
  bool isCurrentlyBuiltIn() const;

  private:
  struct CacheElem {
    string path;
    Context context;
    bool builtIn;
  };
  vector<CacheElem> cache;
  vector<string> current;
  int builtInCounter = 0;
};
