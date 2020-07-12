#include "import_cache.h"
#include "ast.h"

ImportCache::ImportCache() {
}

void ImportCache::insert(string path, Context context, bool builtIn) {
  cache.push_back(CacheElem{path, std::move(context), builtIn});
}

vector<ModuleInfo> ImportCache::getAllImports() const {
  return transform(cache, [](const auto& elem) { return ModuleInfo{elem.path, elem.builtIn}; });
}

bool ImportCache::contains(string path) const {
  for (auto& elem : cache)
    if (elem.path == path)
      return true;
  return false;
}

const Context& ImportCache::getContext(string path) const {
  for (auto& elem : cache)
    if (elem.path == path)
      return elem.context;
  fail();
}

const vector<string>& ImportCache::getCurrentImports() const {
  return current;
}

bool ImportCache::isCurrentlyImported(string path) const {
  return ::contains(current, path);
}

void ImportCache::setBuiltIn() {
  ++builtInCounter;
}

void ImportCache::popBuiltIn() {
  --builtInCounter;
}

void ImportCache::pushCurrentImport(string path, bool isBuiltIn) {
  current.push_back(path);
  if (isBuiltIn)
    ++builtInCounter;
}

void ImportCache::popCurrentImport(bool isBuiltIn) {
  current.pop_back();
  if (isBuiltIn)
    --builtInCounter;
}

bool ImportCache::currentlyInImport() const {
  return current.size() > 0;
}

bool ImportCache::isCurrentlyBuiltIn() const {
  return builtInCounter > 0;
}
