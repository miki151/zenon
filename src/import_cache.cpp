#include "import_cache.h"


void ImportCache::insert(string path, Context context) {
  cache.push_back(CacheElem{path, std::move(context)});
}

vector<string> ImportCache::getAllImports() const {
  return transform(cache, [](const auto& elem) { return elem.path; });
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

void ImportCache::pushCurrentImport(string path) {
  current.push_back(path);
}

void ImportCache::popCurrentImport() {
  current.pop_back();
}
