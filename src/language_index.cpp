#include "stdafx.h"
#include "language_index.h"


void LanguageIndex::addDefinition(CodeLoc l, int length, CodeLoc target) {
  auto& def = files[l.file].definitions[Cursor(l.line, l.column)];
  def.length = length;
  def.targets.insert(target);
  auto& ref = files[target.file].references[Cursor(target.line, target.column)];
  ref.length = length;
  ref.targets.insert(l);
}

set<CodeLoc> LanguageIndex::getTarget(CodeLoc l) {
  auto& m = files[l.file].definitions;
  auto it = m.upper_bound(Cursor(l.line, l.column));
  if (it != m.begin()) {
    --it;
    if (it->first.first == l.line && it->first.second + it->second.length >= l.column)
      return it->second.targets;
  }
  return set<CodeLoc>();
}

set<CodeLoc> LanguageIndex::getReferences(CodeLoc l) {
  auto& m = files[l.file].references;
  auto it = m.upper_bound(Cursor(l.line, l.column));
  if (it != m.begin()) {
    --it;
    if (it->first.first == l.line && it->first.second + it->second.length >= l.column)
      return it->second.targets;
  }
  return set<CodeLoc>();
}

void LanguageIndex::addSignature(CodeLoc l, int endColumn, int endLine, string text) {
  auto& signatures = files[l.file].signatures;
  for (auto& s : signatures)
    if (s.column1 == l.column && s.line1 == l.line && s.column2 == endColumn && s.line2 == endLine) {
      s.text.insert(std::move(text));
      return;
    }
  signatures.push_back(Signature {
      l.line,
      l.column,
      endLine,
      endColumn,
      {std::move(text)}
  });
}

const unordered_set<string>& LanguageIndex::getSignature(CodeLoc l) {
  Signature* best = nullptr;
  for (auto& s : files[l.file].signatures) {
    if (make_pair(s.line1, s.column1) <= make_pair(l.line, l.column) &&
        make_pair(s.line2, s.column2) >= make_pair(l.line, l.column) &&
        (!best || make_pair(best->line1, best->column1) < make_pair(s.line1, s.column1)))
      best = &s;
  }
  if (best)
    return best->text;
  static const unordered_set<string> empty;
  return empty;
}