#pragma once
#include "stdafx.h"
#include "code_loc.h"

struct LanguageIndex {
  using Cursor = pair<int, int>;
  struct DefinitionInfo {
    Cursor end;
    set<CodeLoc> targets;
  };
  struct Signature {
    int line1;
    int column1;
    int line2;
    int column2;
    unordered_set<string> text;
  };
  struct File {
    map<Cursor, DefinitionInfo> definitions;
    map<Cursor, DefinitionInfo> references;
    vector<Signature> signatures;
  };
  unordered_map<string, File> files;

  void addDefinition(CodeLoc, CodeLoc end, CodeLoc target);
  void addDefinition(CodeLoc, int length, CodeLoc target);
  void addSignature(CodeLoc, int endColumn, int endLine, string text);

  set<CodeLoc> getTarget(CodeLoc);
  set<CodeLoc> getReferences(CodeLoc);
  const unordered_set<string>& getSignature(CodeLoc);
};
