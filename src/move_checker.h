#pragma once

#include "util.h"

class CodeLoc;
class ErrorLoc;

class MoveChecker {
  public:
  void startBlock();
  void endBlock();
  void newAlternative();
  void startLoop(int loopId);
  NODISCARD optional<ErrorLoc> endLoop(int loopId);
  void breakStatement(int loopId);
  NODISCARD optional<string> continueStatement();
  void returnStatement();
  void addVariable(string name);
  NODISCARD optional<string> moveVariable(CodeLoc, const string& name);
  NODISCARD optional<string> getUsageError(const string& name) const;
  ~MoveChecker();
  MoveChecker();
  MoveChecker(const MoveChecker&) = delete;

  private:
  struct Block;
  struct VariableInfo;
  vector<Block> blocks;
  optional<CodeLoc> wasMoved(const string& name) const;
};
