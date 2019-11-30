#pragma once

#include "util.h"
#include "code_loc.h"

class CodeLoc;
class ErrorLoc;

class MoveChecker {
  public:
  void startBlock();
  void endBlock();
  void newAlternative();
  void startLoop(int loopId);
  NODISCARD JustError<ErrorLoc> endLoop(int loopId);
  void breakStatement(int loopId);
  NODISCARD JustError<string> continueStatement();
  void returnStatement();
  void addVariable(string name);
  void clearStatementUsages();
  NODISCARD JustError<string> moveVariable(CodeLoc, const string& name);
  NODISCARD JustError<string> getUsageError(const string& name);
  ~MoveChecker();
  MoveChecker();
  MoveChecker(const MoveChecker&) = delete;

  private:
  struct Block;
  struct VariableInfo;
  vector<Block> blocks;
  struct StatementUsage;
  vector<StatementUsage> statementUsages;
  optional<CodeLoc> wasMoved(const string& name) const;
};
