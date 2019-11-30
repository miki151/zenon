#include "stdafx.h"
#include "move_checker.h"
#include "code_loc.h"

enum class BlockType { STANDALONE, ALTERNATIVE, LOOP };

struct MoveInfo {
  string variable;
  optional<int> loopId;
  CodeLoc loc;
};

struct MoveChecker::Block {
  BlockType type;
  unordered_set<string> variables;
  vector<MoveInfo> moves;
};

void MoveChecker::startBlock() {
  blocks.push_back(Block{ BlockType::STANDALONE, {} });
}

struct MoveChecker::StatementUsage {
  string variable;
  bool moved;
};

void MoveChecker::endBlock() {
  vector<MoveInfo> allMoved;
  while (blocks.back().type == BlockType::ALTERNATIVE) {
    allMoved.append(blocks.back().moves);
    blocks.pop_back();
  }
  auto block = blocks.back();
  blocks.pop_back();
  block.moves.append(allMoved);
  for (auto& move : block.moves)
    if (!block.variables.count(move.variable)) {
      CHECK(!blocks.empty());
      blocks.back().moves.push_back(move);
    }
}

JustError<ErrorLoc> MoveChecker::endLoop(int loopId) {
  auto& block = blocks.back();
  CHECK(block.type == BlockType::LOOP);
  for (auto& move : block.moves) {
    if (!move.loopId)
      return move.loc.getError("Variable is moved inside loop: " + move.variable);
    if (move.loopId == loopId)
      move.loopId = none;
  }
  endBlock();
  return none;
}

void MoveChecker::breakStatement(int loopId) {
  for (int i = blocks.size() - 1; i >= 0; --i) {
    for (auto& move : blocks[i].moves)
      move.loopId = loopId;
    if (blocks[i].type == BlockType::ALTERNATIVE)
      break;
  }
}

JustError<string> MoveChecker::continueStatement() {
  for (int i = blocks.size() - 1; i >= 0; --i) {
    for (auto& move : blocks[i].moves)
      if (!move.loopId)
        return "Variable is moved inside loop: " + move.variable;
    if (blocks[i].type != BlockType::STANDALONE)
      break;
  }
  return none;
}

void MoveChecker::returnStatement() {
  for (int i = blocks.size() - 1; i >= 0; --i) {
    blocks[i].moves.clear();
    if (blocks[i].type == BlockType::ALTERNATIVE)
      break;
  }
}

void MoveChecker::newAlternative() {
  blocks.push_back(Block{ BlockType::ALTERNATIVE, {} });
}

void MoveChecker::startLoop(int loopId) {
  blocks.push_back(Block{ BlockType::LOOP, {} });
}

void MoveChecker::addVariable(string name) {
  blocks.back().variables.insert(std::move(name));
}

void MoveChecker::clearStatementUsages() {
  statementUsages.clear();
}

JustError<string> MoveChecker::moveVariable(CodeLoc loc, const string& name) {
  if (auto loc = wasMoved(name))
    return "Variable " + name + " has already been moved here: " + loc->toString();
  for (auto& usage : statementUsages)
    if (usage.variable == name)
      return "Variable " + name + " can't be both moved and referenced within a single statement";
  blocks.back().moves.push_back(MoveInfo{name, none, loc});
  statementUsages.push_back(StatementUsage{name, true});
  return none;
}

JustError<string> MoveChecker::getUsageError(const string& name) {
  if (auto loc = wasMoved(name))
    return "Variable " + name + " has been moved here:" + loc->toString();
  for (auto& usage : statementUsages)
    if (usage.variable == name && usage.moved)
      return "Variable " + name + " can't be both moved and referenced within a single statement";
  statementUsages.push_back(StatementUsage{name, false});
  return none;//"Variable not found: " + name;
}

MoveChecker::~MoveChecker() {
  endBlock();
}

MoveChecker::MoveChecker() {
  startBlock();
}

optional<CodeLoc> MoveChecker::wasMoved(const string &name) const {
  for (int i = blocks.size() - 1; i >= 0; --i) {
    for (auto& move : blocks[i].moves)
      if (move.variable == name && !move.loopId)
        return move.loc;
    if (blocks[i].variables.count(name))
      return none;
    if (blocks[i].type == BlockType::ALTERNATIVE)
      while (i > 0 && blocks[i - 1].type == BlockType::ALTERNATIVE)
        --i;
  }
  // We end here if it's a struct field because it wasn't declared as a variable
  return none;
}
