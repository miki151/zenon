#include "reader.h"

std::string readFromFile(const char* path, optional<CodeLoc> codeLoc){
  ifstream in;
  in.open(path);
  string error = "Failed to open file " + quote(path);
  if (codeLoc)
    codeLoc->check(in.good(), error);
  else
    CHECK_SYNTAX(in.good()) << error;
  stringstream ss;
  ss << in.rdbuf();
  return ss.str();
}
