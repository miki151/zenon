#include "reader.h"

WithError<FileContents> readFromFile(const char* path){
  ifstream in;
  in.open(path);
  if (!in.good())
    return "Failed to open file " + quote(path);
  stringstream ss;
  ss << in.rdbuf();
  return FileContents{ss.str()};
}
