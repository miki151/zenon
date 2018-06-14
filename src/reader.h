#pragma once

#include "util.h"
#include "code_loc.h"

struct FileContents {
  string value;
};

extern WithError<FileContents> readFromFile(const char* path);
