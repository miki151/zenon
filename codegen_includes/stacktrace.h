#pragma once

#ifndef _WIN32 
#include <sys/types.h>
#include <sys/ptrace.h>

// This works on both linux and MacOSX (and any BSD kernel).
inline bool runningUnderDebugger() {
  if (ptrace(PTRACE_TRACEME, 0, 1, 0) < 0)
    return true;
  else {
    ptrace(PTRACE_DETACH, 0, 1, 0);
    return false;
  }
}
#else
inline bool runningUnderDebugger() {
  return IsDebuggerPresent();
}
#endif


struct PanicException {};

#define F_BEGIN try
#define F_END(FUN) catch(PanicException& panicException1234) {\
  fprintf(stderr, "%s\n", FUN);\
  throw panicException1234;\
}

