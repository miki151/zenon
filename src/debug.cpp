/* Copyright (C) 2013-2014 Michal Brzozowski (rusolis@poczta.fm)

   This file is part of KeeperRL.

   KeeperRL is free software; you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   KeeperRL is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program.
   If not, see http://www.gnu.org/licenses/ . */

#include "stdafx.h"

#include "debug.h"

void fail() {
  struct ProgramCrashed {};
  throw ProgramCrashed{};
}

DebugOutput DebugOutput::toStream(std::ostream& o) {
  return DebugOutput(o, [&] { o << "\n" << std::flush;});
}

DebugOutput DebugOutput::toString(function<void (const string&)> callback) {
  stringstream* os = new stringstream(); // we are going to leak this object, sorry
  return DebugOutput(*os, [=] { callback(os->str()); os->str(""); });
}

DebugOutput DebugOutput::crash() {
  return DebugOutput(*(new stringstream()), [] { fail(); });
}

DebugOutput DebugOutput::exitProgram(int code) {
  return DebugOutput(*(new stringstream()), [code] { exit(code); });
}

void DebugLog::addOutput(DebugOutput o) {
  outputs.push_back(o);
}

DebugLog::Logger DebugLog::get() {
  return Logger(outputs);
}

DebugLog InfoLog;
DebugLog FatalLog;
DebugLog ErrorLog;
