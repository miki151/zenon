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

#ifndef STDAFX_H
#define STDAFX_H

#ifndef RELEASE
  #if __has_cpp_attribute(nodiscard)
    #define NODISCARD [[nodiscard]]
  #elif __has_cpp_attribute(gnu::warn_unused_result)
    #define NODISCARD [[gnu::warn_unused_result]]
  #else
    #define NODISCARD
  #endif

  #if __has_cpp_attribute(fallthrough)
    #define FALLTHROUGH [[fallthrough]]
  #elif __has_cpp_attribute(clang::fallthrough)
    #define FALLTHROUGH [[clang::fallthrough]]
  #else
    #define FALLTHROUGH
  #endif

#else
  #define NODISCARD
  #define FALLTHROUGH
#endif

#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>
#include <functional>
#include <sstream>
#include <string>
#include <cstdlib>
#include <memory>
#include <bitset>
#include "ctype.h"
#include <cstring>
#include <time.h>
#ifndef VSTUDIO
#include <sys/time.h>
#endif
#include <cstdlib>
#include <typeinfo>
#include <unordered_set>
#include <unordered_map>
#include <queue>
#include <random>
#include <stack>
#include <stdexcept>
#include <tuple>
#include <numeric>
#include <chrono>
#include <cstddef>
#include <boost/regex.hpp>
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

#include <thread>

#include <typeinfo>
#include <tuple>
#include <mutex>
#include <atomic>
#include <condition_variable>

#include <string>
#include <deque>
#include <map>
#include <set>
#include "my_shared_ptr.h"
#include "my_containers.h"
using std::map;
using std::multiset;
using std::deque;
using std::string;

using std::queue;
using std::unique_ptr;
using std::make_unique;
using std::default_random_engine;
using std::function;
using std::initializer_list;
using std::pair;
using std::tuple;
using std::out_of_range;
using std::unordered_map;
using std::bitset;
using std::min;
using std::max;
using std::to_string;
using std::ofstream;
using std::ifstream;
using std::istream;
using std::ostream;
using std::stringstream;
using std::istringstream;
using std::ostringstream;
using std::endl;
using std::cerr;
using std::cout;
using boost::regex;
using boost::sregex_iterator;
using boost::regex_replace;
using std::priority_queue;
using std::make_pair;
using std::stack;
using std::uniform_int_distribution;
using std::uniform_real_distribution;
using std::make_tuple;
using std::hash;
using std::array;

using std::recursive_mutex;
typedef std::unique_lock<recursive_mutex> RecursiveLock;

#ifdef OSX
using boost::thread;
using boost::this_thread::sleep_for;
using boost::chrono::duration;
using boost::chrono::milliseconds;
using boost::chrono::steady_clock;
using boost::chrono::duration_cast;
inline thread::id currentThreadId() { return boost::this_thread::get_id(); }
#else
using std::thread;
using std::this_thread::sleep_for;
using std::chrono::duration;
using std::chrono::milliseconds;
using std::chrono::steady_clock;
using std::chrono::duration_cast;
inline thread::id currentThreadId() { return std::this_thread::get_id(); }
#endif
using std::atomic;
using std::swap;
using std::remove_if;

#define TRY(expr) ({ auto&& x = expr; if (!x) return x.get_error(); std::move(*x); })

using namespace std::string_literals;

#include "variant.h"

#endif
