#!/bin/sh

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'


for I in tests/*; do 
  echo "Running $I"
  ./zygmunt $I > tmp.cpp 2> log.out
  RESULT=$?
  if [ "$RESULT" = "1" ]; then
    echo "$RED Compilation failed$NC"
    continue
  fi
  if [ "$RESULT" != "0" ]; then
    echo "$RED Compiler crashed$NC"
    continue
  fi
  g++ -std=c++14 tmp.cpp -o tmp
  if [ "$?" != "0" ]; then
    echo "$RED C++ compilation failed$NC"
    continue
  fi
  ./tmp
  RESULT=$?
  EXPECTED=`head -n 1 $I | cut -c 4-`
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo "$GREEN Success$NC"
done

