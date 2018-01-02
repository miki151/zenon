#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

function transpile() {
  ./zygmunt $1 > tmp.cpp 2> log.out
  RESULT=$?
  if [ "$RESULT" = "1" ]; then
    echo -e "$RED Compilation failed$NC"
    return 1
  fi
  if [ "$RESULT" != "0" ]; then
    echo -e "${RED} Compiler crashed$NC"
    return 1
  fi
}

for I in `ls tests/*.znn`; do 
  echo "Running $I"
  transpile $I
  if [ "$?" != "0" ]; then
    continue
  fi
  g++ -std=c++14 tmp.cpp -o tmp
  if [ "$?" != "0" ]; then
    echo -e "$RED C++ compilation failed$NC"
    continue
  fi
  ./tmp
  RESULT=$?
  EXPECTED=`head -n 1 $I | cut -c 4-`
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo -e "$GREEN Success$NC"
done

for D in `ls -d tests/*/`; do
  echo "Running directory $D"
  OBJECTS=""
  for I in `ls $D`; do
    echo "Compiling $I"
    transpile $D/$I
    if [ "$?" != "0" ]; then
      continue 2
    fi
    g++ -std=c++14 tmp.cpp -c -o $I.o
    OBJECTS="$OBJECTS $I.o"
    if [ "$?" != "0" ]; then
      echo -e "$RED C++ compilation failed$NC"
      continue
    fi
  done
  echo "Linking $OBJECTS"
  g++ $OBJECTS -o tmp
  ./tmp
  RESULT=$?
  EXPECTED=`head -n 1 $D/main.znn | cut -c 4-`
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo -e "$GREEN Success$NC"
done
