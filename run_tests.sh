#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

function transpile() {
#echo "Expecting $1 $2"
  if [ "$2" = "no_compile" ]; then
    EXPECTED_RET="1"
  else
    EXPECTED_RET="0"
  fi
  ./zygmunt $1 > tmp.cpp 2> log.out
  RESULT=$?
#echo "Result $RESULT"
  if [ "$RESULT" != "0" ] && [ "$RESULT" != "1" ]; then
    echo -e "${RED} Compiler crashed$NC"
    return 1
  fi
  if [ "$RESULT" != "$EXPECTED_RET" ]; then
    if [ "$EXPECTED_RET" = "0" ]; then
      echo -e "$1: $RED Compilation failed$NC"
    else
      echo -e "$1: $RED Compilation succeeded$NC"
    fi
    return 1
  else
    return 0
  fi
}

for I in `ls tests/*.znn`; do 
  echo "Running $I"
  EXPECTED=`head -n 1 $I | cut -c 4-`
  transpile $I $EXPECTED
  if [ "$?" != "0" ]; then
    continue
  fi
  if [ "$EXPECTED" = "no_compile" ]; then
    echo -e "$GREEN Success$NC"
    continue
  fi
  g++ -std=c++14 tmp.cpp -o tmp
  if [ "$?" != "0" ]; then
    echo -e "$RED C++ compilation failed$NC"
    continue
  fi
  ./tmp
  RESULT=$?
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo -e "$GREEN Success$NC"
done

for D in `ls -d tests/*/`; do
  echo "Running directory $D"
  EXPECTED=`head -n 1 $D/main.znn | cut -c 4-`
  OBJECTS=""
  FILES=`ls $D`
  if [ "$EXPECTED" = "no_compile" ]; then
    FILES="main.znn"
  fi
  for I in $FILES; do
#    echo "Compiling $I"
    transpile $D/$I $EXPECTED
    if [ "$?" != "0" ]; then
      continue 2
    fi
    if [ "$EXPECTED" = "no_compile" ]; then
      echo -e "$GREEN Success$NC"
      continue 2
    fi
    g++ -std=c++14 tmp.cpp -c -o $I.o
    if [ "$?" != "0" ]; then
      echo -e "$RED C++ compilation failed$NC"
      continue
    fi
    OBJECTS="$OBJECTS $I.o"
  done
#  echo "Linking $OBJECTS"
  g++ $OBJECTS -o tmp
  ./tmp
  RESULT=$?
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo -e "$GREEN Success$NC"
done
