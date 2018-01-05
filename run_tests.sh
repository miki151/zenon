#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'


function compile() {
  OUTPUT=$3
  if [ "$2" = "no_compile" ]; then
    EXPECTED_RET="1"
  else
    EXPECTED_RET="0"
  fi
  ./zenon $1 -o $OUTPUT 2> /dev/null
  RESULT=$?
  if [ "$RESULT" = "2" ]; then
    echo -e "$RED C++ compilation failed$NC"
    return 1
  fi
  if [ "$RESULT" -gt "2" ]; then
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

BINARY_TMP=$(mktemp)

for I in `ls tests/*.znn`; do 
  echo "Running $I"
  EXPECTED=`head -n 1 $I | cut -c 4-`
  if [ "$EXPECTED" = "" ]; then
    echo -e "$RED No expected value specified$NC"
    continue
  fi
  compile $I $EXPECTED $BINARY_TMP
  if [ "$?" != "0" ]; then
    continue
  fi
  if [ "$EXPECTED" = "no_compile" ]; then
    echo -e "$GREEN Success$NC"
    continue
  fi
  $BINARY_TMP
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
    OPATH=$(mktemp)
    compile "$D/$I -c" $EXPECTED $OPATH
    if [ "$?" != "0" ]; then
      continue 2
    fi
    if [ "$EXPECTED" = "no_compile" ]; then
      echo -e "$GREEN Success$NC"
      continue 2
    fi
    OBJECTS="$OBJECTS $OPATH"
  done
#  echo "Linking $OBJECTS"
  g++ $OBJECTS -o $BINARY_TMP
  $BINARY_TMP
  RESULT=$?
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$RED Expected $EXPECTED, got $RESULT$NC"
    continue
  fi
  echo -e "$GREEN Success$NC"
  rm $OPATH
done

rm $BINARY_TMP
