#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

#CLANG_OPT="-fsanitize=address"

cd tests/

function compile() {
  OUTPUT=$3
  if [ "$2" = "no_compile" ]; then
    EXPECTED_RET="1"
  else
    EXPECTED_RET="0"
  fi
  ../zenon $1 -o $OUTPUT --cpp "clang++ $CLANG_OPT" 2> /dev/null
  RESULT=$?
  if [ "$RESULT" = "2" ]; then
    echo -e "$1: $RED C++ compilation failed$NC"
    return 1
  fi
  if [ "$RESULT" -gt "2" ]; then
    echo -e "$1: ${RED} Compiler crashed$NC"
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


function run_test() {
  I=$1
  EXPECTED=`head -n 1 $I | cut -c 4-`
#echo -n "Running $I Expecting: $EXPECTED"
  if [ "$EXPECTED" = "" ]; then
    echo -e "$I: $RED No expected value specified$NC"
    return
  fi
  compile $I $EXPECTED $BINARY_TMP
  if [ "$?" != "0" ]; then
    return
  fi
  if [ "$EXPECTED" = "no_compile" ]; then
#    echo -e "$GREEN Success$NC"
    return
  fi
  $BINARY_TMP
  RESULT=$?
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$I: $RED Expected $EXPECTED, got $RESULT$NC"
    return
  fi
#echo -e "$GREEN Success$NC"
}
for I in `ls *.znn`; do 
  run_test $I
done

for I in `ls */main.znn`; do 
  run_test $I
done

rm $BINARY_TMP
