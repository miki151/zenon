#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

#CLANG_OPT="-fsanitize=address"

WILDCARD=$1

cd tests/

function compile() {
  OUTPUT=$3
  if [ "$2" = "no_compile" ]; then
    EXPECTED_RET="1"
  else
    EXPECTED_RET="0"
  fi
  ../zenon $1 -o $OUTPUT 2> /dev/null
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



function run_test() {
  BINARY_TMP=$(mktemp)
  I=$1
  EXPECTED=`head -n 1 $I | cut -c 4-`
  echo -ne "\033[2KRunning $I"\\r
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
  $BINARY_TMP testArg1 testArg2
  RESULT=$?
  if [ "$RESULT" != "$EXPECTED" ]; then
    echo -e "$I: $RED Expected $EXPECTED, got $RESULT$NC"
    return
  fi
  rm $BINARY_TMP
#echo -e "$GREEN Success$NC"
}

ALL_TESTS=`ls $WILDCARD*{.znn,/main.znn} 2> /dev/null`
THREADS=`nproc`

RUNNING=0
for I in $ALL_TESTS; do 
  run_test $I &
  let "RUNNING+=1"
  if [[ "$RUNNING" -gt "$THREADS" ]]; then
    wait
    RUNNING=0
  fi
done

wait

echo -ne "\033[2K"\\r
