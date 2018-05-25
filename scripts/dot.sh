#!/bin/bash

if [ ! -f "$1" ] ; then
  echo "First copy the output of '/remoll/printgeometry false' to a file."
  echo "Then run this script as: $0 <filename>"
  exit
fi

cat $1  | sed 's/\[[-0-9]*\]\ .*\ [0-9]*\ .*$//' |
{
  IFS=''
  level=0
  echo "digraph G {"
  while read line ; do
    nline="${line//[^ ]}"
    level=${#nline}
    parent[$level]=$line
    let parentlevel=$level-2
    if [ ${level} -eq 0 ] ; then
      #top level
      continue
    fi
    echo "$nline${parent[$parentlevel]} -> $line ;"
  done
  echo "}"
} | tee ${1%.*}.dot | dot -Tpng -o ${1%.*}.png
