#!/usr/bin/env bash
xor=$(grep -o "(xor" $1 | wc -l) 
not=$(grep -o "(not" $1 | wc -l)
or=$(grep -o "(or" $1 | wc -l)
and=$(grep -o "(and" $1 | wc -l)
var=$(grep -o "(declare-fun" $1 | wc -l)

echo -e "XOR:\t${xor}"
echo -e "NOT:\t${not}"
echo -e "OR:\t${or}"
echo -e "AND:\t${and}"
echo -e "VAR:\t${var}"
