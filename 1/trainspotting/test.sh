#!/bin/bash

#==============================================================================
#decription     	: build and run Lab1
#author		 				: niklasad1
#date            	: 2016-09-12
#version         	: 1.0   
#filename		 			: run.sh
#notes           	: Tests each combination for 10 minutes and searches through the output for error messages
#arguments	    	: TODO
#==============================================================================

echo "STARTING TESTS"
OUTPUT_DIR=tests
FILES=$PWD/$OUTPUT_DIR/*
TEST_PASS=""
mkdir -p $OUTPUT_DIR

for (( i=1; i<16; i++ )); do
  for (( j=1; j < 6; j++ )); do
    filename="$OUTPUT_DIR/log.$i-$j"
    echo $filename
    java -cp bin Main Lab1.map $i $j 1 > "$filename" 2>&1 &
  done
  sleep 1m
  killall -9 java
  killall -9 tsim
  for (( j=6; j < 10; j++ )); do
    filename="$OUTPUT_DIR/log.$i-$j"
    echo $filename
    java -cp bin Main Lab1.map $i $j 1 > "$filename" 2>&1 &
  done
  sleep 1m
  killall -9 java
  killall -9 tsim
  for (( j=10; j < 16; j++ )); do
    filename="$OUTPUT_DIR/log.$i-$j"
    echo $filename
    java -cp bin Main Lab1.map $i $j 1 > "$filename" 2>&1 &
  done
  sleep 1m
  killall -9 java
  killall -9 tsim
done

for f in $FILES
do
  b=${f##*/}  
  # echo "Proccessing file $b"
  dmy=$(cat "$f" | grep -Ei 'error|illegal|no such')
  if [[ -n "$dmy" ]]; 
  then
    echo "TEST "$b" FAILED"
    TEST_PASS="FAIL"
  fi
done

if [[ -z "$TEST_PASS" ]];
then
  echo "=============================================================================="
  echo "TESTS PASSED!!!!"
  echo "=============================================================================="
else
  echo "=============================================================================="
  echo "TESTS FAILED"
  echo "=============================================================================="
fi
