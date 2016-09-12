#!/bin/bash
#decription     	: build and run Lab1
#author		 				: niklasad1
#date            	: 2016-09-06
#version         	: 1.0   
#filename		 			: run.sh
#notes           	: N/A
#arguments	    	: TODO
#==============================================================================

echo "STRESS TEST"
FILES=$PWD/tests/*
#  TEST COMBINATIONS 10-15 SPEED 
#  RUN TIME 10 min then exit

for (( i=10; i<16; i++ )); do
  for (( j=10; j <16; j++ )); do
    filename="tests/log.$i-$j"
    echo $filename
    java -cp bin Main Lab1.map $i $j > "$filename" 2>&1 &
    sleep 10m
  done
done

for f in $FILES
do
  b=${f##*/}  
  echo "Proccessing file $b"
  dmy=$(cat $f | grep -i 'error')
  if [[ -n "$dmy" ]]; 
  then
    echo "TEST $b FAILED"
  fi
done




