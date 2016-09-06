#!/bin/bash
#decription     	: build and run Lab1
#author		 				: niklasad1
#date            	: 2016-09-06
#version         	: 1.0   
#filename		 			: run.sh
#notes           	: N/A
#arguments	    	: TODO
#==============================================================================

echo "BUILD AND RUN"
echo "====================================================================="
if [[ $# -eq 0 ]]; then
  make all
  java -cp bin Main Lab1.map 5 5 20
elif [[ $# -eq 3 ]]; then
  make all
  java -cp bin Main Lab1.map "$1" "$2" "$3" 
else
  echo "Syntax: $0 || $0 speed1 speed2 sim_speed"
fi

