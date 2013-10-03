#!/bin/bash

gcc -c driver.c -o driver.o
llc prism.bc -o prism.s
gcc prism.s driver.o -o prism.out
