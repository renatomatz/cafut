#!/bin/bash

caf -g cafut.f90 basic_tests.f90 -o basic_tests
cafrun -n 1 ./basic_tests
