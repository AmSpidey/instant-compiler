#!/bin/bash
llvm-as -o $1.bc $1.ll
llvm-link -o out.bc $1.bc
lli out.bc