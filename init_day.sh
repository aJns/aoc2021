#!/bin/bash

printf -v daydir "day%02d" "$1"
srcdir="$daydir/src"

mkdir -p "$srcdir"
cp Makefile "$daydir/"
cp Main.hs "$srcdir/"
