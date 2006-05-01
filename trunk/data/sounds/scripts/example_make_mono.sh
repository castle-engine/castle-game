#!/bin/bash
set -eu

# Sample trivial script to use sox to convert some sound files to mono.

doit()
{
  sox "$1".wav -c 1 "$1"_2.wav
  mv "$1".wav ~/.Trash/
  mv "$1"_2.wav "$1".wav
}

doit input1
doit input2