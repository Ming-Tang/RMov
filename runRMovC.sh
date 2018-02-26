#!/bin/bash
ghc RMovC.hs -main-is RMovC -o RMovC
./RMovC $1 $2 instrs
./RMovC $1 $2 dot > $2.dot
dot -Tsvg $2.dot > $2.svg && open $2.svg

