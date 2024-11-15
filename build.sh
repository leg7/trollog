#!/bin/sh

ghc -outputdir ./build -O2 -Wall -Wall -Wno-unused-do-bind ./*.hs
