#!/bin/sh

exec dune exec --profile=release src/test-gen-diamond/test_gen_diamond.exe -- $@ 

