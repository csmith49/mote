# BUILD VARIABLES =======================
build=_build/default/bin

# BUILDING THE TOOL =====================

# entrypoint just uses dune to build the synth tool
.phony: all
all: enumerate evaluate
enumerate: lib bin/enumerate.ml
	dune build bin/enumerate.exe
	mv $(build)/enumerate.exe enumerate

evaluate: lib bin/evaluate.ml
	dune build bin/evaluate.exe
	mv $(build)/evaluate.exe evaluate

# takes us into an interactive prompt with
.phony: live
live: lib
	dune utop lib

# for cleaning the bulid
.phony: clean
clean:
	dune clean
	rm -rf _build enumerate
