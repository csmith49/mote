# BUILD VARIABLES =======================
build=_build/default/bin

# BUILDING THE TOOL =====================

# entrypoint just uses dune to build the synth tool
.phony: all
all: enumerate
enumerate: lib
	dune build bin/enumerate.exe
	mv $(build)/enumerate.exe enumerate

# takes us into an interactive prompt with
.phony: live
live: lib
	dune utop lib

# for cleaning the bulid
.phony: clean
clean:
	dune clean
	rm -rf _build enumerate
