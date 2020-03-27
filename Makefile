build=_build/default/bin

.PHONY: all
all: enumerate
enumerate: lib bin/enumerate.ml
	dune build bin/enumerate.exe
	mv $(build)/enumerate.exe enumerate

.PHONY: live
live: lib
	dune utop lib

.PHONY: clean
clean:
	dune clean
	rm -rf _build enumerate