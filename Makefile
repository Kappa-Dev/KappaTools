.DEFAULT_GOAL := all

.PHONY: all clean

%:
	ocamlbuild -classic-display $@

all: KaSim.native
	rm -f KaSim && ln -s _build/KaSim.native KaSim

clean:
	ocamlbuild -clean
	rm -f KaSim
	find . -name \*~ -delete
