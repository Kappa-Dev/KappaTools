.DEFAULT_GOAL := all

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch

%:
	ocamlbuild -classic-display $@

all: KaSim.native
	rm -f KaSim && ln -s _build/KaSim.native KaSim

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
	ocamlbuild -clean
	rm -f KaSim
	find . -name \*~ -delete

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappa{Lexer.ml,Parser.ml{,i}}
