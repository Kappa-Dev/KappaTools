.DEFAULT_GOAL := all

TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display
else
 OCAMLBUILDFLAGS =
endif

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch

%.native %.byte: $(filter-out _build/,$(wildcard */*.ml*))
	ocamlbuild $(OCAMLBUILDFLAGS) $@

all: KaSim.native
	rm -f KaSim && ln -s _build/main/KaSim.native KaSim

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
	ocamlbuild -clean
	rm -f KaSim
	find . -name \*~ -delete

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappa{Lexer.ml,Parser.ml{,i}}
