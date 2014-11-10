.DEFAULT_GOAL := all

TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display
else
 OCAMLBUILDFLAGS =
endif

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check

%.native %.byte: $(filter-out _build/,$(wildcard */*.ml*))
	ocamlbuild $(OCAMLBUILDFLAGS) $@

all: KaSim.native
	[ -d bin ] || mkdir bin && cp _build/main/KaSim.native bin/KaSim
	rm -f KaSim && ln -s bin/KaSim KaSim

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
	ocamlbuild -clean
	rm -f KaSim bin/KaSim
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/cflows clean

check:
	+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/cflows

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli
