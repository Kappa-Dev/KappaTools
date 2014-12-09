.DEFAULT_GOAL := all

OCAMLINCLUDES=-cflags -I,+labltk,-I,+lablgtk2 -lflags -I,+labltk,-I,+lablgtk2,unix.cmxa,str.cmxa,nums.cmxa,labltk.cmxa,jpflib.cmxa,frxlib.cmxa

TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display 
else
 OCAMLBUILDFLAGS = 
endif

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check build-tests

%.native %.byte: $(filter-out _build/,$(wildcard */*.ml*)) $(filter_out _build/,$(wildcard */*/*.ml*)) $(filter_out _build/,$(wildcard */*/*/*.ml*))
	ocamlbuild $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

all: KaSim.native KaSa.native 
	[ -d bin ] || mkdir bin && cp _build/main/KaSim.native bin/KaSim
	rm -f KaSim && ln -s bin/KaSim KaSim
	[ -d bin ] || mkdir bin && cp _build/KaSa_rep/main/KaSa.native bin/KaSa
	rm -f KaSa && ln -s bin/KaSa KaSa

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
	ocamlbuild -clean
	rm -f KaSim bin/KaSim KaSa bin/KaSa
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/cflows clean

check:
	@+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/cflows all

build-tests:
	@+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/cflows build

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli
