.DEFAULT_GOAL := all



TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display 
else
 OCAMLBUILDFLAGS = 
endif

USE_TK=0

ifeq ($(USE_TK),1)
OCAMLINCLUDES = -cflags -I,+labltk,-I,+lablgtk2 -lflags -I,+labltk,-I,+lablgtk2,unix.cmxa,str.cmxa,nums.cmxa,labltk.cmxa,jpflib.cmxa,frxlib.cmxa
else
OCAMLINCLUDES = -lflags unix.cmxa,str.cmxa,nums.cmxa
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


full: 
	@cp Makefile Makefile.tmp
	@cp _tags _tags.tmp
	make clean
	@sh switch_full.sh 
	make 
	@cp Makefile.tmp Makefile
	@cp _tags.tmp _tags 

light: 
	@cp Makefile Makefile.tmp
	@cp _tags _tags.tmp
	make clean
	@sh switch_light.sh
	make 
	@cp Makefile.tmp Makefile
	@cp _tags.tmp _tags
