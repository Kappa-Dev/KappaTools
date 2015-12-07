.DEFAULT_GOAL := all

LABLTKLIBREP?=$(CAML_LD_LIBRARY_PATH)/../labltk

MANREP= man/
MANSCRIPTREP = $(MANREP)scripts/
MANKAPPAMODELSREP = $(MANREP)models/
MANIMGREP = $(MANREP)img/
GENIMG = generated_img
MANGENREP = $(MANREP)$(GENIMG)/

KASAREP = KaSa_rep/

TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb) # An approximation of "am I launched from emacs ?" :-)
 OCAMLBUILDFLAGS = -classic-display $(EXTRAFLAGS)
else
 OCAMLBUILDFLAGS = $(EXTRAFLAGS)
endif

USE_TK?=0

ifeq ($(USE_TK),1)
OCAMLINCLUDES = -pkg labltk -I KaSa_rep/lib/full -cflags -I,$(LABLTKLIBREP),-I,+labltk -lflags -I,$(LABLTKLIBREP),-I,+labltk -libs labltk,jpflib
else
OCAMLINCLUDES = -I KaSa_rep/lib/light
endif

SCRIPTSSOURCE = $(wildcard $(MANSCRIPTREP)*.sh)
SCRIPTSWITNESS = $(SCRIPTSSOURCE:.sh=.witness)
MODELS = $(wildcard $(MANKAPPAMODELSREP)*.ka)

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check build-tests doc clean_doc fetch_version on_linux_for_windows debug

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

main/version.ml: main/version.ml.skel $(wildcard .git/refs/heads/*)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

%.cma %.native %.byte %.docdir/index.html: main/version.ml $(filter-out _build/,$(wildcard */*.ml*)) $(wildcard $(KASAREP)*/*.ml*) $(wildcard $(KASAREP)*/*/*.ml*)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

JaSim.byte: $(filter-out _build/,$(wildcard */*.ml*))
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) -I js \
	-tag-line "<js/**> : thread, package(js_of_ocaml.tyxml), package(js_of_ocaml.syntax), package(tyxml.syntax), package(lwt), syntax(camlp4o)" \
	$@

js/JaSim.js: JaSim.byte
	js_of_ocaml "+weak.js" "+nat.js" _build/js/$< -o $@

bin/%: %.native Makefile
	[ -d bin ] || mkdir bin && cp $< $@
	rm -f $(notdir $@) && ln -s $@ $(notdir $@)


%.pdf: %.tex $(SCRIPTSWITNESS)
	cd $(dir $<) && LOG=$$(mktemp -t pdflatexlogXXXX); rm -f *.aux && \
	pdflatex -halt-on-error $(notdir $<) > $${LOG} 2>&1 && \
	bibtex $(basename $(notdir $<)) >> $${LOG} 2>&1 && \
	makeindex $(basename $(notdir $<)).idx >> $${LOG} 2>&1 && \
	pdflatex -halt-on-error $(notdir $<) >> $${LOG} 2>&1 && \
	pdflatex -halt-on-error $(notdir $<) >> $${LOG} 2>&1 && \
	rm $${LOG} || { cat $${LOG}; rm $${LOG}; exit 2; }

%.htm: %.tex %.pdf
	cd $(dir $<) && LOG=$$(mktemp -t htlatexlogXXXX); \
	htlatex $(notdir $<)  "nma.cfg,htm,charset=utf-8,p-width" \
	" -cunihtf -utf8" "" "-halt-on-error"> $${LOG} 2>&1 && \
	rm $${LOG} || { cat $${LOG}; rm $${LOG}; exit 2; }

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa $(MODELS) %.gplot
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa $(MODELS)
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

doc: man/KaSim_manual.pdf
doc_html: dev/KaSim.docdir/index.html man/KaSim_manual.htm

debug:
	@+$(MAKE) EXTRAFLAGS="-tag debug" KaSim.byte dev/db_printers.cma

all: bin/KaSim bin/KaSa

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts \( -name \*.witness \) -delete
	rm -rf $(MANGENREP)

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch clean_doc
	"$(OCAMLBINPATH)ocamlbuild" -clean
	rm -f main/version.ml
	rm -f sanity_test bin/sanity_test
	rm -f KaSim bin/KaSim KaSa bin/KaSa
	rm -f js/JaSim.js
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean

check: bin/sanity_test
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite all

build-tests: bin/sanity_test
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite build

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli

on_linux_for_windows:
	$(MAKE) clean
	$(MAKE) OCAMLFIND_CONF=/etc/x86_64-w64-mingw32-ocamlfind.conf KaSim.native KaSa.native
	mv _build/main/KaSim.native KaSim.exe
	mv _build/KaSa_rep/main/KaSa.native KaSa.exe

full:
	$(MAKE) clean
	$(MAKE) USE_TK=1 || echo 0

light:
	$(MAKE) clean
	$(MAKE) USE_TK=0 || echo 0
