.DEFAULT_GOAL := all
TEMPDIR := $(shell mktemp -d)
LABLTKLIBREP?=$(CAML_LD_LIBRARY_PATH)/../labltk

MANREP= man/
MANSCRIPTREP = $(MANREP)scripts/
MANKAPPAMODELSREP = $(MANREP)models/
MANIMGREP = $(MANREP)img/
GENIMG = generated_img
MANGENREP = $(MANREP)$(GENIMG)/

KASAREP = KaSa_rep/
RANDOM_NUMBER = $(shell bash -c 'echo $$RANDOM')
TERM = $(shell echo $$TERM)

OCAMLBUILDFLAGS = $(EXTRAFLAGS)

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
VERSION=generated/version.ml
RESOURCE=generated/resource_strings.ml
GENERATED=$(VERSION) \
	  $(RESOURCE) \
	  generated/ApiTypes_t.ml generated/ApiTypes_j.ml \
	  generated/WebMessage_t.ml generated/WebMessage_j.ml

generated:
	mkdir -p generated

generated/ApiTypes_t.ml: api/ApiTypes.atd generated
	atdgen -t -o generated/ApiTypes api/ApiTypes.atd

generated/ApiTypes_j.ml: api/ApiTypes.atd generated
	atdgen -j -j-std -o generated/ApiTypes api/ApiTypes.atd

generated/WebMessage_t.ml: js/WebMessage.atd generated
	atdgen -t -o generated/WebMessage js/WebMessage.atd

generated/WebMessage_j.ml: js/WebMessage.atd generated
	atdgen -j -j-std -o generated/WebMessage js/WebMessage.atd

$(RESOURCE): shared/flux.js shared/plot.js shared/common.js
	./dev/generate-string.sh $^  > $@

$(VERSION): main/version.ml.skel $(wildcard .git/refs/heads/*) generated
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

%.cma %.native %.byte %.docdir/index.html: $(filter-out _build/,$(wildcard */*.ml*)) $(wildcard $(KASAREP)*/*.ml*) $(wildcard $(KASAREP)*/*/*.ml*) $(VERSION) $(RESOURCE)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

site:
	mkdir site

site/external: site
ifeq ($(NO_CDN),1)
	curl -LsS -o $(TEMPDIR)/bootstrap.zip   https://github.com/twbs/bootstrap/releases/download/v3.3.5/bootstrap-3.3.5-dist.zip ;\
	curl -LsS -o $(TEMPDIR)/codemirror.zip  http://codemirror.net/codemirror.zip ;\
	mkdir -p $@ ;\
	unzip -d $@ $(TEMPDIR)/bootstrap.zip ;\
	unzip -d $@ $(TEMPDIR)/codemirror.zip ;\
	mkdir -p $@/jquery ;\
	curl -LsS -o $@/jquery/jquery.js https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.1/jquery.js ;\
	mkdir -p $@/d3 ;\
	curl -LsS -o $@/d3/d3.v3.min.js http://d3js.org/d3.v3.min.js
else
endif


site/JsSim.js: JsSim.byte site/external
	js_of_ocaml --debuginfo --pretty "+weak.js" "+nat.js" _build/js/$< -o $@

site/WebWorker.js: WebWorker.byte
	js_of_ocaml --debuginfo --pretty "+nat.js" _build/js/$< -o $@

test:
	cat js/no-cdn.html | sed s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g > site/index.html

site/index.html: site js/no-cdn.html js/use-cdn.html site/JsSim.js site/WebWorker.js js/*.js shared/*.js js/favicon.ico js/*.css
ifeq ($(NO_CDN),1)
	cat js/no-cdn.html | sed "s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g" > site/index.html
else
	cat js/use-cdn.html | sed "s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g" > site/index.html
endif
	cp shared/*.js site
	cp -f js/*.js js/*.css js/favicon.ico site

JsSim.byte: $(filter-out _build/,$(wildcard */*.ml*)) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt)" \
	-tag-line "<js/*> : thread, package(atdgen), package(js_of_ocaml.tyxml), package(js_of_ocaml.syntax), package(tyxml.syntax), package(lwt), syntax(camlp4o)" \
	$@

WebWorker.byte: $(filter-out webapp/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt)" \
	-tag-line "<js/*> : thread, package(atdgen), package(js_of_ocaml), package(js_of_ocaml.syntax), package(lwt), syntax(camlp4o)" \
	$@

WebSim.native: $(filter-out js/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I webapp -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt)" \
	-tag-line "<webapp/*> : thread, package(atdgen), package(cohttp.lwt), package(re), package(re.perl)" \
	$@

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
	rm -f $(VERSION) $(RESOURCE)
	rm -f sanity_test bin/sanity_test
	rm -f KaSim bin/KaSim KaSa bin/KaSa
	rm -rf site generated
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
