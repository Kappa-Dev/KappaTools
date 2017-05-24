.DEFAULT_GOAL := all

include externals.mk

LABLTKLIBREP?=$(CAML_LD_LIBRARY_PATH)/../labltk

MANREP= man/
MANSCRIPTREP = $(MANREP)scripts/
MANKAPPAMODELSREP = $(MANREP)models/
MANIMGREP = $(MANREP)img/
GENIMG = generated_img
MANGENREP = $(MANREP)$(GENIMG)/

KASAREP = KaSa_rep/
KADEREP = odes/
RANDOM_NUMBER = $(shell bash -c 'echo $$RANDOM')

OCAMLBEST := $(shell which `ocamlfind opt -only-show` > /dev/null && echo native || echo byte)
OCAMLBUILDFLAGS = $(EXTRAFLAGS)

USE_TK?=0

ifeq ($(DEBUG),1)
JSOFOCAMLFLAGS = --debuginfo --pretty "+weak.js" "+nat.js"
else
JSOFOCAMLFLAGS = "+nat.js" "+weak.js"
endif

ifeq ($(USE_TK),1)
OCAMLINCLUDES = -pkg labltk -I KaSa_rep/lib/full -cflags -I,$(LABLTKLIBREP),-I,+labltk -lflags -I,$(LABLTKLIBREP),-I,+labltk -libs labltk,jpflib
else
OCAMLINCLUDES = -I KaSa_rep/lib/light
endif

SCRIPTSSOURCE = $(wildcard $(MANSCRIPTREP)*.sh)
SCRIPTSWITNESS = $(SCRIPTSSOURCE:.sh=.witness) $(MANGENREP)version.tex
MODELS = $(wildcard $(MANKAPPAMODELSREP)*.ka)

VERSION=generated/version.ml
RESOURCE=generated/resource_strings.ml
GENERATED=$(VERSION) \
	  $(RESOURCE) \
	  generated/api_types_t.ml generated/api_types_j.ml \
	  generated/mpi_message_t.ml generated/mpi_message_j.ml

RESOURCES_HTML=$(wildcard js/*.js) $(wildcard shared/*.js) \
		$(wildcard js/*.css) js/favicon.ico js/package.json

ifeq ($(NO_CDN),1)
SITE_EXTRAS= site/external site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist site/external/codemirror-$(CODEMIRROR_VERSION) site/external/d3 site/external/jquery
INDEX_HTML=js/no-cdn.html
else
SITE_EXTRAS=
INDEX_HTML=js/use-cdn.html
endif

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check build-tests doc clean_doc fetch_version KappaBin.zip debug
.PHONY: profiling Kappapp.app kappalib install-lib

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

generated:
	mkdir -p generated

generated/api_types_t.ml: api/api_types.atd generated
	atdgen -t -o generated/api_types api/api_types.atd

generated/api_types_j.ml: api/api_types.atd generated
	atdgen -j -j-std -o generated/api_types api/api_types.atd

generated/mpi_message_t.ml: api/mpi_message.atd generated
	atdgen -t -o generated/mpi_message api/mpi_message.atd

generated/mpi_message_j.ml: api/mpi_message.atd generated
	atdgen -j -j-std -o generated/mpi_message api/mpi_message.atd

$(RESOURCE): shared/flux.js shared/plot.js shared/common.js js/JsSim.css api/test_message.json
	./dev/generate-string.sh $^  > $@

$(VERSION): main/version.ml.skel $(wildcard .git/refs/heads/*) generated
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

$(MANGENREP)version.tex: $(MANREP)version.tex.skel $(wildcard .git/refs/heads/*) $(MANGENREP)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

META: META.skel $(wildcard .git/refs/heads/*)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

ide/Info.plist: ide/Info.plist.skel $(wildcard .git/refs/heads/*)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

%.cma %.cmxa %.native %.byte %.docdir/index.html: $(filter-out _build/,$(wildcard */*.ml*)) $(wildcard $(KASAREP)*/*.ml*) $(wildcard $(KADEREP)*/*.ml*) $(wildcard $(KASAREP)*/*/*.ml*) $(VERSION) $(RESOURCE)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

site: $(RESOURCES_HTML)
	mkdir -p $@
	cp $^ $@

site/external: site
	mkdir -p $@

site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist: externals.mk
	FILE=$$(mktemp -t bootstrapXXXX); \
	curl -LsS -o $$FILE https://github.com/twbs/bootstrap/releases/download/v$(BOOTSTRAP_VERSION)/bootstrap-$(BOOTSTRAP_VERSION)-dist.zip && \
	rm -rf $@ && unzip -d $(dir $@) $$FILE && rm $$FILE
	touch $@

site/external/codemirror-$(CODEMIRROR_VERSION): externals.mk
	FILE=$$(mktemp -t codemirrorXXXX); \
	curl -LsS -o $$FILE http://codemirror.net/codemirror-$(CODEMIRROR_VERSION).zip &&\
	rm -rf $@ && unzip -d $(dir $@) $$FILE && rm $$FILE
	touch $@

site/external/d3: externals.mk
	mkdir -p $@
	curl -LsS -o $@/d3.v4.min.js http://d3js.org/d3.v4.min.js

site/external/jquery: externals.mk
	mkdir -p $@
	curl -LsS -o site/external/jquery/jquery.js https://code.jquery.com/jquery-$(JQUERY_VERSION).min.js
	curl -LsS -o site/external/jquery/jquery-ui.min.js http://code.jquery.com/ui/$(JQUERY_UI_VERSION)/jquery-ui.min.js

site/JsSim.js: JsSim.byte site
	js_of_ocaml $(JSOFOCAMLFLAGS) _build/js/$< -o $@
	sed -i.bak 's/.process.argv.length>0/.process.argv.length>1/' site/JsSim.js

site/WebWorker.js: WebWorker.byte site
	js_of_ocaml $(JSOFOCAMLFLAGS) _build/js/$< -o $@

ounit: TestJsSim TestWebSim

TestJsSim: TestJsSim.byte
	./TestJsSim.byte
TestWebSim: TestWebSim.byte
	./TestWebSim.byte -runner sequential

site/index.html: $(INDEX_HTML) $(SITE_EXTRAS) site/JsSim.js site/WebWorker.js
	cat $< | ./dev/embed-file.sh | sed "s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g" | sed "s/JQUERY_VERSION/$(JQUERY_VERSION)/g" | sed "s/JQUERY_UI_VERSION/$(JQUERY_UI_VERSION)/g" |  sed "s/CODEMIRROR_VERSION/$(CODEMIRROR_VERSION)/g" | sed "s/BOOTSTRAP_VERSION/$(BOOTSTRAP_VERSION)/g" > $@

JsSim.byte: $(filter-out _build/,$(wildcard */*.ml*)) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<js/*> : thread, package(atdgen), package(js_of_ocaml.tyxml), package(lwt_react)" \
	-tag-line "<js/*.ml*> : package(js_of_ocaml.ppx), package(tyxml.ppx)" \
	$@

TestJsSim.byte: $(filter-out webapp/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen), package(qcheck.ounit)" \
	-tag-line "<js/*> : thread, package(qcheck.ounit), package(atdgen), package(js_of_ocaml.ppx), package(lwt_react)" \
	$@

TestWebSim.byte: $(filter-out webapp/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I webapp -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen), package(qcheck.ounit)" \
	-tag-line "<webapp/*> : thread, package(atdgen), package(qcheck.ounit), package(cohttp.lwt), package(re), package(re.perl)" \
	$@

WebWorker.byte: $(filter-out webapp/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<js/*> : thread, package(atdgen), package(js_of_ocaml), package(lwt)" \
	$@

WebSim.native WebSim.byte: $(filter-out js/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-I webapp -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<webapp/*> : thread, package(atdgen), package(cohttp.lwt), package(re), package(re.perl)" \
	$@

StdSim.native StdSim.byte: $(filter-out js/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-I webapp -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<webapp/*> : thread, package(lwt),package(lwt.unix),package(atdgen)" \
	$@

bin/%: %.$(OCAMLBEST) Makefile
	[ -d bin ] || mkdir bin && strip -o $@ $<
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

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa bin/KaStor bin/KaDE $(MODELS) %.gplot
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa bin/KaStor bin/KaDE $(MODELS)
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

doc: $(MANGENREP) man/KaSim_manual.pdf
doc_html: dev/KaSim.docdir/index.html man/KaSim_manual.htm

debug:
	@+$(MAKE) EXTRAFLAGS="-tag debug" KaSim.byte KaDE.byte KaStor.byte WebSim.byte KappaLib.cma dev/db_printers.cma

profiling:
	@+$(MAKE) EXTRAFLAGS="-pkg landmarks.ppx -pkg landmarks" OCAML_LANDMARKS="auto,allocation" all

all: bin/KaSim bin/KaSa bin/KaStor bin/KaDE

kappalib: KappaLib.cma
ifeq ($(OCAMLBEST),native)
	@+$(MAKE) KappaLib.cmxa
endif

install-lib:
	ocamlfind install KappaLib META _build/KappaLib.cma $(wildcard _build/*/*.cmi) $(wildcard _build/*/*.mli) -optional _build/KappaLib.cmxa _build/KappaLib.a $(wildcard _build/*/*.cmx)

clean_ide:
	rm -f StdSim bin/StdSim
	rm -rf ide/Kappa.iconset
	rm -f ide/Kappa.icns ide/Info.plist
	rm -rf Kappapp.app

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts -name \*.witness -delete
	rm -rf $(MANGENREP)

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch clean_doc clean_ide
	"$(OCAMLBINPATH)ocamlbuild" -clean
	rm -f $(VERSION) $(RESOURCE)
	rm -f sanity_test bin/sanity_test
	rm -f KaSim bin/KaSim KaSa bin/KaSa WebSim bin/WebSim KaStor bin/KaStor
	rm -f KaDE bin/KaDE META KappaLib.cm*
	rm -rf KappaBin KappaBin.zip
	rm -rf site generated
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean

check:
	@+$(MAKE) bin/sanity_test
	@+$(MAKE) bin/KaSa_json
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite all

build-tests: bin/sanity_test
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite build

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli

KappaBin.zip:
	+$(MAKE) clean
	+$(MAKE) NO_CDN=1 site/index.html
	+$(MAKE) OCAMLFIND_TOOLCHAIN=windows KaSim.native KaSa.native KaStor.native KaDE.native StdSim.native
	mkdir KappaBin
	mkdir KappaBin/bin
	mv site KappaBin/package.nw
	mv _build/main/KaSim.native KappaBin/bin/KaSim.exe
	mv _build/KaSa_rep/main/KaSa.native KappaBin/bin/KaSa.exe
	mv _build/cflow/KaStor.native KappaBin/bin/KaStor.exe
	mv _build/odes/KaDE.native KappaBin/bin/KaDE.exe
	mv _build/webapp/StdSim.native KappaBin/bin/StdSim.exe
	zip -r $@ KappaBin
	rm -r KappaBin

Kappapp.app:
	+$(MAKE) clean
	+$(MAKE) NO_CDN=1 site/index.html
	+$(MAKE) all bin/StdSim
	+$(MAKE) ide/Kappa.icns ide/Info.plist
	FILE=$$(mktemp -t nwjsXXXX); \
	curl -LsS -o $$FILE https://dl.nwjs.io/v$(NWJS_VERSION)/nwjs-v$(NWJS_VERSION)-osx-x64.zip && \
	unzip $$FILE && rm -f $$FILE
	mv nwjs-v$(NWJS_VERSION)-osx-x64/nwjs.app $@
	rm -r nwjs-v$(NWJS_VERSION)-osx-x64/
	rm -r $@/Contents/Resources/*.lproj/
	mv bin $@/Contents/Resources/
	mv site $@/Contents/Resources/app.nw
	mv ide/Kappa.icns $@/Contents/Resources/
	mv ide/Info.plist $@/Contents/

full:
	$(MAKE) clean
	$(MAKE) USE_TK=1 || echo 0

light:
	$(MAKE) clean
	$(MAKE) USE_TK=0 || echo 0

ide/Kappa.iconset: ide/Kappa-Logo.png
	rm -rf $@ && mkdir $@
	sips -z 16 16     $< --out $@/icon_16x16.png
	sips -z 32 32     $< --out $@/icon_16x16@2x.png
	sips -z 32 32     $< --out $@/icon_32x32.png
	sips -z 64 64     $< --out $@/icon_32x32@2x.png
	sips -z 128 128   $< --out $@/icon_128x128.png
	sips -z 256 256   $< --out $@/icon_128x128@2x.png
	sips -z 256 256   $< --out $@/icon_256x256.png
	sips -z 512 512   $< --out $@/icon_256x256@2x.png
	sips -z 512 512   $< --out $@/icon_512x512.png
	cp $< $@/icon_512x512@2x.png
ide/Kappa.icns: ide/Kappa.iconset
	iconutil -c icns $<
