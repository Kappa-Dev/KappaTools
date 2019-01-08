.DEFAULT_GOAL := all

include externals.mk

MANREP= man/
MANSCRIPTREP = $(MANREP)scripts/
MANKAPPAMODELSREP = $(MANREP)models/
MANIMGREP = $(MANREP)img/
GENIMG = generated_img
MANGENREP = $(MANREP)$(GENIMG)/

KASAREP = KaSa_rep/
RANDOM_NUMBER = $(shell bash -c 'echo $$RANDOM')

OCAMLBEST := $(shell which `ocamlfind opt -only-show` > /dev/null && echo native || echo byte)
OCAMLBUILDFLAGS = $(EXTRAFLAGS)

ifeq ($(DEBUG),1)
JSOFOCAMLFLAGS = --debuginfo --sourcemap --pretty "+nat.js"
else
JSOFOCAMLFLAGS = "+nat.js"
endif

SCRIPTSSOURCE = $(wildcard $(MANSCRIPTREP)*.sh)
SCRIPTSWITNESS = $(SCRIPTSSOURCE:.sh=.witness) $(MANGENREP)version.tex
MODELS = $(wildcard $(MANKAPPAMODELSREP)*.ka)

RESOURCES_HTML=$(wildcard _build/default/js/*.js) $(wildcard shared/*.js) $(wildcard viz/*.js) \
		$(wildcard viz/*.css) js/favicon.ico js/package.json

APP_EXT?=cdn
INDEX_HTML=js/use-$(APP_EXT).html
ifeq ($(APP_EXT),local)
SITE_EXTRAS= site/external site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist site/external/codemirror-$(CODEMIRROR_VERSION) site/external/dagre-d3 site/external/d3 site/external/jquery
else
SITE_EXTRAS=
endif

.PHONY: all agents clean check build-tests doc clean_doc fetch_version debug
.PHONY: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: profiling Kappapp.app kappalib install-lib KappaBin.zip Kappapp.tar.gz

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

$(MANGENREP)version.tex: $(MANREP)version.tex.skel $(wildcard .git/refs/heads/*) $(MANGENREP)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

ide/Info.plist: ide/Info.plist.skel $(wildcard .git/refs/heads/*)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

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

site/external/dagre-d3: externals.mk
	mkdir -p $@
	curl -LsS -o $@/dagre-d3.min.js https://dagrejs.github.io/project/dagre-d3/latest/dagre-d3.min.js

site/external/jquery: externals.mk
	mkdir -p $@
	curl -LsS -o site/external/jquery/jquery.js https://code.jquery.com/jquery-$(JQUERY_VERSION).min.js
	curl -LsS -o site/external/jquery/jquery-ui.min.js http://code.jquery.com/ui/$(JQUERY_UI_VERSION)/jquery-ui.min.js

site/index.html: $(INDEX_HTML) $(SITE_EXTRAS) site/JsSim.js site/WebWorker.js  site/KaSaWorker.js site/KaStorWorker.js
	cat $< | sed "s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g" | sed "s/JQUERY_VERSION/$(JQUERY_VERSION)/g" | sed "s/JQUERY_UI_VERSION/$(JQUERY_UI_VERSION)/g" |  sed "s/CODEMIRROR_VERSION/$(CODEMIRROR_VERSION)/g" | sed "s/BOOTSTRAP_VERSION/$(BOOTSTRAP_VERSION)/g" > $@

JsSim.byte: $(filter-out _build/,$(wildcard */*.ml*)) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<js/*> : package(atdgen), package(js_of_ocaml.tyxml), package(js_of_ocaml-lwt), package(lwt_react), open(Js_of_ocaml)" \
	-tag-line "<js/*.ml*> : package(js_of_ocaml.ppx), package(tyxml-ppx)" \
	$@

%Worker.byte: $(filter-out webapp/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-tag debug -I js -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<js/*> : package(atdgen), package(js_of_ocaml.ppx), package(js_of_ocaml-lwt), open(Js_of_ocaml)" \
	$@

WebSim.native WebSim.byte: $(filter-out js/,$(filter-out _build/,$(wildcard */*.ml*))) $(GENERATED)
	"$(OCAMLBINPATH)ocamlbuild" $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) \
	-I webapp -I api \
	-tag-line "<generated/*> : package(atdgen)" \
	-tag-line "<api/*> : package(lwt),package(atdgen)" \
	-tag-line "<agents/*> : package(atdgen)" \
	-tag-line "<webapp/*> : thread, package(atdgen), package(cohttp-lwt-unix)" \
	$@

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
	for file in *_img/*.pdf ; do pdftoppm $${file} $${file%.pdf} -png -singlefile ; done && \
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

doc_html: man/KaSim_manual.htm
	dune build --only-packages kappa-library,kappa-binaries @doc

debug:
	@+$(MAKE) EXTRAFLAGS="-tag debug" KappaLib.cma dev/db_printers.cma KaSim.byte KaDE.byte KaStor.byte KaSa.byte

profiling:
	@+$(MAKE) EXTRAFLAGS="-pkg landmarks.ppx -pkg landmarks" OCAML_LANDMARKS="auto,allocation" all

all:
	dune build --only-packages kappa-library,kappa-binaries

agents:
	dune build --only-packages kappa-library,kappa-agents

kappalib: KappaLib.cma
ifeq ($(OCAMLBEST),native)
	@+$(MAKE) KappaLib.cmxa
endif

install-lib:
	ocamlfind install KappaLib META _build/KappaLib.cma $(wildcard _build/*/*.cmi) $(wildcard _build/*/*.mli) -optional _build/KappaLib.cmxa _build/KappaLib.a $(wildcard _build/*/*.cmx)

clean_ide:
	rm -f KaSimAgent bin/KaSimAgent
	rm -f KaSaAgent bin/KaSaAgent
	rm -rf ide/Kappa.iconset
	rm -f ide/Kappa.icns ide/Info.plist
	rm -rf Kappapp.app Kappapp.app.zip
	rm -rf KappaBin KappaBin.zip Kappapp Kappapp.tar.gz
	rm -rf site
	rm -rf python/__pycache__/

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts -name \*.witness -delete
	rm -rf $(MANGENREP)

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch clean_doc clean_ide
	dune clean
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean

check:
	@+$(MAKE) bin/sanity_test
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite clean
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite all

build-tests: bin/sanity_test
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C models/test_suite build

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli

Kappapp.tar.gz:
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	FILE=$$(mktemp -t nwjsXXXX); \
	curl -LsS -o $$FILE https://dl.nwjs.io/v$(NWJS_VERSION)/nwjs-v$(NWJS_VERSION)-linux-x64.tar.gz && \
	tar xzf $$FILE && rm -f $$FILE
	mkdir Kappapp
	mv nwjs-v$(NWJS_VERSION)-linux-x64/* Kappapp/
	rmdir nwjs-v$(NWJS_VERSION)-linux-x64
	mv Kappapp/nw Kappapp/kappapp
	mv site Kappapp/package.nw
	mkdir Kappapp/bin
	cp bin/* Kappapp/bin/
	tar czf $@ Kappapp
	rm -r Kappapp

KappaBin.zip:
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build -x windows --only-packages kappa-library,kappa-binaries,kappa-agents
	mkdir KappaBin
	mkdir KappaBin/bin
	mv site KappaBin/package.nw
	FILE=$$(mktemp -t nwjsXXXX); \
	curl -LsS -o $$FILE https://dl.nwjs.io/v$(NWJS_VERSION)/nwjs-v$(NWJS_VERSION)-win-x64.zip && \
	unzip $$FILE && rm -f $$FILE
	mv nwjs-v$(NWJS_VERSION)-win-x64/* KappaBin/
	rmdir nwjs-v$(NWJS_VERSION)-win-x64
	mv KappaBin/nw.exe KappaBin/Kappapp.exe
	cp _build/default.windows/main/KaSim.exe KappaBin/bin/
	cp _build/default.windows/KaSa_rep/main/KaSa.exe KappaBin/bin/
	cp _build/default.windows/agents/KaStor.exe KappaBin/bin/
	cp _build/default.windows/odes/KaDE.exe KappaBin/bin/
	cp _build/default.windows/agents/KaSimAgent.exe KappaBin/bin/
	cp _build/default.windows/agents/KaSaAgent.exe KappaBin/bin/
	zip -y -r $@ KappaBin
	rm -r KappaBin

Kappapp.app:
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	+$(MAKE) ide/Kappa.icns ide/Info.plist
	FILE=$$(mktemp -t nwjsXXXX); \
	curl -LsS -o $$FILE https://dl.nwjs.io/v$(NWJS_VERSION)/nwjs-v$(NWJS_VERSION)-osx-x64.zip && \
	unzip $$FILE && rm -f $$FILE
	mv nwjs-v$(NWJS_VERSION)-osx-x64/nwjs.app $@
	rm -r nwjs-v$(NWJS_VERSION)-osx-x64/
	rm -r $@/Contents/Resources/*.lproj/
	mkdir $@/Contents/Resources/bin
	cp bin/* $@/Contents/Resources/bin/
	mv site $@/Contents/Resources/app.nw
	mv ide/Kappa.icns $@/Contents/Resources/
	mv ide/Info.plist $@/Contents/

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
