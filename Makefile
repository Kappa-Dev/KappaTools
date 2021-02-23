.DEFAULT_GOAL := all

include externals.mk

MANREP= man/
MANSCRIPTREP = $(MANREP)scripts/
MANKAPPAMODELSREP = $(MANREP)examples/
MANIMGREP = $(MANREP)img/
GENIMG = generated_img
MANGENREP = $(MANREP)$(GENIMG)/

RANDOM_NUMBER = $(shell bash -c 'echo $$RANDOM')

SCRIPTSSOURCE = $(wildcard $(MANSCRIPTREP)*.sh)
SCRIPTSWITNESS = $(SCRIPTSSOURCE:.sh=.witness) $(MANGENREP)version.tex
MODELS = $(wildcard $(MANKAPPAMODELSREP)*.ka)

RESOURCES_HTML=$(wildcard shared/*.js) $(wildcard viz/*.js) $(wildcard viz/*.css) gui/favicon.ico gui/package.json

APP_EXT?=cdn
INDEX_HTML=gui/use-$(APP_EXT).html
ifeq ($(APP_EXT),local)
SITE_EXTRAS= site/external site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist site/external/codemirror-$(CODEMIRROR_VERSION) site/external/dagre-d3 site/external/d3 site/external/jquery
else
SITE_EXTRAS=
endif

.PHONY: all agents clean check build-tests doc clean_doc debug
.PHONY: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: profiling Kappapp.app kappalib install-lib KappaBin.zip Kappapp.tar.gz

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

$(MANGENREP)version.tex: $(MANREP)version.tex.skel $(wildcard .git/refs/heads/*) $(MANGENREP)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

gui/Info.plist: gui/Info.plist.skel $(wildcard .git/refs/heads/*)
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

%.bc.js: $(filter-out _build/,$(wildcard */*.ml*))
	dune build $@

site/%.js: _build/default/gui/%.bc.js site
	sed 's/.process.argv.length>0/.process.argv.length>1/' $< > $@

site/index.html: $(INDEX_HTML) $(SITE_EXTRAS) site/JsSim.js site/KaSimWorker.js  site/KaSaWorker.js site/KaStorWorker.js site/KaMoHaWorker.js
	cat $< | sed "s/RANDOM_NUMBER/$(RANDOM_NUMBER)/g" | sed "s/JQUERY_VERSION/$(JQUERY_VERSION)/g" | sed "s/JQUERY_UI_VERSION/$(JQUERY_UI_VERSION)/g" |  sed "s/CODEMIRROR_VERSION/$(CODEMIRROR_VERSION)/g" | sed "s/BOOTSTRAP_VERSION/$(BOOTSTRAP_VERSION)/g" > $@

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

%.witness: %.sh $(MANGENREP) $(MODELS) %.gplot
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

%.witness: %.sh $(MANGENREP) $(MODELS)
	cd $(dir $@) && KAPPABIN="$(CURDIR)/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

doc: $(MANGENREP) man/KaSim_manual.pdf

doc_html: man/KaSim_manual.htm
	dune build --only-packages kappa-library,kappa-binaries @doc

profiling:
	@+$(MAKE) EXTRAFLAGS="-pkg landmarks.ppx -pkg landmarks" OCAML_LANDMARKS="auto,allocation" all

all:
	dune build --only-packages kappa-library,kappa-binaries

agents:
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents

clean_ide:
	rm -rf gui/Kappa.iconset
	rm -f gui/Kappa.icns gui/Info.plist
	rm -rf Kappapp.app Kappapp.app.zip
	rm -rf KappaBin KappaBin.zip Kappapp Kappapp.tar.gz
	rm -rf site
	rm -rf python/__pycache__/

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts -name \*.witness -delete
	rm -rf $(MANGENREP)

clean: clean_doc clean_ide
	dune clean
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C tests/integration clean

check:
	dune runtest
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C tests/integration clean
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C tests/integration all

build-tests:
	dune promote
	@+$(MAKE) KAPPABIN="$(CURDIR)/bin/" -C tests/integration build

# https://electronjs.org/docs/tutorial/application-distribution

Kappapp.tar.gz:
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	mkdir Kappapp
	FILE=$$(mktemp -t electronXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-linux-x64.zip && \
	unzip $$FILE -d Kappapp
	mv Kappapp/electron Kappapp/kappapp
	mv site Kappapp/resources/app
	mkdir Kappapp/resources/bin
	cp bin/* Kappapp/resources/bin/
	tar czf $@ Kappapp
	rm -r Kappapp

KappaBin.zip:
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build -x windows --only-packages kappa-library,kappa-binaries,kappa-agents
	mkdir KappaBin
	FILE=$$(mktemp -t electronXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-win32-x64.zip && \
	unzip $$FILE -d KappaBin
	mv site KappaBin/resources/app
	mv KappaBin/electron.exe KappaBin/Kappapp.exe
	mkdir KappaBin/resources/bin
	cp _build/default.windows/core/main/KaSim.exe KappaBin/resources/bin/
	cp _build/default.windows/core/KaSa_rep/main/KaSa.exe KappaBin/resources/bin/
	cp _build/default.windows/core/agents/KaStor.exe KappaBin/resources/bin/
	cp _build/default.windows/core/odes/KaDE.exe KappaBin/resources/bin/
	cp _build/default.windows/core/agents/KappaSwitchman.exe KappaBin/resources/bin/
	cp _build/default.windows/core/agents/KaMoHa.exe KappaBin/resources/bin/
	cp _build/default.windows/core/agents/KaSimAgent.exe KappaBin/resources/bin/
	cp _build/default.windows/core/agents/KaSaAgent.exe KappaBin/resources/bin/
	zip -y -r $@ KappaBin
	rm -r KappaBin

Kappapp.app: gui/Info.plist gui/Kappa.icns
	+$(MAKE) clean
	+$(MAKE) APP_EXT=local site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	+$(MAKE) gui/Kappa.icns gui/Info.plist
	FILE=$$(mktemp -t electronXXXX); FOLDER=$$(mktemp -t electron_unzipedXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-darwin-x64.zip && \
	rm $$FOLDER && mkdir -p $$FOLDER && pushd $$FOLDER && unzip $$FILE && popd && mv $$FOLDER/Electron.app $@ && rm -r $$FOLDER
	rm -r $@/Contents/Resources/*.lproj/
	mkdir $@/Contents/Resources/bin
	cp bin/* $@/Contents/Resources/bin/
	mv site $@/Contents/Resources/app/
	mv gui/Kappa.icns $@/Contents/Resources/
	mv gui/Info.plist $@/Contents/

gui/Kappa.iconset: gui/Kappa-Logo.png
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

gui/Kappa.icns: gui/Kappa.iconset
	iconutil -c icns $<
