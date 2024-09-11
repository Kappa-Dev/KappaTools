DEFAULT_GOAL := all

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

RESOURCES_HTML=$(wildcard gui/shared/*.js) $(wildcard gui/viz/*.js) $(wildcard gui/viz/*.css) gui/favicon.ico gui/package.json

APP_EXT?=cdn
INDEX_HTML=gui/use-$(APP_EXT).html
ifeq ($(APP_EXT),local)
SITE_EXTRAS= build/site/external build/site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist build/site/external/codemirror-$(CODEMIRROR_VERSION) build/site/external/dagre-d3 build/site/external/d3 build/site/external/jquery
else
SITE_EXTRAS=
endif

.PHONY: all agents clean check build-tests doc clean_doc debug
.PHONY: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: profiling kappalib install-lib
.PHONY: Kappapp Kappapp.tar.gz KappaBin KappaBin.zip Kappapp.app

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

$(MANGENREP)version.tex: $(MANREP)version.tex.skel $(wildcard .git/refs/heads/*) $(MANGENREP)
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

build/site: $(RESOURCES_HTML)
	mkdir -p $@
	cp $^ $@

build/site/external: build/site
	mkdir -p $@

build/site/external/bootstrap-$(BOOTSTRAP_VERSION)-dist: externals.mk
	mkdir -p build/site/external
	FILE=$$(mktemp -t bootstrapXXXX); \
	curl -LsS -o $$FILE https://github.com/twbs/bootstrap/releases/download/v$(BOOTSTRAP_VERSION)/bootstrap-$(BOOTSTRAP_VERSION)-dist.zip && \
	rm -rf $@ && unzip -d $(dir $@) $$FILE && rm $$FILE
	touch $@

build/site/external/codemirror-$(CODEMIRROR_VERSION): externals.mk
	mkdir -p build/site/external
	FILE=$$(mktemp -t codemirrorXXXX); \
	curl -LsS -o $$FILE http://codemirror.net/codemirror-$(CODEMIRROR_VERSION).zip &&\
	rm -rf $@ && unzip -d $(dir $@) $$FILE && rm $$FILE
	touch $@

build/site/external/d3: externals.mk
	mkdir -p $@
	curl -LsS -o $@/d3.v4.min.js http://d3js.org/d3.v4.min.js

build/site/external/dagre-d3: externals.mk
	mkdir -p $@
	curl -LsS -o $@/dagre-d3.min.js https://dagrejs.github.io/project/dagre-d3/latest/dagre-d3.min.js

build/site/external/jquery: externals.mk
	mkdir -p $@
	curl -LsS -o build/site/external/jquery/jquery.js https://code.jquery.com/jquery-$(JQUERY_VERSION).min.js
	curl -LsS -o build/site/external/jquery/jquery-ui.min.js http://code.jquery.com/ui/$(JQUERY_UI_VERSION)/jquery-ui.min.js

%.bc.js: $(filter-out _build/,$(wildcard */*.ml*))
	dune build $@

build/site/%.js: _build/default/gui/%.bc.js build/site
	mkdir -p build/site
	sed 's/.process.argv.length>0/.process.argv.length>1/' $< > $@

build/site/index.html: $(INDEX_HTML) $(SITE_EXTRAS) build/site/JsSim.js build/site/KaSimWorker.js  build/site/KaSaWorker.js build/site/KaStorWorker.js build/site/KaMoHaWorker.js
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
	cd $(dir $@) && KAPPABIN="$(CURDIR)/_build/install/default/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

%.witness: %.sh $(MANGENREP) $(MODELS)
	cd $(dir $@) && KAPPABIN="$(CURDIR)/_build/install/default/bin/" sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

doc: $(MANGENREP) man/KaSim_manual.pdf

doc_html: man/KaSim_manual.htm
	dune build --only-packages kappa-library,kappa-binaries @doc

profiling:
	@+$(MAKE) EXTRAFLAGS="-pkg landmarks.ppx -pkg landmarks" OCAML_LANDMARKS="auto,allocation" all

all:
	dune build --only-packages kappa-library,kappa-binaries @install
	dune build @install
	# Note: removed ./bin symlink used by kappy, as it caused build issues with dune deleting files
	mkdir -p bin
	cp -f _build/install/default/bin/* bin/

agents:
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents @install

clean_ide:
	rm -rf build/
	rm -rf python/__pycache__/

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts -name \*.witness -delete
	rm -rf $(MANGENREP)

clean: clean_doc clean_ide
	rm -rf build
	dune clean
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN="$(CURDIR)/_build/install/default/bin/" -C tests/integration clean

# Run all tests
check:
	dune runtest
	+$(MAKE) --no-print-directory KAPPABIN="$(CURDIR)/_build/install/default/bin/" -C tests/integration clean
	+$(MAKE) KAPPABIN="$(CURDIR)/_build/install/default/bin/" -C tests/integration all

# Change test results to match current implementation
build-tests:
	dune promote
	+$(MAKE) KAPPABIN="$(CURDIR)/_build/install/default/bin/" -C tests/integration build

# https://electronjs.org/docs/tutorial/application-distribution

build/Kappapp:
	mkdir -p build/Kappapp
	+$(MAKE) APP_EXT=local build/site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	FILE=$$(mktemp -t electronXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-linux-x64.zip && \
	unzip $$FILE -d build/Kappapp
	mv build/Kappapp/electron build/Kappapp/kappapp
	mv build/site build/Kappapp/resources/app
	mkdir build/Kappapp/resources/bin
	cp _build/install/default/bin/* build/Kappapp/resources/bin/

Kappapp:
	# make clean before to be sure things are rebuilt   TODO improve?
	+$(MAKE) clean
	+$(MAKE) build/Kappapp

build/Kappapp.tar.gz: build/Kappapp
	tar czf $@ build/Kappapp
	rm -r build/Kappapp

Kappapp.tar.gz: build/Kappapp.tar.gz

build/KappaBin:
	mkdir -p build/KappaBin
	+$(MAKE) APP_EXT=local build/site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	FILE=$$(mktemp -t electronXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-win32-x64.zip && \
	unzip $$FILE -d build/KappaBin
	mv build/site build/KappaBin/resources/app
	mv build/KappaBin/electron.exe build/KappaBin/Kappapp.exe
	mkdir build/KappaBin/resources/bin
	cp _build/default/core/main/KaSim.exe build/KappaBin/resources/bin/
	cp _build/default/core/KaSa_rep/main/KaSa.exe build/KappaBin/resources/bin/
	cp _build/default/core/agents/KaStor.exe build/KappaBin/resources/bin/
	cp _build/default/core/odes/KaDE.exe build/KappaBin/resources/bin/
	cp _build/default/core/agents/KappaSwitchman.exe build/KappaBin/resources/bin/
	cp _build/default/core/agents/KaMoHa.exe build/KappaBin/resources/bin/
	cp _build/default/core/agents/KaSimAgent.exe build/KappaBin/resources/bin/
	cp _build/default/core/agents/KaSaAgent.exe build/KappaBin/resources/bin/

KappaBin:
	# make clean before to be sure things are rebuilt   TODO improve?
	+$(MAKE) clean
	+$(MAKE) build/KappaBin

build/KappaBin.zip: build/KappaBin
	zip -y -r $@ build/KappaBin
	rm -r build/KappaBin

KappaBin.zip: build/KappaBin.zip

build/Kappapp.app: build/Info.plist build/Kappa.icns
	# do not call make clean here or deps will be erased
	+$(MAKE) APP_EXT=local build/site/index.html
	dune build --only-packages kappa-library,kappa-binaries,kappa-agents
	+$(MAKE) build/Kappa.icns build/Info.plist
	FILE=$$(mktemp -t electronXXXX); FOLDER=$$(mktemp -t electron_unzipedXXXX); \
	curl -LsS -o $$FILE https://github.com/electron/electron/releases/download/v$(ELECTRON_VERSION)/electron-v$(ELECTRON_VERSION)-darwin-x64.zip && \
	rm $$FOLDER && mkdir -p $$FOLDER && pushd $$FOLDER && unzip $$FILE && popd && mv $$FOLDER/Electron.app $@ && rm -r $$FOLDER
	rm -r $@/Contents/Resources/*.lproj/
	mkdir $@/Contents/Resources/bin
	cp _build/install/default/bin/* $@/Contents/Resources/bin/
	mv build/site $@/Contents/Resources/app/
	mv build/Kappa.icns $@/Contents/Resources/
	mv build/Info.plist $@/Contents/

Kappapp.app:
	# make clean before to be sure things are rebuilt   TODO improve?
	+$(MAKE) clean
	+$(MAKE) build/Kappapp.app

build/Info.plist: gui/Info.plist.skel $(wildcard .git/refs/heads/*)
	mkdir -p build
	sed -e s/'\(.*\)\".*tag: \([^,\"]*\)[,\"].*/\1\"\2\"'/g $< | \
	sed -e 's/\$$Format:%D\$$'/"$$(git describe --always --dirty || echo unkown)"/ > $@

build/Kappa.iconset: gui/Kappa-Logo.png
	mkdir -p build
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

build/Kappa.icns: build/Kappa.iconset
	mkdir -p build
	iconutil -c icns $<
