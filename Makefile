VN:=$(shell cat tag/number)#            #Git version number for commit tag
VERSION:=$(shell cat tag/version)#      #Major revision release number
RELEASE:=$(shell cat tag/release)#      #Release number
DATE:=`date +'%Y-%m-%d %H:%M:%S'`#      #date YYYY-MM-DD 

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
 OCAMLBUILDFLAGS = -classic-display
else
 OCAMLBUILDFLAGS =
endif

USE_TK?=0

ifeq ($(USE_TK),1)
OCAMLINCLUDES = -I KaSa_rep/lib/full -cflags -I,$(LABLTKLIBREP),-I,+labltk -lflags -I,$(LABLTKLIBREP),-I,+labltk -libs labltk,jpflib
else
OCAMLINCLUDES = -I KaSa_rep/lib/light
endif

SCRIPTSSOURCE = $(wildcard $(MANSCRIPTREP)*.sh)
SCRIPTSWITNESS = $(SCRIPTSSOURCE:.sh=.witness)
MODELS = $(wildcard $(MANKAPPAMODELSREP)*.ka)

.PHONY: all clean temp-clean-for-ignorant-that-clean-must-be-done-before-fetch
.PHONY: check build-tests doc clean_doc fetch_version

.PRECIOUS: $(SCRIPTSWITNESS)

$(MANGENREP): $(SCRIPTSSOURCE) $(MODELS)
	rm -rf $@
	mkdir $@

%.native %.byte %.docdir/index.html: $(filter-out _build/,$(wildcard */*.ml*)) $(wildcard $(KASAREP)*/*.ml*) $(wildcard $(KASAREP)*/*/*.ml*)
	$(OCAMLBINPATH)ocamlbuild $(OCAMLBUILDFLAGS) $(OCAMLINCLUDES) $@

JaSim.byte: $(filter-out _build/,$(wildcard */*.ml*))
	ocamlbuild $(OCAMLbuiLDFLAGS) $(OCAMLBUILDFLAGS) -I js \
	-tag-line "<js/**> : thread, package(js_of_ocaml.tyxml), package(js_of_ocaml.syntax), package(tyxml.syntax), package(lwt), syntax(camlp4o)" \
	$@

js/JaSim.js: JaSim.byte
	js_of_ocaml "+weak.js" _build/js/$< -o $@

bin/%: %.native Makefile
	[ -d bin ] || mkdir bin && cp $< $@
	rm -f $(notdir $@) && ln -s $@ $(notdir $@)


%.pdf: %.tex $(SCRIPTSWITNESS)
	cd $(dir $<) && LOG=$$(mktemp -t latexlogXXXX); rm -f *.aux && \
	pdflatex -halt-on-error $(notdir $<) > $${LOG} 2>&1 && \
	bibtex $(basename $(notdir $<)) >> $${LOG} 2>&1 && \
	makeindex $(basename $(notdir $<)).idx >> $${LOG} 2>&1 && \
	pdflatex -halt-on-error $(notdir $<) >> $${LOG} 2>&1 && \
	pdflatex -halt-on-error $(notdir $<) >> $${LOG} 2>&1 && \
	rm $${LOG} || { cat $${LOG}; rm $${LOG}; exit 2; }

%.htm: %.tex %.pdf
	cd $(dir $<) && htlatex $(notdir $<)  "nma.cfg,htm,charset=utf-8,p-width" " -cunihtf -utf8" &&\
	htlatex $(notdir $<)  "nma.cfg,htm,charset=utf-8,p-width" " -cunihtf -utf8"

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa $(MODELS) %.gplot
	cd $(dir $@) && KAPPABIN=$(CURDIR)/bin/ sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

%.witness: %.sh $(MANGENREP) bin/KaSim bin/KaSa $(MODELS)
	cd $(dir $@) && KAPPABIN=$(CURDIR)/bin/ sh $(notdir $<) > $(notdir $@) 2>&1 \
	|| { cat $(notdir $@); rm $(notdir $@); exit 2; }

doc: man/KaSim_manual.pdf
doc_html: dev/KaSim.docdir/index.html man/KaSim_manual.htm

all: bin/KaSim bin/KaSa

clean_doc:
	find man \( -not -name \*.tex -and -name KaSim_manual.\* \) -delete
	find man \( -name \*.htm \) -delete
	find man/scripts \( -name \*.witness \) -delete
	rm -rf $(MANGENREP)

clean: temp-clean-for-ignorant-that-clean-must-be-done-before-fetch clean_doc
	$(OCAMLBINPATH)ocamlbuild -clean
	rm -f sanity_test bin/sanity_test
	rm -f KaSim bin/KaSim KaSa bin/KaSa
	find . -name \*~ -delete
	+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/test_suite clean

check: bin/sanity_test
	@+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/test_suite all

build-tests:
	@+$(MAKE) KAPPABIN=$(CURDIR)/bin/ -C models/test_suite build

temp-clean-for-ignorant-that-clean-must-be-done-before-fetch:
	find . \( -name \*.cm\* -or -name \*.o -or -name \*.annot \) -delete
	rm -f grammar/kappaLexer.ml grammar/kappaParser.ml grammar/kappaParser.mli


full: 
	make clean
	make USE_TK=1 || echo 0 

light: 
	make clean
	make USE_TK=0 || echo 0 

commit: fetch_version 
	echo -n `expr $(VN) + 1` > tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Not a release" send_caml

major_version: fetch_version
	echo -n `expr $(VERSION) + 1` > tag/version
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n 1 > tag/release
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

release: fetch_version
	echo -n `expr $(RELEASE) + 1`> tag/release
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

fetch_version:
	cd tag ; git checkout HEAD * 

send_caml: 
	echo -n xxx$(VN)$(RELEASE)$(VERSION)$(DATE)xxx
	echo -n let git_commit_version,git_commit_release,git_commit_tag,git_commit_date  = $(VERSION),$(RELEASE),$(VN),\"$(DATE)\" > KaSa_rep/automatically_generated/git_commit_info.ml 
	git commit -a 
	git tag -a $(VN)  -m "$(PREF) v$(VERSION).$(RELEASE)...$(VN) $(DATE)"  
	git push --tags
	git push 
