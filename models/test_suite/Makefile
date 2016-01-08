OUTPUT_PATH:=output
ERROR:=error

DIRS_OF_TEST:=$(shell find "$(CURDIR)" -mindepth 2 -maxdepth 2 -type d -print)

DOT=$(wildcard */*/$(OUTPUT_PATH)/*.dot)
PDF=$(DOT:.dot=.pdf)

.DEFAULT_GOAL := all
.PHONY: all build help pdf

help:
	@echo 'make help:      this help'
	@echo 'make build:     recursively launch KaSim and store the result as a reference'
	@echo 'make test:      recursively launch KaSim and compare the result with the reference files'
	@echo 'make pdf:       recursively build all pdf from existing dot files'
	@echo 'make svg:       recursively build all svg from existing dot files'
	@echo 'make clean:     recursively clean everything but the reference files'
	@echo 'make clean_all: recursively clean everything including the reference files'

%/$(OUTPUT_PATH)/LOG: %/README #$(shell which "$(KAPPABIN)KaSim") $(shell which "$(KAPPABIN)KaSa")
	@[ -d $(dir $<)$(OUTPUT_PATH) ] || mkdir $(dir $<)$(OUTPUT_PATH)
	@find $(dir $<)$(OUTPUT_PATH) -mindepth 1 \( -not -name \*.ref \) -delete
	cd $(dir $<) && sh $(notdir $<) > $(OUTPUT_PATH)/LOG 2> $(OUTPUT_PATH)/error.log

%/profiling.txt.diff:
	@touch $(basename $@).txt.diff

%/compression_status.txt.diff:
	@touch $(basename $@).txt.diff

%.diff:
	@touch $(basename $@).ref $(basename $@)
	@sed '/generation time/d;/Kappa Static Analyzer/d;/Analysis launched at/d' $(basename $@) | diff -q $(basename $@).ref - || true
	@sed '/generation time/d;/Kappa Static Analyzer/d;/Analysis launched at/d' $(basename $@) | diff -u $(basename $@).ref - > $@ || true

%/$(ERROR): %/$(OUTPUT_PATH)/LOG
	@rm -f $@
	@+$(MAKE) $$(find $(dir $@)$(OUTPUT_PATH) \( -not -type d -and -not -name \*.diff.\* \) -print \
		| sed -e 's/^\(.*\).ref/\1/g' | sort | uniq | sed -e 's/$$/.diff/')
	@TMP=$$(mktemp -t error_file-XXXXXX) ; cat $(dir $@)$(OUTPUT_PATH)/*.diff > $$TMP ;\
	if [ -s $$TMP ] ; then echo ****$(dir $(dir $@))**** > $@ ;\
	  echo  >> $@ ; cat $(dir $(dir $@))README >> $@ ; echo >> $@ ; cat $$TMP >> $@ ; \
	else touch $@ ; fi ; rm $$TMP

error: $(DIRS_OF_TEST:=/$(ERROR))
	@cat $^ > $@

all: error
	@if [ -s $< ] ; then \
	if [ $$(cat $< | wc -l) -gt 1500 ] ; then echo "Error file too big" ; else cat $< ; fi && exit 2 ; \
	else echo "No difference has been found" ; fi

build:
	@+$(MAKE) clean_all
	@+$(MAKE) $(DIRS_OF_TEST:=/$(OUTPUT_PATH)/LOG)
	find "$(CURDIR)" \( -path \*/\*/$(OUTPUT_PATH)/\* -and -not -name \*.diff -and -not -name \*profiling.txt \
		-and -not -name \*compression_status.txt -and -not -name \*.ref \) -exec mv \{\} \{\}.ref \;
	find "$(CURDIR)" -name \*.ref -exec sed -i"" -e '/generation time/d;/Kappa Static Analyzer/d;/Analysis launched at/d' \{\} \;
	@+$(MAKE) clean

.PHONY: clean clean_all

clean:
	find "$(CURDIR)" \( -name \#\* -or -name \*~ -or -name \*.sxw \) -delete
	for i in $(DIRS_OF_TEST:=/$(OUTPUT_PATH)) ;\
	  do [ -d $${i} ] && find $${i} -mindepth 1 \( -not -name \*.ref \) -delete || true;\
	done
	rm -f $(DIRS_OF_TEST:=/$(ERROR)) error

clean_all: clean
	rm -rf $(DIRS_OF_TEST:=/$(OUTPUT_PATH))

%.pdf: %.dot
	dot -Tpdf $< -o $@

pdf: $(PDF)

%.svg: %.dot
	dot -Tsvg $< -o $@

svg: $(DOT:.dot=.svg)
