OUTPUT_PATH:=output
ERROR:=error

DIRS_OF_TEST:=$(shell find $(CURDIR) -mindepth 1 -maxdepth 1 -type d -print)

DOT=$(wildcard */$(OUTPUT_PATH)/*.dot)
PDF=$(DOT:.dot=.pdf)

.DEFAULT_GOAL := all
.PHONY: all build help pdf

help:
	@echo 'make help:      this help'
	@echo 'make build:     recursively launch KaSim and store the result as a reference'
	@echo 'make test:      recursively launch KaSim and compare the result with the reference files'
	@echo 'make pdf:       recursively build all pdf from existing dot files'
	@echo 'make clean:     recursively clean everything but the reference files'
	@echo 'make clean_all: recursively clean everything including the reference files'

%/$(OUTPUT_PATH)/LOG: %/README $(shell which $(KAPPABIN)KaSim)
	@[ -d $(dir $<)$(OUTPUT_PATH) ] || mkdir $(dir $<)$(OUTPUT_PATH)
	cd $(dir $<) && sh $(notdir $<) > $(OUTPUT_PATH)/LOG 2> $(OUTPUT_PATH)/error.log

%.ref: %
	@sed '/generation/d' $< > $@

%.diff:
	@sed '/generation/d' $(basename $@) | diff -u $(basename $@).ref - > $@ || true

TODIFFFILES=$(filter-out %.diff,\
	$(sort $(patsubst %.ref,%,$(wildcard $(dir $@)$(OUTPUT_PATH)/*))))
%/$(ERROR): %/README $(shell which $(KAPPABIN)KaSim)
	@find $(dir $@)$(OUTPUT_PATH) -mindepth 1 \( -not -name \*.ref \) -delete
	@+$(MAKE) $(dir $@)$(OUTPUT_PATH)/LOG
	@+$(MAKE) $(TODIFFFILES:=.diff)
	@TMP=$$(mktemp -t error_file-XXXXXX) ; cat $(dir $@)$(OUTPUT_PATH)/*.diff > $$TMP ;\
	if [ -s $$TMP ] ; then echo ****$(dir $(dir $@))**** > $@ ;\
	  echo  >> $@ ; cat $(dir $(dir $@))README >> $@ ; echo >> $@ ; cat $$TMP >> $@ ; \
	else touch $@ ; fi ; rm $$TMP

error: $(DIRS_OF_TEST:=/$(ERROR))
	@cat $^ > $@

all: error
	@if [ -s $< ] ; then cat $< && exit 2 ; else echo "No difference has been found" ; fi

GENFILES=$(filter-out %.diff %.ref,$(wildcard */$(OUTPUT_PATH)/*))
build:
	@+$(MAKE) clean_all
	@+$(MAKE) $(DIRS_OF_TEST:=/$(OUTPUT_PATH)/LOG)
	@+$(MAKE) $(GENFILES:=.ref)
	@+$(MAKE) clean

.PHONY: clean_light clean clean_all

clean: clean_light
	find $(CURDIR) \( -name \#\* -or -name \*~ -or -name \*.txt -or -name \*.sxw \) -delete
	for i in $(DIRS_OF_TEST:=/$(OUTPUT_PATH)) ;\
	  do [ -d $${i} ] && find $${i} -mindepth 1 \( -not -name \*.ref \) -delete || true;\
	done
	rm -f $(DIRS_OF_TEST:=/$(ERROR))

clean_all: clean
	rm -rf $(DIRS_OF_TEST:=/$(OUTPUT_PATH)) error

%.pdf: %.dot
	@dot -Tpdf $< -o $@

pdf: $(PDF)
