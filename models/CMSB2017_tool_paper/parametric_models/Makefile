SUMMARY?=summary.txt
TIMEOUT?=10m
FROM?=1
TO?=3

KAPPABIN?=$(HOME)/KaSim/bin/
BNGBIN?=$(HOME)/BioNetGen/

MODELREP=generated_models
ML=$(wildcard *.ml)
BIN=$(basename $(ML))
REPS= $(patsubst %,$(MODELREP)/%,$(BIN))
SCRIPTREP= ../script/
GENMODEL=$(ML:%.ml=%.generate_model)
ANALYZEMODEL=$(ML:%.ml=%.analyze_model)

all: $(REPS) $(GENMODEL) $(ANALYZEMODEL)

%.generate_model: %.bin $(MODELREP) $(MODELREP)/%
	./$* $(MODELREP)/$* $(FROM) $(TO)
	touch $@

%.analyze_model:  %.generate_model
	cd $(MODELREP)/$* ; make clean ; make KAPPABIN=$(KAPPABIN) BNGBIN=$(BNGBIN) SUMMARY=$(SUMMARY) TIMEOUT=$(TIMEOUT)
	echo time_out $(TIMEOUT) >> $(SUMMARY)
	cat $(MODELREP)/$*/$(SUMMARY) >> $(SUMMARY)
	touch $@

%.bin: %.ml
	ocamlopt.opt	$< -o $*
	touch $@

$(MODELREP):
	mkdir $@

$(MODELREP)/%: $(MODELREP)
	mkdir $@
	cp -f $(SCRIPTREP)Makefile $@/

clean:
	rm -rf  *.cm* *.o

clean_all: clean
	rm -rf $(MODELREP) $(BIN) $(SUMMARY) *.generate_model *.analyze_model *.bin
