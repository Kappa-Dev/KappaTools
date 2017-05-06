SUMMARY?=summary.txt
FROM?=1
TO?=10

MODELREP=generated_models
ML=$(wildcard *.ml)
BIN=$(basename $(ML))
REPS= $(patsubst %,$(MODELREP)/%,$(BIN))
SCRIPTREP= ../script/
GENMODEL=$(ML:%.ml=%.generate_model)
ANALYZEMODEL=$(ML:%.ml=%.analyze_model)

all: $(ANALYZEMODEL)

%.generate_model: % $(BIN) $(MODELREP) $(REPS)
	./$* $(MODELREP)/$*/ $(FROM) $(TO)
	touch $@

%.analyze_model: %.generate_model
	cd $(MODELREP)/$* ; make clean ; make SUMMARY=$(SUMMARY)
	cat $(MODELREP)/$*/$(SUMMARY) >> $(SUMMARY)

%: %.ml
	ocamlopt.opt	$< -o $@

$(MODELREP):
	mkdir $(MODELREP)

$(MODELREP)/%: $(MODELREP)
	mkdir $@
	cp -f $(SCRIPTREP)*.sh $@/
	cp -f $(SCRIPTREP)Makefile $@/

clean:
	rm -rf  *.cm* *.o

clean_all: clean
	rm -rf $(MODELREP) $(BIN) $(SUMMARY)
