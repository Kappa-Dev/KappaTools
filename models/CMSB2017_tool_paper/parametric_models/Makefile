MODELREP=generated_models
ML=$(wildcard *.ml)
BIN=$(basename $(ML))
REPS= $(patsubst %,$(MODELREP)/%,$(BIN))
SCRIPTREP= ../script/

FROM?=1
TO?=10

all: $(BIN) $(MODELREP) $(REPS)
	echo $(REPS) ; \
	for file in $(BIN) ; do \
					./$$file $(MODELREP)/$$file/ $(FROM) $(TO) ; \
  done

%: %.ml
	ocamlopt.opt	$< -o $@

$(MODELREP):
	mkdir $(MODELREP)

$(MODELREP)/%: $(MODELREP)
	mkdir $@
	cp $(SCRIPTREP)* $@/

clean:
	rm -rf  *.cm* *.o

clean_all: clean
	rm -rf $(MODELREP) $(BIN)
