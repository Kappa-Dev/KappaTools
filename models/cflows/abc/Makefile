MODEL= $(wildcard *.ka)
DIR=$(shell pwd)
EXPECTED_OUTPUT_PATH=expected-output
TEST_PATH=test-output
SCRIPT=script

EXPECTED_DOT= $(wildcard expected-output/*.dot) 
TESTED_DOT= $(wildcard test-output/*.dot)

DIFF=$(TESTED_DOT:.dot=.diff)
CHECK=$(TESTED_DOT:.dot=.error)

all: test

$(TEST_PATH): 
	@rm -rf $(TEST_PATH) 
	@mkdir $(TEST_PATH)

$(SCRIPT): README
	@sed "s/expected-output/test-output/g" $< > $@

test: $(TEST_PATH) 
	@make launch_script 
	@make do_diff

launch_script: $(SCRIPT)
	@sh $<

do_diff: $(DIFF) $(CHECK)

%.diff:  $(TEST_PATH)/%.dot 
	@diff $^ $(EXPECTED_OUTPUT_PATH)/$*.dot > $@ || exit 0 

$(TEST_PATH)/%.diff: %.diff
	@mv $< $(TEST_PATH) 

%.error: %.diff 
	@file $< | grep -q empty || (echo $(DIR)/$(MODEL): $*.dot has changed! > $@ ; cat $< >> $@ 

clean:
	@rm -rf $(TEST_PATH) $(SCRIPT) $(DIFF_PATH)