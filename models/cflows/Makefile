.DEFAULT_GOAL := all

%/error: $(shell which $(KAPPABIN)KaSim)
	@+$(MAKE) -C $(shell dirname $@) test

error: $(foreach dir,$(shell find $(CURDIR) -mindepth 1 -maxdepth 1 -type d -print),$(dir)/error)
	@cat $^ > $@

all: error
	@if [ -s $< ] ; then cat $< && exit 2 ; else echo "No difference has been found" ; fi

build:
	@for i in * ; do (if (file $$i | grep directory ) then (cd $$i ; $(MAKE) build ; ) else (echo 0 > /dev/null ; ) ;  fi ; )   ; done 

clean: 
	@for i in * ; do (if (file $$i | grep directory ) then (cd $$i ; $(MAKE) clean ; ) else (echo 0 > /dev/null ; ) ;  fi ; )   ; done 

clean_all: 
	@for i in * ; do (if (file $$i | grep directory ) then (cd $$i ; $(MAKE) clean_all ; ) else (echo 0 > /dev/null ; ) ;  fi ; )   ; done 
	@rm -f error 

.phony help:
	@echo 'make help:      this help'
	@echo 'make build:     recursively launch KaSim and store the result as a reference'
	@echo 'make test:      recursively launch KaSim and compare the result with the reference files'
	@echo 'make pdf:       recursively build all pdf from existing dot files'
	@echo 'make clean:     recursively clean everything but the reference files'
	@echo 'make clean_all: recursively clean everything including the reference files'

pdf: 
	@for i in * ; do (if (file $$i | grep directory ) then (cd $$i ; $(MAKE) pdf ; ) else (echo 0 ; ) ;  fi ; )   ; done 

