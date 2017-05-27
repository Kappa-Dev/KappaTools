WWWDIR=$(HOME)/www/shtml/CMSB2017-tool-paper/

all: parametric_models_models_only.tgz parametric_models_all_files.tgz kinase_phosphatase.tgz n_phos_sites.tgz n_phos_sites_with_counter.tgz other_examples_models_only.tgz other_examples_all_files.tgz

parametric_models_models_only.tgz:
	rm -rf tmp
	mkdir tmp
	mkdir tmp/generated_models
	mkdir tmp/generated_models/kinase_phosphatase
	mkdir tmp/generated_models/n_phos_sites
	mkdir tmp/generated_models/n_phos_sites_with_counter
	cp parametric_models/generated_models/kinase_phosphatase/Makefile tmp/generated_models/kinase_phosphatase/
	cp parametric_models/generated_models/kinase_phosphatase/*.ka tmp/generated_models/kinase_phosphatase/
	cp parametric_models/generated_models/kinase_phosphatase/*.bngl tmp/generated_models/kinase_phosphatase/
	cp parametric_models/generated_models/n_phos_sites/Makefile tmp/generated_models/n_phos_sites/
	cp parametric_models/generated_models/n_phos_sites/*.ka tmp/generated_models/n_phos_sites/
	cp parametric_models/generated_models/n_phos_sites/*.bngl tmp/generated_models/n_phos_sites/
	cp parametric_models/generated_models/n_phos_sites_with_counter/Makefile tmp/generated_models/n_phos_sites_with_counter/
	cp parametric_models/generated_models/n_phos_sites_with_counter/*.ka tmp/generated_models/n_phos_sites_with_counter/
	cp parametric_models/generated_models/n_phos_sites_with_counter/*.bngl tmp/generated_models/n_phos_sites_with_counter/
	cd tmp ; tar czf $@ generated_models/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

kinase_phosphatase.tgz:
	rm -rf tmp
	mkdir tmp
	cp -rf parametric_models/generated_models/kinase_phosphatase tmp/
	cd tmp ; tar czf $@ kinase_phosphatase/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

parametric_models_all_files.tgz:
	rm -rf tmp
	mkdir tmp
	cp -rf parametric_models/generated_models tmp/
	cd tmp ; tar czf $@ generated_models/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

n_phos_sites.tgz:
	rm -rf tmp
	mkdir tmp
	cp -rf parametric_models/generated_models/n_phos_sites tmp/
	cd tmp ; tar czf $@ n_phos_sites/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

n_phos_sites_with_counter.tgz:
	rm -rf tmp
	mkdir tmp
	cp -rf parametric_models/generated_models/n_phos_sites_with_counter tmp/
	cd tmp ; tar czf $@ n_phos_sites_with_counter/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

other_examples_models_only.tgz:
	rm -rf tmp
	mkdir tmp
	mkdir tmp/other_examples
	mkdir tmp/other_examples/FceriModels
	cp other_examples/Makefile tmp/other_examples/
	cp other_examples/FceriModels/Makefile tmp/other_examples/FceriModels/
	cp other_examples/*.ka tmp/other_examples/
	cp other_examples/*.bngl tmp/other_examples/
	cp other_examples/FceriModels/*.ka tmp/other_examples/FceriModels/
	cp other_examples/FceriModels/*.bngl tmp/other_examples/FceriModels/
	cd tmp ; tar czf $@ other_examples/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp

other_examples_all_files.tgz:
	rm -rf tmp
	mkdir tmp
	cp -rf other_examples tmp/
	cd tmp ; tar czf $@ other_examples/*
	cp tmp/$@ .
	cp $@ $(WWWDIR)
	rm -rf tmp
