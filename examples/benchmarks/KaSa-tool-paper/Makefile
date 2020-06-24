OUTPUT?=benchmarks.tex
MODEL?=
TITLE?=
KASA?=KaSa

TIMEOUTBIN=/usr/local/bin/gtimeout $(TIMEOUT)
TIMEOUT?=500s

DEALWITHERRORS=||exit 0
OPTIONS?=
COMMON_OPTIONS = --reset-all --backdoor-file $(OUTPUT) $(MODEL) $(OPTIONS)

all:

hline:
	echo '\\hline' >> $(OUTPUT)

title:
	echo 'model & \\multicolumn{11}{c|}{Number of} & \\multicolumn{9}{c|}{Analysis time (ms)} \\cr & \\rotatebox{270}{rules} & \\rotatebox{270}{constraints} & \\rotatebox{270}{dead rules} & \\rotatebox{270}{non weakly reversible transitions} & \\rotatebox{270}{indirect influences} & \\rotatebox{270}{direct influences} & \\rotatebox{270}{realisable influences} & \\rotatebox{270}{strongly connected components (Syntactic)} & \\rotatebox{270}{strongly connected components (Realisable)} & \\rotatebox{270}{edges per scc in average (Syntactic)}&  \\rotatebox{270}{edges per scc in average (High)}&\\rotatebox{270}{reachability analysis} & \\rotatebox{270}{influence map (Indirect)} & \\rotatebox{270}{influence map (Direct)} & \\rotatebox{270}{influence map (Realisable)} & \\rotatebox{270}{polymer detection (Syntactic)} &	\\rotatebox{270}{polymer detection (Realisable)} &\\rotatebox{270}{non weakly reversible transitions} & \\rotatebox{270}{local traces} & \\rotatebox{270}{all-in-one (at maxiaml accuracy)} \\cr' > $(OUTPUT)

line:
	echo '$(TITLE)\c' >> $(OUTPUT)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-rules $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-constraints $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-nbr-dead-rules $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-separating-transitions --backdoor-dump-nbr-non-weakly-reversible-transitions $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map  --influence-map-accuracy-level Indirect --backdoor-dump-nbr-influences $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map --influence-map-accuracy-level Direct  --backdoor-dump-nbr-influences $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map --influence-map-accuracy-level Realisable  --backdoor-dump-nbr-influences $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles  --polymer-detection-accuracy-level Low --backdoor-dump-nbr-scc $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles  --polymer-detection-accuracy-level High --backdoor-dump-nbr-scc $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --polymer-detection-accuracy-level Low  --backdoor-dump-average-scc $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --polymer-detection-accuracy-level High --backdoor-dump-average-scc $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-reachability-analysis --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map  --influence-map-accuracy-level Indirect --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map  --influence-map-accuracy-level Direct --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-influence-map --influence-map-accuracy-level Realisable  --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --polymer-detection-accuracy-level Low  --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --polymer-detection-accuracy-level High  --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-local-traces --backdoor-dump-timing $(DEALWITHERRORS)
	echo ' & \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --backdoor-dump-timing $(DEALWITHERRORS)
	echo '& \c' >> $(OUTPUT)
	$(TIMEOUTBIN) $(KASA) $(COMMON_OPTIONS) --compute-potential-cycles --compute-local-traces --backdoor-dump-timing --influence-map-accuracy-level Realisable  --compute-influence-map $(DEALWITHERRORS)
	echo '\\cr' >> $(OUTPUT)
