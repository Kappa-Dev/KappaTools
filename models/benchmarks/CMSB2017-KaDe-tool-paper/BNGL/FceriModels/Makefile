LOG=log.txt
SUMMARY?=summary.txt
TIMEOUTBIN=/usr/local/bin/gtimeout $(TIMEOUT)
TIMEOUT?=10m
FROM?=1
TO?=10

KAPPABIN?=$(HOME)/KaSim/bin/
BNGBIN?=$(HOME)/BioNetGen/

KA=$(wildcard *.ka)
WITNESS=$(KA:%.ka=%.done)

KADE=$(KAPPABIN)KaDE
KASA=$(KAPPABIN)KaSa
BNGL=$(BNGBIN)BNG2.pl

all: clean $(WITNESS) $(SUMMARY)

$(SUMMARY):
		grep "CPU\|Process" log.txt>$(SUMMARY)

clean:
	rm -f *.kasa *.txt *.net *.done *.title *.kade *.kade_fwd *.kade_bwd *.bngl_wo_sym *.bngl_with_sym

%.done: %.ka %.title %.kade %.kasa %.kade_fwd %.kade_bwd %.bngl_wo_sym %.bngl_with_sym
	touch $@

%.title: %.ka
	echo -------------------------------------- >> $(LOG)
	echo Process with: $* >> $(LOG)
	echo -------------------------------------- >> $(LOG)
	touch $@

%.kade: *.ka
	echo Process without symmetries------------ >> $(LOG)
	$(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --dotnet-output network_$*_wo_sym.net >> $(LOG) |exit 0
	touch $@

%.kade_fwd: %.ka
	echo Process with forward symmetries------- >> $(LOG)
	$(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Forward  --dotnet-output network_$*_with_fsym.net >> $(LOG) |exit 0
	touch $@

%.kade_bwd: %.ka
	echo Process with backward symmetries------ >> $(LOG)
	$(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Backward --dotnet-output network_$*_with_bsym.net >> $(LOG) |exit 0
	touch $@

%.kasa: %.ka
	echo Process symmetries computation-------- >> $(LOG)
	$(TIMEOUTBIN) $(KASA) $<  --print-efficiency --no-do-all --compute-symmetries >> $(LOG) |exit 0
	touch $@

%.bngl_with_sym: %.bngl
	echo Process BNGL with symmetries---------- >> $(LOG)
	$(TIMEOUTBIN) $(BNGL) $*_sym.bngl >> log.txt |exit 0
	touch $@

%.bngl_wo_sym: %_asym.bngl
	echo Process BNGL without symmetries------- >> $(LOG)
	$(TIMEOUTBIN) $(BNGL) $*.bngl >> log.txt |exit 0
	touch $@
