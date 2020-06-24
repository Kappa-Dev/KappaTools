SUMMARY?=summary.txt
TIMEOUTBIN?=/usr/local/bin/gtimeout
TIMEOUT?=10m
FROM?=1
TO?=10

LOG=log.txt
TIMEOUTCMD=$(TIMEOUTBIN) $(TIMEOUT)

KAPPABIN?=$(HOME)/KaSim/bin/
BNGBIN?=$(HOME)/BioNetGen/

KA=$(wildcard *.ka)
WITNESS=$(KA:%.ka=%.done)

KADE=$(KAPPABIN)KaDE
KASA=$(KAPPABIN)KaSa
BNGL=$(BNGBIN)BNG2.pl

all: clean $(WITNESS) $(SUMMARY)

$(SUMMARY):
		grep "CPU T\|CPU t\|Process" log.txt>$(SUMMARY)

clean:
	rm -f *.txt *.net *.done *.title *.kade *.kade_fwd *.kade_bwd *.bngl_wo_sym *.bngl_with_sym

%.done: %.ka %.title %.kade %.kasa %.kade_fwd %.kade_bwd %.bngl_wo_sym %.bngl_with_sym
	touch $@

%.title: %.ka
	echo ------------------------------------------- >> $(LOG)
	echo Process example $* >> $(LOG)
	echo ------------------------------------------- >> $(LOG)
	touch $@

%.kade: *.ka
	echo Process KaDE -> ground reaction network---- >> $(LOG)
	$(TIMEOUTCMD) $(KADE) $<  --print-efficiency --ode-backend DOTNET --dotnet-output network_$*_wo_sym.net --propagate-constants >> $(LOG) |exit 0
	touch $@

%.kade_fwd: %.ka
	echo Process KaDE with forward symmetries------- >> $(LOG)
	$(TIMEOUTCMD) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Forward  --propagate-constants --dotnet-output network_$*_with_fsym.net >> $(LOG) |exit 0
	touch $@

%.kade_bwd: %.ka
	echo Process kaDe with backward symmetries------ >> $(LOG)
	$(TIMEOUTCMD) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Backward --propagate-constants --dotnet-output network_$*_with_bsym.net >> $(LOG) |exit 0
	touch $@

%.kasa: %.ka
	echo Process symmetries computation------------- >> $(LOG)
	$(TIMEOUTCMD) $(KASA) $<  --print-efficiency --no-do-all --compute-symmetries >> $(LOG) |exit 0
	touch $@

%.bngl_with_sym: %_sym.bngl
	echo Process BNGL with explicit equivalent sites- >> $(LOG)
	$(TIMEOUTCMD) $(BNGL) $*_sym.bngl >> log.txt |exit 0
	touch $@

%.bngl_wo_sym: %.bngl
	echo Process BNGL without equivalent sites------------ >> $(LOG)
	$(TIMEOUTCMD) $(BNGL) $*.bngl >> log.txt |exit 0
	touch $@
