LOG=log.txt
SUMMARY?=summary.txt
TIMEOUTBIN=/usr/local/bin/gtimeout $(TIMEOUT)
TIMEOUT?=10m
FROM?=1
TO?=10

KAPPABIN?=$(HOME)/KaSim/bin/
BNGBIN?=$(HOME)/BioNetGen/

KAPPABIN?=
KA=$(wildcard *.ka)
WITNESS=$(KA:%.ka=%.done)

all: clean $(WITNESS) $(SUMMARY)

$(SUMMARY):
	grep "CPU\|Process" log.txt>$(SUMMARY)


clean:
  rm -f *.txt *.net

%.done: %.ka %.title %.kade %.kasa %.kade_fwd %.kade_bwd %.bngl_wo_sym %.bngl_with_sym
	touch $@

%.title: %.ka
	echo -------------------------------------- >> $(LOG)
	echo -Process with: $* >> $(LOG)
	echo -------------------------------------- >> $(LOG)
  touch $@

%.kade: *.ka
	echo ----------without_symmetries----------- >> $(LOG)
  $(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --dotnet-output network_$*_wo_sym.net --propagate-constants >> $(LOG) | exit 0
  touch $@

%.kade_fwd: %.ka
  echo ----------with_symmetries_forward----------- >> $(LOG)
  $(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Forward  --propagate-constants --dotnet-output network_$*_with_fsym.net >> $(LOG) | exit 0
  touch $@

%.kade_bwd: %.ka
	echo ----------with_symmetries_backward----------- >> $(LOG)
  $(TIMEOUTBIN) $(KADE) $<  --print-efficiency --ode-backend DOTNET --with-symmetries Backward --propagate-constants --dotnet-output network_$*_with_bsym.net >> $(LOG) | exit 0
  touch $@

%.kasa: %.ka
	  echo ----------compute_symmetries----------- >> $(LOG)
    $(TIMEOUTBIN) $(KASA) $<  --print-efficiency --no-do-all --compute-symmetries >> $(LOG) | exit 0
    touch $@

%.bngl_with_sym: %_sym.bngl
	  echo ----------BNGL_symmetries---------- >> $(LOG)
    $(TIMEOUTBIN) $(BNGL) $*_sym.bngl >> log.txt | exit 0

%.bngl_wo_sym: %.bngl
  echo ----------BNGL_without_symmetries----------- >> $(LOG)
  $(TIMEOUTBIN) $(BNGL) $*.bngl >> log.txt | exit 0
