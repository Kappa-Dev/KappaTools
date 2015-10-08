
for f in *.dot; do echo ---------------------$f--------------->> $f.ps; ~/Downloads/Software/graphviz/cmd/dot/dot -Tps $f -o 2>&1 | cat >> $f.ps; done
