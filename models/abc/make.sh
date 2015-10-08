rm log.txt;
for f in  *.ka; do echo ---------------------$f--------------->> log.txt;
~/KaSim/bin/KaSa $f 2>&1 | cat >> log.txt; done
