for f in  *.ka; do echo ---------------------$f--------------->> log.txt;
~/local/KaSim/bin/KaSa $f 2>&1 | cat >> log.txt; done




