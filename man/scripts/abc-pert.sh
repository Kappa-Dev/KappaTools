"${KAPPABIN}"KaSim -i ../kappa/abc-pert.ka -seed 822295616 -e 100000 -t 300 -pp 0.3 -o abc-pert.out --batch && \
gnuplot abc-pert.gplot && \
rm -f  *.out
