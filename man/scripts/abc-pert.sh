"${KAPPABIN}"KaSim ../kappa/abc-pert.ka -seed 822295616 -l 90 -pp 0.3 -o abc-pert.out -mode batch && \
gnuplot abc-pert.gplot && \
rm -f  *.out
