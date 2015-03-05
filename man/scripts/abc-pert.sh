${KAPPABIN}KaSim -i ../kappa/abc-pert.ka -seed 822295616 -e 100000 -t 300 -p 1000 -o abc-pert.out && \
gnuplot abc-pert.gplot && \
rm -f  *.out
