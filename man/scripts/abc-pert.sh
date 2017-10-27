"${KAPPABIN}"KaSim ../../models/abc-pert.ka -seed 822295616 -l 90 -p 0.3 -o abc-pert.csv -mode batch -syntax 4 && \
gnuplot abc-pert.gplot && \
rm -f  *.csv inputs.ka
