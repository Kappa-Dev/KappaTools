"${KAPPABIN}"KaSim ../kappa/abc.ka -seed 751761073 -u e -l 100000 -p 1000 -o abc.csv -mode batch -syntax 3 && \
gnuplot abc.gplot && \
circo -Tpng flux.dot -o ../generated_img/flux.png && \
"${KAPPABIN}"KaSa -syntax 3 ../kappa/abc.ka --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/abc_contact.png && \
dot -Tpng influence.dot -o ../generated_img/abc_influence.png && \
rm -f  *.csv flux.dot contact.dot influence.dot profiling.html inputs.ka
