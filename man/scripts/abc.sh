"${KAPPABIN}"KaSim ../kappa/abc.ka -seed 751761073 -e 100000 -p 1000 -o abc.out --batch && \
gnuplot abc.gplot && \
circo -Tpng flux.dot -o ../generated_img/flux.png && \
"${KAPPABIN}"KaSa ../kappa/abc.ka --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/abc_contact.png && \
dot -Tpng influence.dot -o ../generated_img/abc_influence.png
rm -f  *.out flux.dot contact.dot influence.dot
