${KAPPABIN}KaSim -i ../kappa/abc.ka -e 100000 -p 1000 -o abc.out
gnuplot abc.gplot 
dot -Tpng flux.dot -o ../generated_img/flux.png
rm -f  *.out flux.dot 

