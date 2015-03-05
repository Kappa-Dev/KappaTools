${KAPPABIN}KaSim ../kappa/poly.ka -seed 588300472 -e 10000 && \
dot -Tpng snap*.dot -o ../generated_img/poly.png && \
rm -f *.dot
