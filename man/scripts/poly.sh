"${KAPPABIN}"KaSim ../kappa/poly.ka -seed 588300472 -u e -pp 0 -l 10000 -mode batch && \
dot -Tpng snap*.dot -o ../generated_img/poly.png && \
rm -f *.dot
