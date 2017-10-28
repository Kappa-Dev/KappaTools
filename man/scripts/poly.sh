"${KAPPABIN}"KaSim ../../models/poly.ka -seed 588300472 -u e -p 0 -l 10000 -mode batch && \
dot -Tpng snap*.dot -o ../generated_img/poly.png && \
rm -f *.dot inputs.ka
