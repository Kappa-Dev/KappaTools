${KAPPABIN}KaSim -i ../kappa/poly.ka -e 10000
dot -Tpng snap*.dot -o ../generated_img/poly.png 
rm -f *.dot 
