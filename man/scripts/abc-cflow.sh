"${KAPPABIN}"KaSim ../kappa/abc-cflow.ka -seed 551423834 -l 30 -pp 0 -mode batch &&\
 dot -Tpng abc-flow_Weakly_0.dot -o ../generated_img/cflow.png &&\
rm -f *.dat *.dot *.txt data.out
