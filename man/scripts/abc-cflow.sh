"${KAPPABIN}"KaSim ../kappa/abc-cflow.ka -seed 551423834 -t 30 -p 10 && \
dot -Tpng abc-flow_Weakly_0.dot -o ../generated_img/cflow.png && \
rm -f *.dat *.dot *.txt
