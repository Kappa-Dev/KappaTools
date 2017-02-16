"${KAPPABIN}"KaSim ../kappa/abc-cflow.ka -seed 551423834 -l 30 -p 0 -mode batch &&\
dot -Tpng abc-flow_Weakly_0.dot -o ../generated_img/cflow.png &&\
rm -f *.dat *.dot profiling.html compression_status.txt inputs.ka
