${KAPPABIN}KaSim -i ../kappa/abc-cflow.ka -t 30 
dot -Tpng abc-flow_Weakly_0.dot -o ../generated_img/cflow.png 
rm -f *.dat *.dot *.txt 

