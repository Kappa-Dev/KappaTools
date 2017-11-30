"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_abc.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc_abc_contact.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_relations.ka  --no-output-directory  --polymer-detection-accuracy-level Low && \
dot -Tpng contact.dot -o ../generated_img/scc_relations_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_dimer.ka  --no-output-directory  --polymer-detection-accuracy-level Low && \
dot -Tpng contact.dot -o ../generated_img/scc_dimer_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_relations.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc_relations_contact_high.png && \
rm -f  *.csv flux.dot contact.dot profiling.html
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_dimer.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc_dimer_contact_high.png && \
rm -f  *.csv flux.dot contact.dot profiling.html
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc_dimer.ka --no-double-bonds-domain --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc_dimer_contact_high_no_double_bonds.png && \
rm -f  *.csv flux.dot contact.dot profiling.html
