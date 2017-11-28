"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc2.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc2_contact.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc3.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc3_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc4.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc4_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html && \
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc3.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc3_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html
"${KAPPABIN}"KaSa --compute-potential-cycles ../kappa/scc4.ka  --no-output-directory  && \
dot -Tpng contact.dot -o ../generated_img/scc4_contact_low.png && \
rm -f  *.csv flux.dot contact.dot profiling.html
