"${KAPPABIN}"KaSa ../kappa/protein2x2.ka --compute-local-traces --no-output-directory  && \
    dot -Tpng Agent_trace_P.a1_.a2_.b1_.b2_.g^.dot -o ../generated_img/trace_raw.png && \
    rm -f *.dot && \
    "${KAPPABIN}"KaSa ../kappa/protein2x2.ka --compute-local-traces --no-output-directory --use-macrotransitions-in-local-traces  && \
    dot -Tpng Agent_trace_P.a1_.a2_.b1_.b2_.g^.dot -o ../generated_img/trace_macro.png && \
    rm -f *.dot
rm profiling.html
