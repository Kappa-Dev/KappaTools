cat kasa_options.head > LOG_mute.txt && \
cat kasa_options.head > LOG_low.txt && \
cat kasa_options.head > LOG_medium.txt && \
cat kasa_options.head > LOG_high.txt && \
cat kasa_options.head > LOG_full.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka >> LOG_low.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-view-analysis Mute >> LOG_mute.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-view-analysis Medium >> LOG_medium.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-view-analysis High >> LOG_high.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/reachability.ka --verbosity-level-for-view-analysis Full >> LOG_full.txt && \
cat kasa_options.foot >> LOG_mute.txt && \
cat kasa_options.foot >> LOG_low.txt && \
cat kasa_options.foot >> LOG_medium.txt && \
cat kasa_options.foot >> LOG_high.txt && \
cat kasa_options.foot >> LOG_full.txt && \
cp LOG*.txt ../generated_img/ && \
rm LOG*.txt
