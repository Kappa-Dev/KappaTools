cat kasa_options.head > LOG_influence_reachability.txt && \
"${KAPPABIN}"KaSa --reset-all --compute-reachability-analysis ../kappa/influence.ka | fold -w 61 -s >> LOG_influence_reachability.txt && \
cat kasa_options.foot >> LOG_influence_reachability.txt && \
"${KAPPABIN}"KaSa --reset-all ../kappa/influence.ka --influence-map-accuracy-level Low --compute-influence-map --output-influence-map inf_map_low_res.dot --influence-map-format DOT --verbosity-level-for-reachability-analysis Mute && \
"${KAPPABIN}"KaSa --reset-all ../kappa/influence.ka --compute-influence-map --influence-map-accuracy-level Medium --output-influence-map inf_map_medium_res.dot --influence-map-format DOT --verbosity-level-for-reachability-analysis Mute && \
"${KAPPABIN}"KaSa --reset-all  ../kappa/influence.ka --compute-influence-map --influence-map-accuracy-level High --output-influence-map inf_map_high_res.dot --influence-map-format DOT --verbosity-level-for-reachability-analysis Mute && \

dot -Tpng -o ../generated_img/inf_map_low_res.png output/inf_map_low_res.dot
dot -Tpng -o ../generated_img/inf_map_medium_res.png output/inf_map_medium_res.dot
dot -Tpng -o ../generated_img/inf_map_high_res.png output/inf_map_high_res.dot

rm -r output/

mv LOG*.txt ../generated_img/ #&& \
#rm LOG*.txt
