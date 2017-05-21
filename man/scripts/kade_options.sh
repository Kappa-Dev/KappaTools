cat kade_options.head > ../generated_img/KaDE_options.txt &&\
"${KAPPABIN}"KaDE --help --expert  >> ../generated_img/KaDE_options.txt &&\
cat kade_options.foot >> ../generated_img/KaDE_options.txt
