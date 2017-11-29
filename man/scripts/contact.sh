"${KAPPABIN}"KaSa ../kappa/contact.ka --reset-all --compute-contact-map --contact-map-accuracy-level Low --output-contact-map contact_map_low_res.dot
"${KAPPABIN}"KaSa ../kappa/contact.ka --reset-all --compute-contact-map --contact-map-accuracy-level High --output-contact-map contact_map_high_res.dot
"${KAPPABIN}"KaSa ../kappa/contact.ka --reset-all --compute-contact-map --contact-map-accuracy-level High --no-sites-across-bonds-domain --output-contact-map contact_map_high_res_wo_relations.dot
dot -Tpng -o ../generated_img/contact_map_low_res.png output/contact_map_low_res.dot
dot -Tpng -o ../generated_img/contact_map_high_res.png output/contact_map_high_res.dot
dot -Tpng -o ../generated_img/contact_map_high_res_wo_relations.png output/contact_map_high_res_wo_relations.dot

rm -r output/
