#!/bin/sh

set -e

PLAYGROUND=$(mktemp -d -t kappaXXXX)
git clone --quiet -b master https://${KAPPAGITHUBTOKEN}:@github.com/Kappa-Dev/Kappa-Dev.github.io.git ${PLAYGROUND}
case $1 in
    native )
        [ -d ${PLAYGROUND}/docs/KaSim-manual-master/generated_img ] || \
            mkdir -p ${PLAYGROUND}/docs/KaSim-manual-master/generated_img
        cp man/*.htm man/*.css ${PLAYGROUND}/docs/KaSim-manual-master/
        cp -r man/img ${PLAYGROUND}/docs/KaSim-manual-master/
        cp man/generated_img/*.png ${PLAYGROUND}/docs/KaSim-manual-master/generated_img/
        cp _build/dev/KaSim.docdir/* ${PLAYGROUND}/docs/KaSim-API-master/
        ;;
    js )
        [ -d ${PLAYGROUND}/try ] || \
            mkdir ${PLAYGROUND}/try
        cp js/index.html js/JaSim.js js/Kappa.js js/harness.js js/JaSim.css ${PLAYGROUND}/try/
        ;;
esac
COMMITNAME=$(git show --pretty=oneline -s --no-color)
cd ${PLAYGROUND}
git config user.email "kappa-dev@listes.sc.univ-paris-diderot.fr"
git config user.name "KappaBot"
git add ${PLAYGROUND}/docs/KaSim-manual-master/ ${PLAYGROUND}/try/ \
    ${PLAYGROUND}/docs/KaSim-API-master/ && \
    { git commit -m "Sync website for $1 with Kappa-Dev/KaSim@${COMMITNAME}" && \
    git push -q origin master || echo "No Changes" ; }
