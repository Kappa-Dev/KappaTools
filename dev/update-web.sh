#!/bin/sh

set -e

PLAYGROUND=$(mktemp -d -t kappaXXXX)
git clone --quiet -b master https://${KAPPAGITHUBTOKEN}:@github.com/Kappa-Dev/Kappa-Dev.github.io.git ${PLAYGROUND}
git config user.email "kappa-dev@listes.sc.univ-paris-diderot.fr"
git config user.name "KappaBot"
[ -d ${PLAYGROUND}/docs/KaSim-manual-master/generated_img ] || \
    mkdir -p ${PLAYGROUND}/docs/KaSim-manual-master/generated_img
cp man/*.htm man/*.css man/*.png ${PLAYGROUND}/docs/KaSim-manual-master/
cp -r man/img ${PLAYGROUND}/docs/KaSim-manual-master/
cp man/generated_img/*.png ${PLAYGROUND}/docs/KaSim-manual-master/generated_img/
COMMITNAME=$(git show --pretty=oneline -s --no-color)
cd ${PLAYGROUND} && git add ${PLAYGROUND}/docs/KaSim-manual-master/ && \
{ git commit -m "Sync website with Kappa-Dev/KaSim@${COMMITNAME}" && \
git push -q origin master || echo "No Changes" ; }
