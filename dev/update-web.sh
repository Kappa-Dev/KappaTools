#!/bin/sh

PLAYGROUND=$(mktemp -d -t kappaXXXX)
git clone --quiet -b master https://${KAPPAGITHUBTOKEN}:@github.com/Kappa-Dev/Kappa-Dev.github.io.git ${PLAYGROUND}
[ -d ${PLAYGROUND}/docs/KaSim-manual-master/ ] || mkdir -p ${PLAYGROUND}/docs/KaSim-manual-master/
cp man/*.htm man/*.css ${PLAYGROUND}/docs/KaSim-manual-master/
cp -r man/img ${PLAYGROUND}/docs/KaSim-manual-master/
COMMITNAME=$(git show --pretty=oneline -s --no-color)
cd ${PLAYGROUND} && git add ${PLAYGROUND}/docs/KaSim-manual-master/ && \
git commit -m "Sync website with Kappa-Dev/KaSim@${COMMITNAME}" && \
git push -q origin master
