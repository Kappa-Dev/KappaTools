#!/bin/sh

# NOTE: below are references to travis, but travis is no longer responsible for te deploys but github actions

set -e

empty_or_create ()
{
    [ -d "$1" ] && find "$1" -mindepth 1 -delete || mkdir -p "$1"
}

PLAYGROUND=$(mktemp -d -t kappaXXXX)
git clone --depth 10 --quiet -b master git@github.com:Kappa-Dev/Kappa-Dev.github.io.git ${PLAYGROUND}
case $1 in
    native )
        empty_or_create ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}
        cp man/*.htm man/*.css ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/
	cp man/KaSim_manual.pdf ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}.pdf
        mkdir ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/img
        cp man/img/*.png ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/img/
        mkdir ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/gkappa_img
        cp man/gkappa_img/*.png ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/gkappa_img/
        mkdir ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/generated_img
        cp man/generated_img/*.png ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/generated_img/

        empty_or_create  ${PLAYGROUND}/docs/KaSim-API-${TRAVIS_BRANCH}
        cp -r _build/default/_doc/_html/kappa-library/* ${PLAYGROUND}/docs/KaSim-API-${TRAVIS_BRANCH}/

        scp -r \
            ${PLAYGROUND}/docs travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/
        ;;
    js )
        empty_or_create ${PLAYGROUND}/try
        cp build/site/* ${PLAYGROUND}/try/
	sed '/<\/head>/i \
	<!-- Piwik -->\
	<!--<script type="text/javascript">\
	var _paq = _paq || [];\
	_paq.push(['\''trackPageView'\'']);\
	_paq.push(['\''enableLinkTracking'\'']);\
	(function() {\
		var u="https://coutosuisse.fagny.fr/analytics/";\
		_paq.push(['\''setTrackerUrl'\'', u+'\''piwik.php'\'']);\
		_paq.push(['\''setSiteId'\'', 1]);\
		var d=document, g=d.createElement('\''script'\''), s=d.getElementsByTagName('\''script'\'')[0];\
		g.type='\''text/javascript'\''; g.async=true; g.defer=true; g.src=u+'\''piwik.js'\''; s.parentNode.insertBefore(g,s);\
	    })();\
	</script>\
	<noscript><p><img src="https://coutosuisse.fagny.fr/analytics/piwik.php?idsite=1" style="border:0;" alt="" /></p></noscript>-->\
	<!-- End Piwik Code -->\
        ' build/site/index.html > ${PLAYGROUND}/try/index.html
        scp -o UserKnownHostsFile=dev/deploy_hosts -i dev/travis-deploy -r \
            ${PLAYGROUND}/try travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/
        empty_or_create ${PLAYGROUND}/viz
        cp gui/viz/* ${PLAYGROUND}/viz/
        scp -o UserKnownHostsFile=dev/deploy_hosts -i dev/travis-deploy -r \
            ${PLAYGROUND}/viz travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/
        [ -d ${PLAYGROUND}/binaries ] || mkdir ${PLAYGROUND}/binaries
        cp build/Kappapp.tar.gz ${PLAYGROUND}/binaries/
        scp ${PLAYGROUND}/binaries/Kappapp.tar.gz \
            travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/nightly-builds/
        ;;
    python )
        ;;
    '' )
        ;;
    windows )
        [ -d ${PLAYGROUND}/binaries ] || mkdir ${PLAYGROUND}/binaries
        cp build/KappaBin.zip ${PLAYGROUND}/binaries/
        scp ${PLAYGROUND}/binaries/KappaBin.zip \
            travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/nightly-builds/
        ;;
    MacOS )
        [ -d ${PLAYGROUND}/binaries ] || mkdir ${PLAYGROUND}/binaries
        cp build/Kappapp.app.zip ${PLAYGROUND}/binaries/
        scp ${PLAYGROUND}/binaries/Kappapp.app.zip \
            travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/nightly-builds/
        ;;
esac
COMMITNAME=$(git show --pretty=oneline -s --no-color)
cd ${PLAYGROUND}
git config user.email "kappa-dev@listes.sc.univ-paris-diderot.fr"
git config user.name "KappaBot"
git add ${PLAYGROUND}/docs/KaSim-manual-${TRAVIS_BRANCH}/ ${PLAYGROUND}/try/ \
    ${PLAYGROUND}/docs/KaSim-API-${TRAVIS_BRANCH}/ && \
    { git commit -m "Sync website for $1 with Kappa-Dev/KaSim@${TRAVIS_COMMIT}" && \
    git push -q origin master || echo "No Changes" ; }
cd ${HOME} && rm -rf ${PLAYGROUND}
