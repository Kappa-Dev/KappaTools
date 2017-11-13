#!/bin/sh

set -e

PLAYGROUND=$(mktemp -d -t kappaXXXX)
git clone --depth 10 --quiet -b master https://${KAPPAGITHUBTOKEN}:@github.com/Kappa-Dev/Kappa-Dev.github.io.git ${PLAYGROUND}
case $1 in
    native )
        [ -d ${PLAYGROUND}/docs/KaSim-manual-master/generated_img ] || \
            mkdir -p ${PLAYGROUND}/docs/KaSim-manual-master/generated_img
        cp man/*.htm man/*.css ${PLAYGROUND}/docs/KaSim-manual-master/
        cp -r man/img ${PLAYGROUND}/docs/KaSim-manual-master/
        cp -r man/gkappa_img ${PLAYGROUND}/docs/KaSim-manual-master/
        cp man/generated_img/*.png ${PLAYGROUND}/docs/KaSim-manual-master/generated_img/
        cp _build/dev/KaSim.docdir/* ${PLAYGROUND}/docs/KaSim-API-master/
	scp -o UserKnownHostsFile=dev/deploy_hosts -i dev/travis-deploy -r \
	    ${PLAYGROUND}/docs travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/
        ;;
    js )
        [ -d ${PLAYGROUND}/try ] || mkdir ${PLAYGROUND}/try
        cp site/* ${PLAYGROUND}/try/
	sed '/<\/head>/i \
	<!-- Piwik -->\
	<script type="text/javascript">\
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
	<noscript><p><img src="https://coutosuisse.fagny.fr/analytics/piwik.php?idsite=1" style="border:0;" alt="" /></p></noscript>\
	<!-- End Piwik Code -->\
        ' site/index.html > ${PLAYGROUND}/try/index.html
	scp -o UserKnownHostsFile=dev/deploy_hosts -i dev/travis-deploy -r \
	    ${PLAYGROUND}/try travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/
        ;;
    webserver )
        ;;
    '' )
	;;
    MacOS )
        [ -d ${PLAYGROUND}/binaries ] || mkdir ${PLAYGROUND}/binaries
	codesign -s - Kappapp.app && \
	    zip -r ${PLAYGROUND}/binaries/Kappapp.app.zip Kappapp.app
	scp -o UserKnownHostsFile=dev/deploy_hosts -i dev/travis-deploy ${PLAYGROUND}/binaries/Kappapp.app.zip \
	    travis@api.kappalanguage.org:/var/www/tools.kappalanguage.org/nightly-builds/
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
