""" Web api client for the kappa programming language
"""

import urllib2
import json

class KappaError(Exception):
    """ Error returned from the Kappa server
    """
    def __init__(self, errors):
        Exception.__init__(self)
        self.errors = errors

class KappaRuntime(object):
    """ Create a client by supplying a web endpoint
    """
    def __init__(self, endpoint):
        self.url = "{0}/v2".format(endpoint)

    def info(self):
        """ Get summary information about the web service.
            This should provide a quick status check for
            the endpoint.
        """
        try:
            version_url = "{0}".format(self.url)
            response = urllib2.urlopen(version_url)
            text = response.read()
            return json.loads(text)
        except urllib2.URLError as exception:
            raise KappaError(exception.reason)

    def shutdown(self, key):
        """ Shut down kappa instance.  Given a key to a
            kappa service shutdown a running kappa instance.
        """
        method = "POST"
        handler = urllib2.HTTPHandler()
        opener = urllib2.build_opener(handler)
        parse_url = "{0}/shutdown".format(self.url)
        request = urllib2.Request(parse_url, data=key)
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib2.HTTPError, exception:
            connection = exception
        except urllib2.URLError as exception:
            raise KappaError(exception.reason)
        if connection.code == 200:
            text = connection.read()
            return text
        elif connection.code == 400:
            text = connection.read()
            raise KappaError(text)
        elif connection.code == 401:
            text = connection.read()
            raise KappaError(text)
        else:
            raise exception

if __name__ == "__main__":
    with open("../models/abc-pert.ka") as f:
        try:
            #data = f.read()
            KAPPA_INSTANCE = KappaRuntime("http://localhost:8080")
            print KAPPA_INSTANCE.info()
            #token = runtime.start({'code': data, 'nb_plot': 150})
            #print runtime.status(token)
        except KappaError as exception:
            print exception.errors
