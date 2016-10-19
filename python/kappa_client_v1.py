""" Web api client for the kappa programming language
"""

import urllib.request
import urllib

# bad pratice but done to support python2

from urllib.request import urlopen

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
        self.url = "{0}/v1".format(endpoint)

    def version(self):
        """ get version of environment this should provide
            a quick status check for the endpoint as the
            URL is already versioned.
        """
        try:
            version_url = "{0}/version".format(self.url)
            response = urlopen(version_url)
            text = response.read()
            return json.loads(text.decode('utf-8'))
        except urllib2.URLError as exception:
            raise KappaError(exception.reason)

    def parse(self, code):
        """ parse code throw an exception if the parse
            fails.
        """
        query_args = {'code':code}
        encoded_args = urllib.urlencode(query_args)
        parse_url = "{0}/parse?{1}".format(self.url, encoded_args)
        try:
            response = urllib2.urlopen(parse_url)
            text = response.read()
            return json.loads(text.decode("utf-8"))
        except urllib2.HTTPError as exception:
            if exception.code == 400:
                error_details = json.loads(exception.read())
                raise KappaError(error_details)
            else:
                raise exception
        except urllib2.URLError as exception:
            KappaError(exception.reason)

    def start(self, parameter):
        """ parse code throw an exception if the parse
            fails.
        """
        if not 'max_time' in parameter:
            parameter['max_time'] = None
        if not 'max_events' in parameter:
            parameter['max_events'] = None
        code = json.dumps(parameter)
        method = "POST"
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        parse_url = "{0}/process".format(self.url)
        request = urllib.request.Request(parse_url, data=code.encode("utf-8"))
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.request.HTTPError as exception:
            connection = exception
        except urllib.request.URLError as exception:
            raise KappaError(exception.reason)

        if connection.code == 200:
            text = connection.read()
            return int(json.loads(text.decode("utf-8")))
        elif connection.code == 400:
            text = connection.read()
            error_details = json.loads(text.decode("utf-8"))
            raise KappaError(error_details)
        else:
            raise exception

    def stop(self, token):
        """ stop running process
        """
        method = "DELETE"
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        parse_url = "{0}/process/{1}".format(self.url, token)
        request = urllib.request.Request(parse_url)
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.request.HTTPError as exception:
            connection = exception
        except urllib.request.URLError as exception:
            raise KappaError(exception.reason)

        if connection.code == 200:
            text = connection.read()
            return None
        elif connection.code == 400:
            text = connection.read()
            error_details = json.loads(textdecode("utf-8"))
            raise KappaError(error_details)
        else:
            raise exception

    def status(self, token):
        """ status of running process
        """
        try:
            version_url = "{0}/process/{1}".format(self.url, token)
            response = urllib.request.urlopen(version_url)
            text = response.read()
            return json.loads(text.decode("utf-8"))
        except urllib.request.HTTPError as exception:
            if exception.code == 400:
                error_details = json.loads(exception.read())
                raise KappaError(error_details)
            else:
                raise exception
        except urllib.request.URLError as exception:
            KappaError(exception.reason)


    def shutdown(self, key):
        """ shutdown server
        """
        method = "POST"
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        parse_url = "{0}/shutdown".format(self.url)
        request = urllib.request.Request(parse_url, data=key.encode("utf-8"))
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.request.HTTPError as exception:
            connection = exception
        except urllib.request.URLError as exception:
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
            code = f.read()
            runtime = KappaRuntime("http://localhost:8080")
            token = runtime.start({'code': KAPPA_CODE, 'nb_plot': 150})
            print(runtime.status(token))
        except KappaError as exception:
            print(exception.errors)
