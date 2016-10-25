""" Web api client for the kappa programming language
"""
import urllib.error
import urllib.request
import urllib
import sys, getopt
import time
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
        except urllib.error.URLError as exception:
            raise KappaError(exception.reason)

    def parse(self, code):
        """ parse code throw an exception if the parse
            fails.
        """
        query_args = {'code':code}
        encoded_args = urllib.parse.urlencode(query_args)
        parse_url = "{0}/parse?{1}".format(self.url, encoded_args)
        try:
            response = urlopen(parse_url)
            text = response.read()
            return json.loads(text.decode("utf-8"))
        except urllib.error.HTTPError as exception:
            if exception.code == 400:
                error_details = json.loads(exception.read())
                raise KappaError(error_details)
            else:
                raise exception
        except urllib.error.URLError as exception:
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
            error_details = json.loads(text.decode("utf-8"))
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


def main():
    #command line
    argv = sys.argv[1:]
    cmd = "kappa_client_v1.py"

    #default arguments
    inputfile = None # if missing input file just get version
    url = "http://localhost:8080"
    max_time = 10.0
    max_events = 10
    nb_plot = 150

    try:
        opts, args = getopt.getopt(argv,
                                   "hk:u:t:e:p",
                                   ["kappafile=",
                                    "url=",
                                    "max_time=",
                                    "max_events=",
                                    "number_plot=",])
    except :
        print (cmd+
                  +' -k <kappafile> '
                  +' -u <url> '
                  +' -t <max_time> '
                  +' -e <max_events> '
                  +' -p <number_plots> ')

        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print (cmd+' -i <inputfile> -o <outputfile>')
            sys.exit()
        elif opt in ("-k", "--kappafile"):
            inputfile = arg
        elif opt in ("-u", "--url"):
            url = arg
        elif opt in ("-t", "--max_time"):
            max_time = float(arg)
        elif opt in ("-e", "--max-events"):
            max_events = int(arg)
        elif opt in ("-p", "--number_plot"):
            nb_plot = int(arg)

    print ('Input file is : {0} '.format(inputfile))
    print ('Endpoint url : {0} '.format(url))
    print ('Max time : {0}'.format(max_time))
    print ('Max events : {0} '.format(max_events))
    print ('Number plot : {0} '.format(nb_plot))

    try :
        runtime = KappaRuntime(url)
        if inputfile :
            with open(inputfile) as f:
                code = f.read()
                token = runtime.start({'code': code,
                                       'nb_plot': nb_plot ,
                                       'max_time' : max_time ,
                                       'max_events' : max_events })
                status = runtime.status(token)
                while status['is_running']:
                    time.sleep(1)
                    sys.stdout.write( '.' )
                    status = runtime.status(token)
                print()
                print(status)
        else:
            print(runtime.version())
    except KappaError as exception:
        print(exception.errors)
    None

if __name__ == "__main__":
    main()
