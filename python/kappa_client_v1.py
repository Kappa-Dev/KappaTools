""" Web api client for the kappa programming language
"""
import urllib.error
import urllib.request
import urllib.parse
import sys
import getopt
import time
import kappa_common
# bad practice but done to support python2


class Parameter(object):
    def __init__(self,
                 code,
                 plot_period,
                 max_time=None,
                 max_events=None,
                 seed=None):
        self.code = code
        self.plot_period = plot_period
        self.max_time = max_time
        self.max_events = max_events
        self.seed = seed

    def tojson(self):
        return({"code": self.code,
                "plot_period": self.plot_period,
                "max_time": self.max_time,
                "max_events": self.max_events,
                "seed": self.seed})


class Continuation(object):
    def __init__(self,
                 token,
                 parameter):
        self.continuation_token = token
        self.continuation_parameter = parameter

    def tojson(self):
        return({"continuation_token": self.continuation_token,
                "continuation_parameter": self.continuation_parameter, })


class KappaStd(kappa_common.StdBase):
    def __init__(self, path, delimiter='\x1e'):
        kappa_common.StdBase.__init__(self, path, delimiter, )

    def projection(self, response):
        result_data = response["data"][1]
        if result_data[0] == "Right":
            return result_data[1]
        elif result_data[0] == "Left":
            print(result_data[1])
            raise kappa_common.KappaError(result_data[1])
        else:
            raise kappa_common.KappaError(response)

    def version(self):
        return self.dispatch("Version", None)

    def parse(self, code):
        return self.dispatch("Parse", code)

    def start(self, parameter):
        return self.dispatch("Start", parameter.tojson())

    def list(self):
        return self.dispatch("List", None)

    def status(self, token):
        return self.dispatch("Status", token)

    def stop(self, token):
        return self.dispatch("Stop", token)

    def pause(self, token):
        return self.dispatch("Pause", token)

    def resume(self, continuation):
        return self.dispatch("Resume", continuation)

    def perturbation(self, token, perturbation):
        return self.dispatch("Perturbation",
                             {'perturbation_token': token,
                              'perturbation_code': perturbation})


class KappaRest(kappa_common.RestBase):
    """ Create a client by supplying a web endpoint
    """
    def __init__(self, endpoint):
        kappa_common.RestBase.__init__(self, endpoint)
        self.url = "{0}/v1".format(endpoint)

    def version(self):
        return self.dispatch("GET", "{0}/version".format(self.url), None)

    def parse(self, code):
        query_args = {'code': code}
        encoded_args = urllib.parse.urlencode(query_args)
        parse_url = "{0}/parse?{1}".format(self.url, encoded_args)
        return self.dispatch("GET", parse_url, None)

    def start(self, parameter):
        return self.dispatch("POST", "{0}/process".format(self.url), parameter.tojson())

    def stop(self, token):
        return self.dispatch("DELETE", "{0}/process/{1}".format(self.url, token), None)

    def status(self, token):
        return self.dispatch("GET", "{0}/process/{1}".format(self.url, token), None)

    def list(self):
        return self.dispatch("GET", "{0}/process".format(self.url), None)


def main():
    # command line
    argv = sys.argv[1:]
    cmd = "kappa_client_v1.py"

    # default arguments
    inputfile = None  # if missing input file just get version
    url = "http://localhost:8080"
    max_time = 10.0
    max_events = 10
    plot_period = 0.1
    seed = None

    try:
        opts, args = getopt.getopt(argv,
                                   "hk:u:t:e:pp:s",
                                   ["kappafile=",
                                    "url=",
                                    "max_time=",
                                    "max_events=",
                                    "plot_period=",
                                    "seed=", ])
    except:
        print(cmd
              + ' -k <kappafile> '
              + ' -u <url or path to stdsim> '
              + ' -t <max_time> '
              + ' -e <max_events> '
              + ' -pp <plot_period> '
              + ' -s <random_seed> ')

        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(cmd+' -i <inputfile> -o <outputfile>')
            sys.exit()
        elif opt in ("-k", "--kappafile"):
            inputfile = arg
        elif opt in ("-u", "--url"):
            url = arg
        elif opt in ("-t", "--max_time"):
            max_time = float(arg)
        elif opt in ("-e", "--max-events"):
            max_events = int(arg)
        elif opt in ("-pp", "--plot_period"):
            plot_period = float(arg)
        elif opt in ("-s", "--seed"):
            seed = int(arg)

    print('Input file is : {0} '.format(inputfile))
    print('Endpoint url : {0} '.format(url))
    print('Max time : {0}'.format(max_time))
    print('Max events : {0} '.format(max_events))
    print('Plot period : {0} '.format(plot_period))
    print('Random seed : {0} '.format(seed))

    try:
        if url.startswith('http'):
            runtime = KappaRest(url)
        else:
            runtime = KappaStd(url)
        if inputfile:
            with open(inputfile) as f:
                code = f.read()
                token = runtime.start(Parameter(code,
                                                plot_period,
                                                max_time=max_time,
                                                max_events=max_events,
                                                seed=seed))
                status = runtime.status(token)
                while status['is_running']:
                    time.sleep(1)
                    sys.stdout.write('.')
                    status = runtime.status(token)
                print()
                print(status)
        else:
            print(runtime.version())
    except kappa_common.KappaError as exception:
        print(exception.errors)
    return None

if __name__ == "__main__":
    main()
