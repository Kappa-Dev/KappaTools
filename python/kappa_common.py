""" Web api client for the kappa programming language
"""

import urllib.error
import urllib.request
import urllib.parse
import sys
import getopt
import time
import subprocess
import json
import uuid
import threading
import abc

class KappaError(Exception):
    """ Error returned from the Kappa server
    """
    def __init__(self, errors):
        Exception.__init__(self)
        self.errors = errors

class StdBase(object):
    def __init__(self, path , delimiter = '\x1e', args = None):
        self.delimiter = delimiter
        sim_args  = [ path,
                      "--delimiter" ,
                      "\\x{:02x}".format(ord(self.delimiter)) ,
                      "--log" ,
                      "-" , ]
        if args:
            sim_args = sim_args + args
        self.lock = threading.Lock()
        self.message_id = 0
        self.popen = subprocess.Popen(sim_args,
                                      stdin=subprocess.PIPE,
                                      stdout=subprocess.PIPE ,
                                      stderr=subprocess.STDOUT)

    def get_message_id(self):
        self.message_id = self.message_id + 1
        return(self.message_id)

    def dispatch(self,method,args):
        try:
            self.lock.acquire()
            message_id = self.get_message_id()
            message = { 'id' : message_id,
                        'data' : [method,args]}
            message = "{0}{1}".format(json.dumps(message),self.delimiter)
            self.popen.stdin.write(message.encode('utf-8'))
            self.popen.stdin.flush()
            buffer = bytearray()
            c = self.popen.stdout.read(1)
            while (c != self.delimiter.encode('utf-8') and c):
                buffer.extend(c)
                c = self.popen.stdout.read(1)
            response = json.loads(buffer.decode('utf-8'))
            if response["id"] != message_id:
                raise KappaError("expect id {0} got {1}".format(response["id"],message_id))
            else:
                return(self.projection(response))

        finally:
            self.lock.release()

    @abc.abstractmethod
    def projection(self,response): pass

    def shutdown(self,):
        self.popen.stdin.close()
        self.popen.stdout.close()
        self.popen.kill()

class RestBase(object):
    def __init__(self, endpoint):
        self.url = endpoint
    def dispatch(self,method,url,data):
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        if data:
            code = json.dumps(data)
            request = urllib.request.Request(url,
                                             data=code.encode("utf-8"))
        else:
            request = urllib.request.Request(url)
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.request.HTTPError as exception:
            message = exception.read()
            error = json.loads(message.decode("utf-8"))
            raise KappaError(error)
        except urllib.request.URLError as exception:
            raise KappaError(exception.reason)
        text = connection.read()
        details = json.loads(text.decode("utf-8"))
        if 400 <= connection.code and connection.code < 500 :
            raise KappaError(details)
        else:
            return(details)

    def shutdown(self, key):
        """ Shut down kappa instance.  Given a key to a
            kappa service shutdown a running kappa instance.
        """
        method = "POST"
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        parse_url = "{0}/shutdown".format(self.url)
        request = urllib.request.Request(parse_url, data=key.encode('utf-8'))
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.error.HTTPError as exception:
            connection = exception
        except urllib.error.URLError as exception:
            raise KappaError(exception.reason)
        if connection.code == 200:
            text = connection.read()
            return text
        elif connection.code == 400:
            text = connection.read()
            print(text)
            raise KappaError(text)
        elif connection.code == 401:
            text = connection.read()
            raise KappaError(text)
        else:
            raise exception
