"""Integration test for kappa clients"""
import json
import urllib2
import unittest
import subprocess
import random
import string
import time
from kappa_client import KappaRuntime#, KappaError


class TestKappaClient(unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(cls):
        """ set up unit test by launching client"""
        cls.websim = "../WebSim.native"
        cls.key = cls.generate_key()
        cls.port = 6666
        command_format = "{0} --development --shutdown-key {1} --port {2}"
        subprocess.Popen(command_format.format(cls.websim, cls.key, cls.port).split())
        time.sleep(1)
        cls.endpoint = "http://127.0.0.1:{0}".format(cls.port)

    @classmethod
    def tearDownClass(cls):
        """ tear down test by shutting down"""
        runtime = KappaRuntime(cls.endpoint)
        print runtime.shutdown(cls.key)

    @classmethod
    def generate_key(cls):
        """ generate random key for kappa server. """
        return ''.join(random.
                       SystemRandom().
                       choice(string.ascii_uppercase + string.digits)
                       for _ in range(100))

    def __init__(self, *args, **kwargs):
        """ initalize test by launching kappa server """
        self.websim = "../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        super(TestKappaClient, self).__init__(*args, **kwargs)

    def check_integration_test(self, integration_test):
        """ run an integration test by requesting a url with
            a given method and checking the response code.
        """
        method = integration_test['method']
        handler = urllib2.HTTPHandler()
        opener = urllib2.build_opener(handler)
        url = "{0}{1}".format(self.endpoint, integration_test['url'])
        arguments = integration_test['arguments']
        request = urllib2.Request(url,
                                  integration_test['arguments'])
        request.get_method = lambda : method
        connection = opener.open(request)
        self.assertEqual(connection.code, integration_test['code'])


    def test_response_codes(self):
        """Check the correct error codes are returned upon
           request.
        """
        with open("test_kappa_client.json") as json_file:
            json_data = json.loads(json_file.read())
            for integration_test in json_data:
                self.check_integration_test(integration_test)
    def test_info(self):
        """ the the ability of the server to return
            information about the service.
        """
        runtime = KappaRuntime(self.endpoint)
        info = runtime.info()
        self.assertIsNotNone('environment_simulations' in info)
        self.assertEqual(info['environment_simulations'], 0)
        self.assertIsNotNone('environment_projects' in info)
        self.assertEqual(info['environment_projects'], 0)
        self.assertIsNotNone('environment_build' in info)

if __name__ == '__main__':
    unittest.main(verbosity=2)
