""" integration test for v1 of kappa webservices
    this is going to be deprecated.
"""
import unittest
import subprocess
import random
import string
import time
import kappa_common
import kappa_client_v1

class KappaClientTest(object):

    def test_version(self):
        """ check version of api
        """
        runtime = self.getRuntime()
        version = runtime.version()
        self.assertIsNotNone('version' in version)
        self.assertIsNotNone('build' in version)

    def test_parse(self):
        """ test if the api can parse
        """
        check = lambda parse: self.assertIsNotNone('contact_map' in parse)
        runtime = self.getRuntime()
        parse = runtime.parse("")
        check(parse)
        parse = runtime.parse("%var: 'one' 1")
        check(parse)
        parse = runtime.parse("A(x!1),B(x!1) -> A(x),B(x) @ 0.01")
        check(parse)
        try:
            parse = runtime.parse("A(x)")
            self.fail()
        except kappa_common.KappaError as exception:
            self.assertEqual(exception.errors[0]["message"],
                             'Syntax error')

    def test_start(self):
        """ test starting a simulation
        """
        runtime = self.getRuntime()
        start_1 = runtime.start(kappa_client_v1.Parameter("%var: 'one' 1",1))
        start_2 = runtime.start(kappa_client_v1.Parameter("%var: 'one' 1",1))
        self.assertTrue(start_1 < start_2)

    def test_status_will_fail(self):
        """ test if status can capture a failure
        """
        runtime = self.getRuntime()
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start(kappa_client_v1.Parameter(data,150))
            runtime.status(token)

    def test_status_bad(self):
        """ test bad status
        """
        runtime = self.getRuntime()
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start(kappa_client_v1.Parameter(data,
                                                            150,
                                                            max_time = 10.01))
            status = runtime.status(token)
            self.assertIn('plot', status)
            self.assertIn('log_messages', status)
            self.assertIn('is_running', status)
            self.assertIn('tracked_events', status)
            self.assertIn('event_percentage', status)
            self.assertIn('time_percentage', status)
            self.assertIn('event', status)
            self.assertIn('is_running', status)

    def test_stop(self):
        """ test stop
        """
        runtime = self.getRuntime()
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start(kappa_client_v1.Parameter(data,
                                                            150,
                                                            max_time = 10.01))
            status = runtime.status(token)
            self.assertIn('plot', status)
            self.assertIn('log_messages', status)
            self.assertIn('is_running', status)
            self.assertIn('tracked_events', status)
            self.assertIn('event_percentage', status)
            self.assertIn('time_percentage', status)
            self.assertIn('event', status)
            self.assertIn('is_running', status)
            self.assertIn(token, runtime.list())
            runtime.stop(token)
            self.assertNotIn(token, runtime.list())
            try :
                runtime.status(token)
                self.fail()
            except kappa_common.KappaError as exception:
                None

# class RestClientTest(KappaClientTest,unittest.TestCase):
#     """ Integration test for kappa client"""
#     @classmethod
#     def setUpClass(self):
#         """ set up unit test by launching client"""
#         self.websim = "../WebSim.native"
#         self.key = self.generate_key()
#         self.port = 6666
#         command_format = "{0} --shutdown-key {1} --port {2} --level debug"
#         subprocess.Popen(command_format.format(self.websim, self.key, self.port).split())
#         time.sleep(1)
#         self.endpoint = "http://127.0.0.1:{0}".format(self.port)
#     def getRuntime(self):
#         return(kappa_client_v1.KappaRest(self.endpoint))
#     @classmethod
#     def tearDownClass(self):
#         """ tear down test by shutting down"""
#         runtime = self.getRuntime(self)
#         runtime.shutdown(self.key)

#     @classmethod
#     def generate_key(cls):
#         """ generate random key for kappa server. """
#         return ''.join(random.
#                        SystemRandom().
#                        choice(string.ascii_uppercase + string.digits)
#                        for _ in range(100))

#     def __init__(self, *args, **kwargs):
#         """ initalize test by launching kappa server """
#         self.websim = "../WebSim.native --level debug"
#         self.key = self.generate_key()
#         self.port = 6666
#         super(KappaClientTest, self).__init__(*args, **kwargs)

class StdClientTest(KappaClientTest,unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(self):
        """ set up unit test by launching client"""
        self.stdsim = "../StdSim.native"

    def getRuntime(self):
        return(kappa_client_v1.KappaStd(self.stdsim))
    @classmethod
    def tearDownClass(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime(self)
        runtime.shutdown()

    def __init__(self, *args, **kwargs):
        super(KappaClientTest, self).__init__(*args, **kwargs)

if __name__ == '__main__':
    unittest.main(verbosity=2)
