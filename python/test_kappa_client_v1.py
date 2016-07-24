""" integration test for v1 of kappa webservices
    this is going to be deprecated.
"""
import unittest
import subprocess
import random
import string
import time
from kappa_client_v1 import KappaRuntime, KappaError

class TestKappaClient(unittest.TestCase):
    """ test of kappa web services"""
    @classmethod
    def setUpClass(cls):
        """ start up server"""
        cls.websim = "../WebSim.native"
        cls.key = cls.generate_key()
        cls.port = 6666
        command_format = "{0} --shutdown-key {1} --port {2}"
        subprocess.Popen(command_format.format(cls.websim, cls.key, cls.port).split())
        time.sleep(1)
        cls.endpoint = "http://127.0.0.1:{0}".format(cls.port)

    @classmethod
    def tearDownClass(cls):
        """ stop up server"""
        runtime = KappaRuntime(cls.endpoint)
        print runtime.shutdown(cls.key)

    # http://stackoverflow.com/questions/2257441/random-string-generation-with-upper-case-letters-and-digits-in-python/23728630#23728630
    @classmethod
    def generate_key(cls):
        """ generate random server key"""
        return ''.join(random.
                       SystemRandom().
                       choice(string.ascii_uppercase + string.digits)
                       for _ in range(100))

    def __init__(self, *args, **kwargs):
        """ set up test """
        self.websim = "../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        super(TestKappaClient, self).__init__(*args, **kwargs)

    def test_version(self):
        """ check version of api"""
        runtime = KappaRuntime(self.endpoint)
        version = runtime.version()
        self.assertIsNotNone('version' in version)
        self.assertIsNotNone('build' in version)

    def test_parse(self):
        """ test if the api can parse"""
        check = lambda parse: self.assertIsNotNone('observables' in parse)
        runtime = KappaRuntime(self.endpoint)
        parse = runtime.parse("")
        check(parse)
        parse = runtime.parse("%var: 'one' 1")
        check(parse)
        try:
            parse = runtime.parse("A(x!1),B(x!1) -> A(x),B(x) @ 0.01")
            self.fail()
        except KappaError as exception:
            self.assertEqual(exception.errors[0]["message"],
                             '"A" is not a declared agent.')
        try:
            parse = runtime.parse("A(x)")
            self.fail()
        except KappaError as exception:
            self.assertEqual(exception.errors[0]["message"],
                             'Syntax error')

    def test_start(self):
        """ test starting a simulation"""
        runtime = KappaRuntime(self.endpoint)
        start_1 = runtime.start({'code': "%var: 'one' 1", 'nb_plot': 150})
        start_2 = runtime.start({'code': "%var: 'one' 1", 'nb_plot': 150})
        self.assertTrue(start_1 < start_2)

    def test_status_will_fail(self):
        """ test if status can capture a failure """
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start({'code': data, 'nb_plot': 150})
            runtime.status(token)

    def test_status_bad(self):
        """ test bad status"""
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start({'code': data,
                                   'nb_plot': 150,
                                   'max_time' : 10.01})
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
        """ test stop """
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            token = runtime.start({'code': data,
                                   'nb_plot': 150,
                                   'max_time' : 10.01})
            status = runtime.status(token)
            self.assertIn('plot', status)
            self.assertIn('log_messages', status)
            self.assertIn('is_running', status)
            self.assertIn('tracked_events', status)
            self.assertIn('event_percentage', status)
            self.assertIn('time_percentage', status)
            self.assertIn('event', status)
            self.assertIn('is_running', status)

            runtime.stop(token)
            status = runtime.status(token)
            self.assertIn('plot', status)
            self.assertIn('log_messages', status)
            self.assertIn('is_running', status)
            self.assertIn('tracked_events', status)
            self.assertIn('event_percentage', status)
            self.assertIn('time_percentage', status)
            self.assertIn('event', status)
            self.assertIn('is_running', status)

if __name__ == '__main__':
    unittest.main(verbosity=2)
