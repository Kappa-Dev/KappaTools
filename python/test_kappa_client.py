import unittest
import subprocess
import uuid
import random
import string
import time
from kappa_client import KappaRuntime, RuntimeError

class TestKappaClient(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.websim = "../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        subprocess.Popen("{0} --shutdown-key {1} --port {2}".format(self.websim,self.key,self.port).split())
        time.sleep(1)
        self.endpoint = "http://127.0.0.1:{0}".format(self.port)

    @classmethod
    def tearDownClass(self):
        runtime = KappaRuntime(self.endpoint)
        print runtime.shutdown(self.key)

    # http://stackoverflow.com/questions/2257441/random-string-generation-with-upper-case-letters-and-digits-in-python/23728630#23728630
    @classmethod
    def generate_key(self):
        return ''.join(random.SystemRandom().choice(string.ascii_uppercase + string.digits) for _ in range(100))

    def __init__(self, *args, **kwargs):
        self.websim = "../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        super(TestKappaClient, self).__init__(*args, **kwargs)

    def test_version(self):
        runtime = KappaRuntime(self.endpoint)
        version = runtime.version()
        self.assertIsNotNone('version' in version)
        self.assertIsNotNone('build' in version)

    def test_parse(self):
        check = lambda parse : self.assertIsNotNone('observables' in parse)
        runtime = KappaRuntime(self.endpoint)
        parse = runtime.parse("")
        check(parse)
        parse = runtime.parse("%var: 'one' 1")
        check(parse)
        try :
            parse = runtime.parse("A(x!1),B(x!1) -> A(x),B(x) @ 0.01")
            assert(False)
        except RuntimeError as e:
            assert(e.errors == ['Error at line 1, characters 0-1: : "A" is not a declared agent.'])
        try :
            parse = runtime.parse("A(x)")
            assert(False)
        except RuntimeError as e:
            assert(e.errors == ['Error at line 1, characters 4-4: : Syntax error'])

    def test_start(self):
        runtime = KappaRuntime(self.endpoint)
        start_1 = runtime.start({ 'code': "%var: 'one' 1", 'nb_plot': 150 })
        start_2 = runtime.start({ 'code': "%var: 'one' 1", 'nb_plot': 150 })
        assert(start_1 < start_2)

    def test_status_will_fail(self):
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as f:
            data = f.read()
            try:
                token = runtime.start({ 'code': data , 'nb_plot': 150 })
                status = runtime.status(token)
                assert(False)
            except RuntimeError as e:
                None

    def test_status_bad(self):
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as f:
            data = f.read()
            token = runtime.start({ 'code': data
                                  , 'nb_plot': 150
                                  , 'max_time' : 10.01 })
            status = runtime.status(token)
            assert('plot' in status)
            assert('log_messages' in status)
            assert('is_running' in status)
            assert('tracked_events' in status)
            assert('event_percentage' in status)
            assert('time_percentage' in status)
            assert('event' in status)
            assert(status['is_running'])

    def test_stop(self):
        runtime = KappaRuntime(self.endpoint)
        with open("../models/abc-pert.ka") as f:
            data = f.read()
            token = runtime.start({ 'code': data
                                  , 'nb_plot': 150
                                  , 'max_time' : 10.01 })
            status = runtime.status(token)
            assert('plot' in status)
            assert('log_messages' in status)
            assert('is_running' in status)
            assert('tracked_events' in status)
            assert('event_percentage' in status)
            assert('time_percentage' in status)
            assert('event' in status)
            assert(status['is_running'])

            runtime.stop(token)
            status = runtime.status(token)
            assert('plot' in status)
            assert('log_messages' in status)
            assert('is_running' in status)
            assert('tracked_events' in status)
            assert('event_percentage' in status)
            assert('time_percentage' in status)
            assert('event' in status)
            assert(not status['is_running'])

if __name__ == '__main__':
    unittest.main(verbosity=2)
