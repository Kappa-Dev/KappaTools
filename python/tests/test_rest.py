from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

import random
import string
from os import path
from subprocess import Popen
from time import sleep
from datetime import datetime

from util import _KappaClientTest, _get_id, BIN_DIR, run_nose
import kappy

class RestClientTest(_KappaClientTest):
    """ Integration test for kappa client"""
    def __init__(self, *args, **kwargs):
        """Initalize test by launching kappa server"""
        self.websim = path.join(BIN_DIR, "WebSim")
        self.key = self.generate_key()
        self.port = 6666
        self.endpoint = "http://127.0.0.1:{0}".format(self.port)
        return super(RestClientTest, self).__init__(*args, **kwargs)

    def _mark_log_file(self, msg):
        """A convenience method to mark sections of a continuous log file."""
        with open('WebSim_test.log', 'a') as logfile:
            logfile.write("-"*80 + '\n')
            logfile.write('%s at %s.\n' % (msg, datetime.now()))
            logfile.write("-"*80 + '\n')
        return

    def setUp(self):
        """Set up unit test by launching client

        Note: This is run automatically by nosetests before EACH test method,
        so there is no object permanence between tests.
        """
        print("Starting server")
        self._mark_log_file('Starting server')
        Popen([
            self.websim,
            '--shutdown-key', self.key,
            '--port', str(self.port),
            '--level', 'fatal',  # TODO: This is temporary until --log works.
            '--log', 'WebSim_test.log'
            ])
        sleep(1)
        print("Started")
        return

    def tearDown(self):
        """Tear down test by shutting down.

        Note: This is run automatically by nosetests afer EACH test method,
        so there is no object permanence between tests.
        """
        runtime = self.getRuntime("test_proj")
        print("Closing server...")
        resp = runtime.shutdown(self.key)
        self._mark_log_file('Stopping server')
        sleep(1)
        print("Closed", resp)
        return

    def getRuntime(self, project_id):
        return kappy.KappaRest(self.endpoint, project_id)

    @classmethod
    def generate_key(cls):
        """Generate random key for kappa server. """
        return ''.join(random.
                       SystemRandom().
                       choice(string.ascii_uppercase + string.digits)
                       for _ in range(100))

    # Rest Tests ==============================================================

    def test_get_rest_service_info(self):
        project_id = _get_id("dummy")
        runtime = self.getRuntime(project_id)
        info = runtime.get_info()
        self.assertIsNotNone('environment_projects' in info)
        self.assertIsNotNone('environment_build' in info)


if __name__ == '__main__':
    run_nose(__file__)