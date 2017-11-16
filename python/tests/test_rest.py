from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

import random
import string
import inspect
from os import path
from subprocess import Popen
from time import sleep
from datetime import datetime

import kappy
from kappy.kappa_common import KappaError
from util import _KappaClientTest, run_nose, find_path


def find_websim():
    # Look bellow the KAPPY_DIR first, then KASIM_DIR
    for top_dir in ['../..']:
        sim_path = find_path(top_dir, 'WebSim')
        if sim_path is not None:
            return sim_path

    raise KappaError('WebSim could not be found.')


class RestClientTest(_KappaClientTest):
    """ Integration test for kappa client"""
    def __init__(self, *args, **kwargs):
        """Initalize test by launching kappa server"""
        self.websim = find_websim()
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
            '--level', 'info',  # TODO: This is temporary until --log works.
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

    def getRuntime(self, project_id=None):
        if project_id is None:
            project_id = kappy.KappaRest.make_unique_id('test_project')
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
        runtime = self.getRuntime()
        info = runtime.get_info()
        self.assertIsNotNone('environment_projects' in info)
        self.assertIsNotNone('environment_build' in info)


def test_docs():
    kappa_rest_doc = inspect.getdoc(kappy.KappaRest.file_create)
    kappa_abst_doc = inspect.getdoc(kappy.kappa_common.KappaApi.file_create)
    assert kappa_rest_doc is not None and kappa_rest_doc == kappa_abst_doc, \
        "Doc fixing failed: parent method doc not inheritted by KappaRest."


if __name__ == '__main__':
    run_nose(__file__)
