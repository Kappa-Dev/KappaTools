"""Integration test for kappa clients"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

import sys
import json
import subprocess
import random
import string
import time
import uuid

from os import path
from datetime import datetime

import unittest
import nose
import kappy

KASIM_DIR = path.normpath(
    path.join(path.dirname(path.abspath(__file__)), *([path.pardir]*2))
    )
BIN_DIR = path.join(KASIM_DIR, "bin")
MODELS_DIR = path.join(KASIM_DIR, "models")

def _get_id(name):
    return "%s-%s" % (name, uuid.uuid1())

class _KappaClientTest(unittest.TestCase):

    def test_file_crud(self):
        print("Getting the runtime object...")
        runtime = self.getRuntime(project_id=_get_id('test_proj'))

        print("Creating the file...")
        file_id = _get_id("test_file")
        file_content = str("")
        file_metadata = kappy.FileMetadata(file_id,0)
        file_object = kappy.File(file_metadata,file_content)
        runtime.file_create(file_object)

        print("Getting the file names")
        file_names = [entry.id for entry in runtime.file_info()]

        print("Running checks...")
        self.assertIn(file_id, file_names)
        self.assertEqual(runtime.file_get(file_id).get_content(), file_content)
        runtime.file_delete(file_id)
        try:
            runtime.file_delete(file_id)
            self.fail()
        except kappy.KappaError:
            pass

    def test_parse_multiple_files(self):
        runtime = self.getRuntime(project_id=_get_id('test_proj'))
        file_1_id = _get_id("file1.ka")
        file_2_id = _get_id("file2.ka")
        test_dir = path.join(MODELS_DIR, "test_suite", "compiler", "file_order")
        with open(path.join(test_dir, "file2.ka")) as file_2:
            with open(path.join(test_dir, "file1.ka")) as file_1:
                data_1 = file_1.read()
                file_1_metadata = kappy.FileMetadata(file_1_id,1)
                file_1_object = kappy.File(file_1_metadata,data_1)
                runtime.file_create(file_1_object)

                data_2 = file_2.read()
                file_2_metadata = kappy.FileMetadata(file_2_id,2)
                file_2_object = kappy.File(file_2_metadata,data_2)
                runtime.file_create(file_2_object)
                runtime.project_parse()
                with self.assertRaises(kappy.KappaError):
                    runtime.file_create(file_2_object)
                file_names = [entry.id for entry in runtime.file_info()]
                self.assertIn(file_1_id,file_names)
                self.assertIn(file_2_id,file_names)

    def test_run_simulation(self):
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        file_id = str(uuid.uuid1())
        with open(path.join(MODELS_DIR, "abc-pert.ka")) as kappa_file:
            data = kappa_file.read()
            file_content = str(data)
            file_metadata = kappy.FileMetadata(file_id,0)
            file_object = kappy.File(file_metadata,file_content)
            runtime.file_create(file_object)
            runtime.project_parse()
            pause_condition = "[T] > 10.0"
            simulation_parameter = kappy.SimulationParameter(0.1,pause_condition)
            runtime.simulation_start(simulation_parameter)

            simulation_info = runtime.simulation_info()

            while simulation_info["simulation_info_progress"]["simulation_progress_is_running"] :
                time.sleep(1)
                simulation_info = runtime.simulation_info()

            # test that no limit returns all entries
            last_status = runtime.simulation_plot()
            test_count = 101
            self.assertEqual(test_count, len(last_status['series']))

            print(simulation_info)
            plot_limit_offset = 100
            test_time = 10.0
            test_count = 1
            limit = kappy.PlotLimit(plot_limit_offset)
            last_status = runtime.simulation_plot(limit)
            self.assertEqual(test_count, len(last_status['series']))
            self.assertEqual(test_time, last_status['series'][0][0])

            plot_limit_offset = 10
            plot_limit_points = 1
            test_time = 1.0
            test_count = 1
            limit = kappy.PlotLimit(plot_limit_offset,plot_limit_points)
            last_status = runtime.simulation_plot(limit)
            self.assertEqual(test_count, len(last_status['series']))
            self.assertEqual(test_time, last_status['series'][0][0])

            plot_limit_offset = 50
            test_time = 10.0
            test_count = 51
            limit = kappy.PlotLimit(plot_limit_offset)
            last_status = runtime.simulation_plot(limit)
            self.assertEqual(test_count, len(last_status['series']))
            self.assertEqual(test_time, last_status['series'][0][0])

            runtime.simulation_continue("[T] > 35")

            simulation_info = runtime.simulation_info()

            while simulation_info["simulation_info_progress"]["simulation_progress_is_running"] :
                time.sleep(1)
                simulation_info = runtime.simulation_info()

            # test that no limit returns all entries
            last_status = runtime.simulation_plot()
            self.assertEqual(351, len(last_status['series']))


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
        subprocess.Popen([
            self.websim,
            '--shutdown-key', self.key,
            '--port', str(self.port),
            '--level', 'fatal',  # TODO: This is temporary until --log works.
            '--log', 'WebSim_test.log'
            ])
        time.sleep(1)
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
        time.sleep(1)
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

    def test_info(self):
        """Check if the server can return information about the service."""
        project_id = _get_id("dummy")
        runtime = self.getRuntime(project_id)
        info = runtime.get_info()
        self.assertIsNotNone('environment_projects' in info)
        self.assertIsNotNone('environment_build' in info)

class StdClientTest(_KappaClientTest):
    """ Integration test for kappa client"""

    def getRuntime(self,project_id):
        return kappy.KappaStd(BIN_DIR)

if __name__ == '__main__':
    module_name = sys.modules[__name__].__file__
    print("Running nose for package: %s" % module_name)

    result = nose.run(argv=[sys.argv[0], module_name] + sys.argv[1:])
