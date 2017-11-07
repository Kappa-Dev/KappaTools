"""Integration test for kappa clients"""
import unittest
import nose

import json
import subprocess
import random
import string
import time
import uuid
from os.path import dirname, join, normpath, pardir, abspath

import kappy
from util import run_nose

KASIM_DIR = normpath(join(dirname(abspath(__file__)), *([pardir]*2)))
KAPPA_BIN = os.path.join(KASIN_DIR,"bin")

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

    def parse_multiple_files(self):
        runtime = self.getRuntime(project_id=_get_id('test_proj'))
        file_1_id = _get_id("file1.ka")
        file_2_id = _get_id("file2.ka")
        test_dir = "../models/test_suite/compiler/file_order/"
        with open(test_dir+"file2.ka") as file_2:
            with open(test_dir+"file1.ka") as file_1:
                data_1 = file_1.read()
                file_1_metadata = kappy.FileMetadata(file_1_id,1)
                file_1_object = kappy.File(file_1_metadata,data_1)
                runtime.file_create(file_1_object)

                data_2 = file_2.read()
                file_2_metadata = kappy.FileMetadata(file_2_id,2)
                file_2_object = kappy.File(file_2_metadata,data_2)
                runtime.file_create(file_1_object)
                runtime.project_parse()
                with self.assertRaises(kappy.KappaError):
                    runtime.file_create(file_2_object)
                file_names = [entry.id for entry in runtime.file_info()]
                self.assertIn(file_1_id,file_names)
                self.assertIn(file_2_id,file_names)

    def test_run_simulationd(self):
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        file_id = str(uuid.uuid1())
        with open("../models/abc-pert.ka") as kappa_file:
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
        """ initalize test by launching kappa server """
        self.websim = join(KAPPA_BIN, "WebSim")
        self.key = self.generate_key()
        self.port = 6666
        self.endpoint = "http://127.0.0.1:{0}".format(self.port)

    def setUp(self):
        """ set up unit test by launching client"""
        print("Starting server")
        subproccess.Popen([
            self.websim,
            '--shutdown-key', self.key,
            '--port', str(self.port),
            '--level', 'info'
            ])
        time.sleep(1)
        print("Started")
        return

    def tearDown(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime("__foo")
        print("Closing server...")
        resp = runtime.shutdown(self.key)
        print("Closed", resp)
        return

    def getRuntime(self,project_id):
        return kappy.KappaRest(self.endpoint,project_id)

    @classmethod
    def generate_key(cls):
        """ generate random key for kappa server. """
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
        return(kappy.KappaStd(KAPPA_BIN))

if __name__ == '__main__':
    fpath = os.path.abspath(__file__)
    print("Running nose for package: %s" % fname)
    return nose.run(argv=[sys.argv[0], fpath] + sys.argv[1:])
