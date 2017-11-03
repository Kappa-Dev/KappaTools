"""Integration test for kappa clients"""
import json
import urllib
import urllib.error
import urllib.request
import urllib.parse
import unittest
import subprocess
import random
import string
import time
from kappy import kappa_rest
import kappy
import uuid

def file_catalog_file_id (file_catalog):
    return(map((lambda entry: entry.file_metadata_id),file_catalog))

class KappaClientTest(object):
    def check_integration_test(self, integration_test):
        """ run an integration test by requesting a url with
            a given method and checking the response code.
        """
        method = integration_test['method']
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        url = "{0}{1}".format(self.endpoint, integration_test['url'])
        arguments = integration_test['arguments']
        request = urllib.request.Request(url,
                                         integration_test['arguments'])
        request.get_method = lambda : method
        connection = opener.open(request)
        self.assertEqual(connection.code, integration_test['code'])


    # def test_project_crud(self):
    #     runtime = self.getRuntime()
    #     project_id = str(uuid.uuid1())
    #     runtime.project_create(project_id)
    #     print(project_id)
    #     project_ids = kappa_client.project_catalog_project_id(runtime.project_info())
    #     self.assertIn(project_id,project_ids)
    #     print(runtime.project_info())
    #     runtime.project_delete(project_id)
    #     self.assertNotIn(project_id,runtime.project_info())
    #     try:
    #         runtime.project_delete(project_id)
    #         self.fail()
    #     except kappy.KappaError as exception:
    #         None

    def test_file_crud(self):
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        file_id = str(uuid.uuid1())
        file_content = str("")
        file_metadata = kappy.FileMetadata(file_id,0)
        file_object = kappy.File(file_metadata,file_content)
        runtime.file_create(file_object)
        file_names = file_catalog_file_id(runtime.file_info())
        self.assertIn(file_id,file_names)
        self.assertEqual(runtime.file_get(file_id).get_content(),
                         file_content)
        runtime.file_delete(file_id)
        try:
            runtime.file_delete(file_id)
            self.fail()
        except:
            None

    def parse_multiple_files(self):
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        file_1_id = str(uuid.uuid1())
        file_2_id = str(uuid.uuid1())
        test_dir = "../../models/test_suite/compiler/file_order/"
        with open(test_dir+"file2.ka") as file_2:
            with open(test_dir+"file1.ka") as file_1:
                data_1 = file_1.read()
                file_1_metadata = kappy.FileMetadata(file_1_id,0)
                file_1_object = kappy.File(file_1_metadata,data_1)
                runtime.file_create(file_1_object)

                data_2 = file_2.read()
                file_2_metadata = kappy.FileMetadata(file_2_id,0)
                file_2_object = kappy.File(file_2_metadata,data_2)
                runtime.project_parse()
                with self.assertRaises(kappy.KappaError):
                    runtime.file_create(file_2_object)
                file_names = list(file_catalog_file_id(runtime.file_info()))
                self.assertIn(file_1_id,file_names)
                self.assertIn(file_2_id,file_names)

    def test_run_simulationd(self):
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        file_id = str(uuid.uuid1())
        with open("../../models/abc-pert.ka") as kappa_file:
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


class RestClientTest(KappaClientTest,unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(self):
        """ set up unit test by launching client"""
        self.websim = "../../bin/WebSim"
        self.key = self.generate_key()
        self.port = 6666
        command_format = "{0} --shutdown-key {1} --port {2} --level debug"
        subprocess.Popen(command_format.format(self.websim, self.key, self.port).split())
        time.sleep(1)
        self.endpoint = "http://127.0.0.1:{0}".format(self.port)
    def getRuntime(self,project_id):
        return(kappa_rest.KappaRest(self.endpoint,project_id))
    @classmethod
    def tearDownClass(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime(self,"__foo")
        print(runtime.shutdown(self.key))

    @classmethod
    def generate_key(cls):
        """ generate random key for kappa server. """
        return ''.join(random.
                       SystemRandom().
                       choice(string.ascii_uppercase + string.digits)
                       for _ in range(100))

    def test_info(self):
        """ the the ability of the server to return
            information about the service.
        """
        project_id = str(uuid.uuid1())
        runtime = self.getRuntime(project_id)
        info = runtime.get_info()
        self.assertIsNotNone('environment_projects' in info)
        self.assertIsNotNone('environment_build' in info)

    def __init__(self, *args, **kwargs):
        """ initalize test by launching kappa server """
        self.websim = "../../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        super(KappaClientTest, self).__init__(*args, **kwargs)

class StdClientTest(KappaClientTest,unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(self):
        """ set up unit test by launching client"""

    def getRuntime(self,project_id):
        return(kappy.KappaStd())
    @classmethod
    def tearDownClass(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime(self,"__foo")
        print(runtime.shutdown())

    def __init__(self, *args, **kwargs):
        super(KappaClientTest, self).__init__(*args, **kwargs)

if __name__ == '__main__':
    unittest.main(verbosity=2)
