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
import kappa_common
import kappa_client
import uuid

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


    def test_info(self):
        """ the the ability of the server to return
            information about the service.
        """
        runtime = self.getRuntime()
        info = runtime.info()
        self.assertIsNotNone('environment_simulations' in info)
        self.assertEqual(info['environment_simulations'], 0)
        self.assertIsNotNone('environment_projects' in info)
        self.assertIsNotNone('environment_build' in info)

    def test_project_crud(self):
        runtime = self.getRuntime()
        project_id = str(uuid.uuid1())
        self.assertEqual(project_id,runtime.project_create(project_id))
        print(project_id)
        project_ids = kappa_client.project_catalog_project_id(runtime.project_info())
        self.assertIn(project_id,project_ids)
        print(runtime.project_info())
        runtime.project_delete(project_id)
        self.assertNotIn(project_id,runtime.project_info())
        try:
            runtime.project_delete(project_id)
            self.fail()
        except:
            None

    def test_file_crud(self):
        runtime = self.getRuntime()
        project_id = str(uuid.uuid1())
        self.assertEqual(project_id,runtime.project_create(project_id))
        file_id = str(uuid.uuid1())
        file_content = str("")
        file_metadata = kappa_client.FileMetadata(file_id,0)
        file_object = kappa_client.File(file_metadata,file_content)
        runtime.file_create(project_id,file_object)
        file_names = kappa_client.file_catalog_file_id(runtime.file_info(project_id))
        #file_info = runtime.file_info(project_id)
        #file_names = list(map(lambda x: x.get_file_id(),file_info))
        self.assertIn(file_id,file_names)
        self.assertEqual(runtime.file_get(project_id,file_id).get_content(),
                         file_content)
        runtime.file_delete(project_id,file_id)
        try:
            runtime.file_delete(project_id,file_id)
            self.fail()
        except:
            None

    def test_run_simulationd(self):
        runtime = self.getRuntime()
        project_id = str(uuid.uuid1())
        self.assertEqual(project_id,runtime.project_create(project_id))
        file_id = str(uuid.uuid1())
        with open("../models/abc-pert.ka") as kappa_file:
            data = kappa_file.read()
            file_content = str(data)
            file_metadata = kappa_client.FileMetadata(file_id,0)
            file_object = kappa_client.File(file_metadata,file_content)
            runtime.file_create(project_id,file_object)
            simulation_id = str(uuid.uuid1())
            pause_condition = "[T] > 10.0"
            simulation_parameter = kappa_client.SimulationParameter(0.1,simulation_id,pause_condition)
            runtime.simulation_start(project_id,simulation_parameter)

            simulation_info = runtime.simulation_info(project_id,simulation_id)

            while simulation_info["simulation_info_progress"]["simulation_progress_is_running"] :
                time.sleep(1)
                simulation_info = runtime.simulation_info(project_id,simulation_id)

            # test that no limit returns all entries
            last_status = runtime.simulation_detail_plot(project_id,simulation_id)
            test_count = 101
            self.assertEqual(test_count, len(last_status['plot_detail_plot']['plot_time_series']))

            print(simulation_info)
            plot_limit_offset = 100
            test_time = 10.0
            test_count = 1
            limit = kappa_client.PlotLimit(plot_limit_offset)
            last_status = runtime.simulation_detail_plot(project_id,simulation_id,kappa_client.PlotParameter(limit))
            self.assertEqual(test_count, len(last_status['plot_detail_plot']['plot_time_series']))
            self.assertEqual(test_time, last_status['plot_detail_plot']['plot_time_series'][0][0])

            plot_limit_offset = 10
            plot_limit_points = 1
            test_time = 1.0
            test_count = 1
            limit = kappa_client.PlotLimit(plot_limit_offset,plot_limit_points)
            last_status = runtime.simulation_detail_plot(project_id,simulation_id,kappa_client.PlotParameter(limit))
            self.assertEqual(test_count, len(last_status['plot_detail_plot']['plot_time_series']))
            self.assertEqual(test_time, last_status['plot_detail_plot']['plot_time_series'][0][0])

            print(simulation_info)
            plot_limit_offset = 50
            test_time = 10.0
            test_count = 51
            limit = kappa_client.PlotLimit(plot_limit_offset)
            last_status = runtime.simulation_detail_plot(project_id,simulation_id,kappa_client.PlotParameter(limit))
            self.assertEqual(test_count, len(last_status['plot_detail_plot']['plot_time_series']))
            self.assertEqual(test_time, last_status['plot_detail_plot']['plot_time_series'][0][0])


class RestClientTest(KappaClientTest,unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(self):
        """ set up unit test by launching client"""
        self.websim = "../WebSim.native"
        self.key = self.generate_key()
        self.port = 6666
        command_format = "{0} --development --shutdown-key {1} --port {2} --level debug"
        subprocess.Popen(command_format.format(self.websim, self.key, self.port).split())
        time.sleep(1)
        self.endpoint = "http://127.0.0.1:{0}".format(self.port)
    def getRuntime(self):
        return(kappa_client.KappaRest(self.endpoint))
    @classmethod
    def tearDownClass(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime(self)
        print(runtime.shutdown(self.key))

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
        super(KappaClientTest, self).__init__(*args, **kwargs)

class StdClientTest(KappaClientTest,unittest.TestCase):
    """ Integration test for kappa client"""
    @classmethod
    def setUpClass(self):
        """ set up unit test by launching client"""
        self.stdsim = "../StdSim.native"

    def getRuntime(self):
        return(kappa_client.KappaStd(self.stdsim))
    @classmethod
    def tearDownClass(self):
        """ tear down test by shutting down"""
        runtime = self.getRuntime(self)
        print(runtime.shutdown())

    def __init__(self, *args, **kwargs):
        super(KappaClientTest, self).__init__(*args, **kwargs)

if __name__ == '__main__':
    unittest.main(verbosity=2)
