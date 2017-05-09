""" Standard channel api client for the kappa programming language
"""

import subprocess
import threading
import json
import abc
import kappa_common

class KappaStd(object):
    def __init__(self, path, delimiter='\x1e', args=None):
        self.delimiter = delimiter
        sim_args = [path,
                    "--delimiter",
                    "\\x{:02x}".format(ord(self.delimiter)),
                    "--log",
                    "-", ]
        if args:
            sim_args = sim_args + args
        self.lock = threading.Lock()
        self.message_id = 0
        self.popen = subprocess.Popen(sim_args,
                                      stdin=subprocess.PIPE,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT)

    def get_message_id(self):
        self.message_id += 1
        return self.message_id

    def dispatch(self, method, args):
        try:
            self.lock.acquire()
            message_id = self.get_message_id()
            message = {'id': message_id,
                       'data': [method, args]}
            message = "{0}{1}".format(json.dumps(message), self.delimiter)
            self.popen.stdin.write(message.encode('utf-8'))
            self.popen.stdin.flush()
            buffer = bytearray()
            c = self.popen.stdout.read(1)
            while c != self.delimiter.encode('utf-8') and c:
                buffer.extend(c)
                c = self.popen.stdout.read(1)
            response = json.loads(buffer.decode('utf-8'))
            if response["id"] != message_id:
                raise KappaError("expect id {0} got {1}".format(response["id"], message_id))
            else:
                return self.projection(response)

        finally:
            self.lock.release()

    @abc.abstractmethod
    def projection(self, response): pass

    def shutdown(self,):
        self.popen.stdin.close()
        self.popen.stdout.close()
        self.popen.kill()

    def projection(self,response):
        result_data = response["data"][1]["result_data"]
        data = result_data[1]
        if result_data[0] == "Ok":
            return data
        else:
            raise kappa_common.KappaError(data)

    def info(self):
        return(self.dispatch("EnvironmentInfo",None))

    def project_create(self,project_id):
        project_parameter = { "project_parameter_project_id" : project_id }
        return(self.dispatch("ProjectCreate",project_parameter))
    def project_info(self):
        return(self.dispatch("ProjectCatalog",None))

    def project_delete(self,project_id):
        return(self.dispatch("ProjectDelete",project_id))

    def project_parse(self,project_id):
        return(self.dispatch("ProjectParse",project_id))

    def file_create(self,project_id,file_object):
        file_data = file_object.toJSON()
        return(self.dispatch("FileCreate",[project_id,file_data]))

    def file_delete(self,project_id,file_id):
        return(self.dispatch("FileDelete",[project_id,file_id]))

    def file_get(self,project_id,file_id):
        f = self.dispatch("FileGet",[project_id,file_id])
        return(kappa_common.hydrate_file(f))

    def file_info(self,project_id):
        info = self.dispatch("FileCatalog",project_id)
        #return(list(map(hydrate_filemetadata,info)))
        return(info)

    def simulation_delete(self,project_id):
        return(self.dispatch("SimulationDelete",[project_id]))

    def simulation_detail_file_line(self,project_id,file_line_id):
        return(self.dispatch("SimulationDetailFileLine",
                             [project_id,file_line_id]))

    def simulation_detail_flux_map(self,project_id,flux_map_id):
        return(self.dispatch("SimulationDetailFluxMap",
                             [project_id,flux_map_id]))

    def simulation_detail_log_message(self,project_id):
        return(self.dispatch("SimulationDetailLogMessage",
                             [project_id]))

    def simulation_detail_plot(self,project_id,plot_parameter = None):
        if plot_parameter :
            parameter = plot_parameter.toJSON()
        else:
            parameter = kappa_common.PlotParameter().toJSON()

        return(self.dispatch("SimulationDetailPlot",
                             [project_id,parameter]))

    def simulation_detail_snapshot(self,project_id,snapshot_id):
        return(self.dispatch("SimulationDetailSnapshot",
                             [project_id,snapshot_id]))

    def simulation_info(self,project_id):
        return(self.dispatch("SimulationInfo",project_id))

    def simulation_info_file_line(self,project_id):
        return(self.dispatch("SimulationCatalogFileLine",project_id))

    def simulation_info_flux_map(self,project_id):
        return(self.dispatch("SimulationCatalogFluxMap",project_id))

    def simulation_info_snapshot(self,project_id):
        return(self.dispatch("SimulationCatalogSnapshot",
                             project_id))

    def simulation_pause(self,project_id):
        return(self.dispatch("SimulationPause",
                             project_id))

    def simulation_perturbation(self,project_id,perturbation_code):
        return(self.dispatch("SimulationPause",
                             [project_id ,
                              { "perturbation_code" : perturbation_code }]))

    def simulation_start(self,project_id,simulation_parameter):
        return(self.dispatch("SimulationStart",
                             [project_id,simulation_parameter.toJSON()]))
