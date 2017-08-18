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

    def __del__(self):
        self.shutdown()

    def get_message_id(self):
        self.message_id += 1
        return self.message_id

    def dispatch(self, method, args=None):
        if args is not None:
            data = [method, args]
        else:
            data = method

        try:
            self.lock.acquire()
            message_id = self.get_message_id()
            message = {'id': message_id,'data': data}
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

    def shutdown(self):
        self.popen.stdin.close()
        self.popen.stdout.close()
        self.popen.kill()

    def projection(self,response):
        result_data = response["data"]["result_data"]
        data = result_data[1]
        if result_data[0] == "Ok":
            return data[1]
        else:
            raise kappa_common.KappaError(data)

    def project_parse(self,overwrites=[]):
        return(self.dispatch("ProjectParse",overwrites))

    def file_create(self,file_object):
        file_data = file_object.toJSON()
        return(self.dispatch("FileCreate",file_data))

    def file_delete(self,file_id):
        return(self.dispatch("FileDelete",file_id))

    def file_get(self,file_id):
        f = self.dispatch("FileGet",file_id)
        return(kappa_common.hydrate_file(f))

    def file_info(self):
        info = self.dispatch("FileCatalog")
        #return(list(map(hydrate_filemetadata,info)))
        return(info)

    def simulation_delete(self):
        return(self.dispatch("SimulationDelete"))

    def simulation_detail_file_line(self,file_line_id):
        return(self.dispatch("SimulationDetailFileLine",file_line_id))

    def simulation_detail_flux_map(self,flux_map_id):
        return(self.dispatch("SimulationDetailFluxMap",flux_map_id))

    def simulation_detail_log_message(self):
        return(self.dispatch("SimulationDetailLogMessage"))

    def simulation_detail_plot(self,limit=None):
        if limit is not None:
            parameter = limit.toJSON()
        else:
            parameter = kappa_common.PlotLimit().toJSON()
        return(self.dispatch("SimulationDetailPlot",parameter))

    def simulation_detail_snapshot(self,snapshot_id):
        return(self.dispatch("SimulationDetailSnapshot",snapshot_id))

    def simulation_info(self):
        return(self.dispatch("SimulationInfo"))

    def simulation_info_file_line(self):
        return(self.dispatch("SimulationCatalogFileLine"))

    def simulation_info_flux_map(self):
        return(self.dispatch("SimulationCatalogFluxMap"))

    def simulation_info_snapshot(self):
        return(self.dispatch("SimulationCatalogSnapshot"))

    def simulation_pause(self):
        return(self.dispatch("SimulationPause"))

    def simulation_perturbation(self,perturbation_code):
        return(self.dispatch("SimulationPerturbation",
                              { "perturbation_code" : perturbation_code }))

    def simulation_start(self,simulation_parameter):
        return(self.dispatch("SimulationStart",
                             simulation_parameter.toJSON()))
