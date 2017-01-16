""" Web api client for the kappa programming language
"""

import urllib.error
import urllib.request
import urllib.parse
import sys
import getopt
import time
import subprocess
import json
import uuid
import threading
import kappa_common

class FileMetadata(object):
    def __init__(self,
                 file_metadata_id,
                 file_metadata_position,
                 file_metadata_compile = True ,
                 file_metadata_hash = None):
        self.file_metadata_id = file_metadata_id
        self.file_metadata_position = file_metadata_position
        self.file_metadata_compile = file_metadata_compile
        self.file_metadata_hash = file_metadata_hash
    def toJSON(self):
        return({ "file_metadata_compile" : self.file_metadata_compile ,
                 "file_metadata_hash" : self.file_metadata_hash ,
                 "file_metadata_id" : self.file_metadata_id ,
                 "file_metadata_position" : self.file_metadata_position })
    def get_file_id(self):
        return(self.file_metadata_id)

class File(object):
    def __init__(self,
                 file_metadata,
                 file_content):
        self.file_metadata = file_metadata
        self.file_content = file_content

    def toJSON(self):
        return({ "file_metadata" : self.file_metadata.toJSON() ,
                 "file_content" : self.file_content })

    def get_file_id(self):
        return(self.file_metadata.get_file_id())
    def get_content(self):
        return(self.file_content)

class SimulationParameter(object):
    def __init__(self,
                 simulation_plot_period,
                 simulation_id,
                 simulation_max_time = None,
                 simulation_max_events = None) :
        self.simulation_plot_period = simulation_plot_period
        self.simulation_id = simulation_id
        self.simulation_max_time = simulation_max_time
        self.simulation_max_events = simulation_max_events

    def toJSON(self):
        return({ "simulation_plot_period" : self.simulation_plot_period,
                 "simulation_id" : self.simulation_id,
                 "simulation_max_time" : self.simulation_max_time,
                 "simulation_max_events" : self.simulation_max_events })

# the outputs are returned as json hydrate to an object model
def hydrate_filemetada (info):
    return(FileMetadata(info["file_metadata_id"],
                        info["file_metadata_position"],
                        info["file_metadata_compile"],
                        info["file_metadata_hash"]))

def hydrate_file (info):
    return(File(info["file_metadata"],
                info["file_content"]))

class KappaStd(kappa_common.StdBase):
    def __init__(self, path , delimiter = '\x1e', ):
        kappa_common.StdBase.__init__(self ,
                                      path ,
                                      delimiter ,
                                      args = ["--development"],)

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
        project_parameter = { "project_id" : project_id }
        return(self.dispatch("ProjectCreate",project_parameter))
    def project_info(self):
        return(self.dispatch("ProjectInfo",None))

    def project_delete(self,project_id):
        return(self.dispatch("ProjectDelete",project_id))

    def file_create(self,project_id,file_object):
        file_data = file_object.toJSON()
        return(self.dispatch("FileCreate",[project_id,file_data]))

    def file_delete(self,project_id,file_id):
        return(self.dispatch("FileDelete",[project_id,file_id]))

    def file_get(self,project_id,file_id):
        f = self.dispatch("FileGet",[project_id,file_id])
        return(hydrate_file(f))

    def file_info(self,project_id):
        info = self.dispatch("FileInfo",project_id)
        return(list(map(hydrate_filemetada,info)))

    def simulation_delete(self,project_id,simulation_id):
        return(self.dispatch("SimulationDelete",[project_id,simulation_id]))

    def simulation_detail_file_line(self,project_id,simulation_id,file_line_id):
        return(self.dispatch("SimulationDetailFileLine",
                             [project_id,simulation_id,file_line_id]))

    def simulation_detail_flux_map(self,project_id,simulation_id,flux_map_id):
        return(self.dispatch("SimulationDetailFluxMap",
                             [project_id,simulation_id,flux_map_id]))

    def simulation_detail_log_message(self,project_id,simulation_id):
        return(self.dispatch("SimulationDetailLogMessage",
                             [project_id,simulation_id]))

    def simulation_detail_plot(self,project_id,simulation_id):
        return(self.dispatch("SimulationDetailPlot",
                             [project_id,simulation_id]))

    def simulation_detail_snapshot(self,project_id,simulation_id,snapshot_id):
        return(self.dispatch("SimulationDetailSnapshot",
                             [project_id,simulation_id,snapshot_id]))

    def simulation_info(self,project_id,simulation_id):
        return(self.dispatch("SimulationInfo",
                             [project_id,simulation_id]))

    def simulation_info_file_line(self,project_id,simulation_id):
        return(self.dispatch("SimulationInfoFileLine",
                             [project_id,simulation_id]))

    def simulation_info_flux_map(self,project_id,simulation_id):
        return(self.dispatch("SimulationInfoFluxMap",
                             [project_id,simulation_id]))

    def simulation_info_snapshot(self,project_id,simulation_id):
        return(self.dispatch("SimulationInfoSnapshot",
                             [project_id,simulation_id]))

    def simulation_list(self,project_id):
        return(self.dispatch("SimulationInfoSnapshot",
                             project_id))

    def simulation_pause(self,project_id,simulation_id):
        return(self.dispatch("SimulationPause",
                             [project_id,simulation_id]))

    def simulation_perturbation(self,project_id,simulation_id,perturbation_code):
        return(self.dispatch("SimulationPause",
                             [project_id ,
                              simulation_id ,
                              { "perturbation_code" : perturbation_code }]))

    def simulation_start(self,project_id,simulation_parameter):
        return(self.dispatch("SimulationStart",
                             [project_id,simulation_parameter.toJSON()]))


class KappaRest(kappa_common.RestBase):
    def __init__(self, endpoint):
        kappa_common.RestBase.__init__(self ,"{0}/v2".format(endpoint))

    def info(self):
        method = "GET"
        url = "{0}".format(self.url)
        body = None
        return(self.dispatch(method,url,body))

    def project_create(self,project_id):
        method = "POST"
        url = "{0}/projects".format(self.url)
        body = { "project_id" : project_id }
        return(self.dispatch(method,url,body))

    def project_info(self):
        method = "GET"
        url = "{0}/projects".format(self.url)
        body = None
        return(self.dispatch(method,url,body))

    def project_delete(self,project_id):
        method = "DELETE"
        url = "{0}/projects/{1}".format(self.url,project_id)
        body = None
        return(self.dispatch(method,url,body))

    def file_create(self,project_id,file_object):
        method = "POST"
        url = "{0}/projects/{1}/files".format(self.url,project_id)
        body = file_object.toJSON()
        return(self.dispatch(method,url,body))

    def file_delete(self,project_id,file_id):
        method = "DELETE"
        url = "{0}/projects/{1}/files/{2}".format(self.url,project_id,file_id)
        body = None
        return(self.dispatch(method,url,body))

    def file_get(self,project_id,file_id):
        method = "GET"
        url = "{0}/projects/{1}/files/{2}".format(self.url,project_id,file_id)
        body = None
        file = self.dispatch(method,url,body)
        return(hydrate_file(file))

    def file_info(self,project_id):
        method = "GET"
        url = "{0}/projects/{1}/files".format(self.url,project_id)
        body = None
        info = self.dispatch(method,url,body)
        return(list(map(hydrate_filemetada,info)))

    def simulation_delete(self,project_id,simulation_id):
        method = "DELETE"
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,project_id,simulation_id)
        body = None
        return(self.dispatch(method,url,body))

    def simulation_detail_file_line(self,project_id,simulation_id,file_line_id):
        url = "{0}/projects/{1}/simulations/{2}/file_lines/{3}".format(self.url,project_id,simulation_id,file_line_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_flux_map(self,project_id,simulation_id,flux_map_id):
        url = "{0}/projects/{1}/simulations/{2}/fluxmaps/{3}".format(self.url,project_id,simulation_id,flux_map_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_log_message(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}/logmessages".format(self.url,project_id,simulation_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_plot(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}/plot".format(self.url,project_id,simulation_id)
        return(self.dispatch("GET",url,None))


    def simulation_detail_snapshot(self,project_id,simulation_id,snapshot_id):
        url = "{0}/projects/{1}/simulations/{2}/snapshots/{3}".format(self.url,project_id,snapshot_id)
        return(self.dispatch("GET",url,None))

    def simulation_info(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,project_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_file_line(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}/file_lines".format(self.url,project_id,simulation_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_flux_map(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}/fluxmaps".format(self.url,project_id,simulation_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_snapshot(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}/snapshots".format(self.url,
                                                                  project_id,snapshot_id)
        return(self.dispatch("GET",url,None))

    def simulation_list(self,project_id):
        url = "{0}/projects/{1}/simulations".format(self.url,
                                                    project_id)
        return(self.dispatch("GET",url,None))

    def simulation_delete(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,
                                                        project_id,
                                                        snapshot_id)
        return(self.dispatch("DELETE",url,None))

    def simulation_pause(self,project_id,simulation_id):
        message = { "action" : "pause" }
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,
                                                        project_id,
                                                        snapshot_id)
        return(self.dispatch("PUT",url,message ))

    def simulation_perturbation(self,project_id,simulation_id,perturbation_code):
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,
                                                        project_id,
                                                        snapshot_id)
        message = { "perturbation_code" : perturbation_code }
        return(self.dispatch("PUT",url,message ))

    def simulation_start(self,project_id,simulation_parameter):
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,project_id,snapshot_id)
        message = simulation_parameter.toJSON()
        return(self.dispatch("PUT",url,message ))

def scratch():
    file_id = str(uuid.uuid1())
    project_id_1 = "1"
    project_id_2 = "2"
    project_id_3 = "3"
    kappaStd = KappaStd("/home/mwm1/Work/KaSim/StdSim.native")
    print(kappaStd.info())
    print(kappaStd.project_create(project_id_1))
    # print(kappaStd.project_create(project_id_2))
    # print(kappaStd.info())
    print(kappaStd.project_info())
    # file = File(FileMetadata(file_id,0),
    #             "%agent: A(x,c) # Declaration of agent A ")
    # print(kappaStd.file_create(project_id_1,file))
    # print(kappaStd.file_info(project_id_1))
    # print(kappaStd.file_get(project_id_1,file_id))
    # print(kappaStd.file_delete(project_id_1,file_id))
    # print(kappaStd.file_info(project_id_1))

    # file_id = str(uuid.uuid1())
    # project_id_1 = str(uuid.uuid1())
    # print(kappaStd.project_create(project_id_1))
    # inputfile = "/home/mwm1/Work/KaSim/models/abc.ka"
    # with open(inputfile) as f:
    #     code = f.read()
    #     file = File(FileMetadata(file_id,0),code)
    #     print(kappaStd.file_create(project_id_1,file))
    #     simulation_parameter = SimulationParameter(1,project_id_1,simulation_max_events = 10)
    #     print(kappaStd.simulation_start(project_id_1,simulation_parameter))
    #     print(kappaStd.simulation_info(project_id_1,project_id_1))
    # kappaStd.shutdown()


def main():
    None

if __name__ == "__main__":
    main()
