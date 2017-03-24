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
                 file_metadata_hash = None ,
                 file_version = []):
        self.file_metadata_id = file_metadata_id
        self.file_metadata_position = file_metadata_position
        self.file_metadata_compile = file_metadata_compile
        self.file_metadata_hash = file_metadata_hash
        self.file_version = file_version
    def toJSON(self):
        return({ "file_metadata_compile" : self.file_metadata_compile ,
                 "file_metadata_hash" : self.file_metadata_hash ,
                 "file_metadata_id" : self.file_metadata_id ,
                 "file_metadata_position" : self.file_metadata_position ,
                 "file_metadata_version" : self.file_version })

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
                 simulation_pause_condition,
                 simulation_seed = None) :
        self.simulation_plot_period = simulation_plot_period
        self.simulation_id = simulation_id
        self.simulation_pause_condition = simulation_pause_condition
        self.simulation_seed = simulation_seed

    def toJSON(self):
        return({ "simulation_plot_period" : self.simulation_plot_period,
                 "simulation_id" : self.simulation_id,
                 "simulation_pause_condition": self.simulation_pause_condition ,
                 "simulation_seed" : self.simulation_seed })

class PlotLimit(object):

    def __init__(self,
                 plot_limit_offset,
                 plot_limit_points = None) :
        self.plot_limit_offset = plot_limit_offset
        self.plot_limit_points = plot_limit_points

    def toURL(self):

        if self.plot_limit_offset :
            url_plot_limit_offset = "&plot_limit_offset={0}".format(self.plot_limit_offset)
        else :
            url_plot_limit_offset = ""

        if self.plot_limit_points :
            url_plot_limit_points = "&plot_limit_points={0}".format(self.plot_limit_points)
        else :
            url_plot_limit_points = ""

        url_plot_limit = "{0}{1}".format(url_plot_limit_offset,
                                         url_plot_limit_points)
        return url_plot_limit

    def toJSON(self):
        return({ "plot_limit_offset" : self.plot_limit_offset ,
                 "plot_limit_points" : self.plot_limit_points })

class PlotParameter(object):
    def __init__(self,
                 plot_parameter_plot_limit = None) :
        self.plot_parameter_plot_limit = plot_parameter_plot_limit

    def toJSON(self):
        if self.plot_parameter_plot_limit :
            limit = self.plot_parameter_plot_limit.toJSON()
        else:
            limit =  None
        return({ "plot_parameter_plot_limit" : limit })

    def toURL(self):
        if self.plot_parameter_plot_limit :
            url = self.plot_parameter_plot_limit.toURL()
        else:
            url = ""
        return url

def project_catalog_project_id (project_catalog):
    print(project_catalog)
    return(map((lambda entry: entry["project_id"]),project_catalog["project_list"]))

def file_catalog_file_id (file_catalog):
    return(map((lambda entry: entry["file_metadata_id"]),file_catalog["file_metadata_list"]))

def hydrate_file (info):
    return(File(info["file_metadata"],
                info["file_content"]))

class KappaStd(kappa_common.StdBase):
    def __init__(self, path , delimiter = '\x1e', ):
        kappa_common.StdBase.__init__(self ,
                                      path ,
                                      delimiter ,
                                      args = [],)

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
        return(hydrate_file(f))

    def file_info(self,project_id):
        info = self.dispatch("FileCatalog",project_id)
        #return(list(map(hydrate_filemetadata,info)))
        return(info)

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

    def simulation_detail_plot(self,project_id,simulation_id,plot_parameter = None):
        if plot_parameter :
            parameter = plot_parameter.toJSON()
        else:
            parameter = PlotParameter().toJSON()

        return(self.dispatch("SimulationDetailPlot",
                             [project_id,simulation_id,parameter]))

    def simulation_detail_snapshot(self,project_id,simulation_id,snapshot_id):
        return(self.dispatch("SimulationDetailSnapshot",
                             [project_id,simulation_id,snapshot_id]))

    def simulation_info(self,project_id,simulation_id):
        return(self.dispatch("SimulationInfo",
                             [project_id,simulation_id]))

    def simulation_info_file_line(self,project_id,simulation_id):
        return(self.dispatch("SimulationCatalogFileLine",
                             [project_id,simulation_id]))

    def simulation_info_flux_map(self,project_id,simulation_id):
        return(self.dispatch("SimulationCatalogFluxMap",
                             [project_id,simulation_id]))

    def simulation_info_snapshot(self,project_id,simulation_id):
        return(self.dispatch("SimulationCatalogSnapshot",
                             [project_id,simulation_id]))

    def simulation_list(self,project_id):
        return(self.dispatch("SimulationCatalog",
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
        body = { "project_parameter_project_id" : project_id }
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

    def project_parse(self,project_id):
        method = "GET"
        url = "{0}/projects/{1}/parse".format(self.url,project_id)
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
        #return(list(map(hydrate_filemetada,info)))
        return(info)

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

    def simulation_detail_plot(self,project_id,simulation_id,plot_parameter = None):
        if plot_parameter :
            parameter = "?{0}".format(plot_parameter.toURL())
        else:
            parameter =  ""
        url = "{0}/projects/{1}/simulations/{2}/plot{3}".format(self.url,project_id,simulation_id,parameter)
        print(url)
        return(self.dispatch("GET",url,None))

    def simulation_detail_snapshot(self,project_id,simulation_id,snapshot_id):
        url = "{0}/projects/{1}/simulations/{2}/snapshots/{3}".format(self.url,project_id,snapshot_id)
        return(self.dispatch("GET",url,None))

    def simulation_info(self,project_id,simulation_id):
        url = "{0}/projects/{1}/simulations/{2}".format(self.url,project_id,simulation_id)
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
        url = "{0}/projects/{1}/simulations".format(self.url,project_id)
        message = simulation_parameter.toJSON()
        return(self.dispatch("POST",url,message ))

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
    #     simulation_parameter =
    #       SimulationParameter(1,project_id_1,simulation_pause_condition = "[E] = 10")
    #     print(kappaStd.simulation_start(project_id_1,simulation_parameter))
    #     print(kappaStd.simulation_info(project_id_1,project_id_1))
    # kappaStd.shutdown()


def main():
    # command line
    argv = sys.argv[1:]
    cmd = "kappa_client.py"

    # default arguments
    inputfile = None  # if missing input file just get version
    url = "http://localhost:8080"
    pause_condition = "[false]"
    plot_period = 0.1
    seed = None

    try:
        opts, args = getopt.getopt(argv,
                                   "hk:u:t:e:pp:s",
                                   ["kappafile=",
                                    "url=",
                                    "max_time=",
                                    "max_event=",
                                    "plot_period=",
                                    "seed=", ])
    except:
        print(cmd
              + ' -k <kappafile> '
              + ' -u <url or path to stdsim> '
              + ' -t <max_time> '
              + ' -e <max_events> '
              + ' -pp <plot_period> '
              + ' -s <random_seed> ')

        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(cmd+' -i <inputfile> -o <outputfile>')
            sys.exit()
        elif opt in ("-k", "--kappafile"):
            inputfile = arg
        elif opt in ("-u", "--url"):
            url = arg
        elif opt in ("-t", "--max_time"):
            pause_condition = "[T]>"+arg+" || "+pause_condition
        elif opt in ("-e", "--max_events"):
            pause_condition = "[E]>"+arg+" || "+pause_condition
        elif opt in ("-pp", "--plot_period"):
            plot_period = float(arg)
        elif opt in ("-s", "--seed"):
            seed = int(arg)

    print('Input file is : {0} '.format(inputfile))
    print('Endpoint url : {0} '.format(url))
    print('Pause conditon : {0}'.format(pause_condition))
    print('Plot period : {0} '.format(plot_period))
    print('Random seed : {0} '.format(seed))

    try:
        if url.startswith('http'):
            runtime = KappaRest(url)
        else:
            runtime = KappaStd(url)
        project_id = "{0}-{1}".format(cmd,str(uuid.uuid1()))
        print("project_id : {0}".format(runtime.project_create(project_id)))
        if inputfile:
            with open(inputfile) as f:
                code = f.read()
                file_content = str(code)
                file_metadata = FileMetadata(inputfile,0)
                file_object = File(file_metadata,file_content)
                runtime.file_create(project_id,file_object)
                simulation_id = str(uuid.uuid1())
                print("simulation_id : {0}".format(simulation_id))


                end_time = 10.0
                simulation_parameter = SimulationParameter(plot_period,
                                                           simulation_id,
                                                           pause_condition,
                                                           seed)
                runtime.simulation_start(project_id,simulation_parameter)

                simulation_info = runtime.simulation_info(project_id,simulation_id)

                while simulation_info["simulation_info_progress"]["simulation_progress_is_running"] :
                    time.sleep(1)

                    percentage = ""
                    time_percentage = simulation_info["simulation_info_progress"]["simulation_progress_time_percentage"]
                    event_percentage = simulation_info["simulation_info_progress"]["simulation_progress_event_percentage"]

                    if time_percentage or time_percentage == 0 :
                        percentage = time_percentage
                    if event_percentage or event_percentage == 0 :
                        percentage = event_percentage

                    sys.stdout.write("..{0}.. ".format(percentage))
                    sys.stdout.flush()
                    simulation_info = runtime.simulation_info(project_id,simulation_id)

                print("")
                print("info")
                print(simulation_info)
                plot_detail = runtime.simulation_detail_plot(project_id,simulation_id,)
                print("plot")
                print(plot_detail)
        else:
            print(runtime.info())
    except kappa_common.KappaError as exception:
        print(exception.errors)
    return None
    None

if __name__ == "__main__":
    main()
