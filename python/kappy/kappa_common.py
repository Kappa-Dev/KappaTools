""" Shared functions of api client for the kappa programming language
"""

import json

class FileMetadata(object):

    def __init__(self,
                 file_metadata_id,
                 file_metadata_position,
                 file_metadata_compile = True ,
                 file_version = []):
        self.file_metadata_id = file_metadata_id
        self.file_metadata_position = file_metadata_position
        self.file_metadata_compile = file_metadata_compile
        self.file_version = file_version
    def toJSON(self):
        return({ "compile" : self.file_metadata_compile ,
                 "id" : self.file_metadata_id ,
                 "position" : self.file_metadata_position ,
                 "version" : self.file_version })

    def get_file_id(self):
        return(self.file_metadata_id)

def hydrate_file_metadata (info):
    return(
        FileMetadata(
            info["id"],
            info["position"],
            info["compile"] ,
            [] #hydrate_file_version(info["version"])
        ))

def stringAsFile(content,position=1):
    return(File(FileMetadata("inlined_input",position),content))

class File(object):

    def __init__(self,
                 file_metadata,
                 file_content):
        self.file_metadata = file_metadata
        self.file_content = file_content

    def toJSON(self):
        return({ "metadata" : self.file_metadata.toJSON() ,
                 "content" : self.file_content })

    def get_file_id(self):
        return(self.file_metadata.get_file_id())
    def get_content(self):
        return(self.file_content)

def hydrate_file (info):
    return(File(hydrate_file_metadata(info["metadata"]),
                info["content"]))

class SimulationParameter(object):

    def __init__(self,
                 simulation_plot_period,
                 simulation_pause_condition,
                 simulation_seed = None,
                 simulation_store_trace = False) :
        self.simulation_plot_period = simulation_plot_period
        self.simulation_pause_condition = simulation_pause_condition
        self.simulation_seed = simulation_seed
        self.simulation_store_trace = simulation_store_trace

    def toJSON(self):
        return({ "plot_period" : self.simulation_plot_period,
                 "pause_condition": self.simulation_pause_condition ,
                 "store_trace": self.simulation_store_trace ,
                 "seed" : self.simulation_seed })

class PlotLimit(object):

    def __init__(self,
                 plot_limit_offset = None,
                 plot_limit_points = None) :
        self.plot_limit_offset = plot_limit_offset
        self.plot_limit_points = plot_limit_points

    def toURL(self):

        if self.plot_limit_offset is not None :
            url_plot_limit_offset = "&plot_limit_offset={0}".format(self.plot_limit_offset)
        else :
            url_plot_limit_offset = ""

        if self.plot_limit_points is not None :
            url_plot_limit_points = "&plot_limit_points={0}".format(self.plot_limit_points)
        else :
            url_plot_limit_points = ""

        url_plot_limit = "{0}{1}".format(url_plot_limit_offset,
                                         url_plot_limit_points)
        return url_plot_limit

    def toJSON(self):
        return({ "offset" : self.plot_limit_offset ,
                 "nb_points" : self.plot_limit_points })

def PlotParameter(plot_limit_offset = None,
                  plot_limit_points = None) :
    return(PlotLimit(plot_limit_offset,plot_limit_points))

class KappaError(Exception):
    """ Error returned from the Kappa server
    """
    def __init__(self, errors):
        Exception.__init__(self)
        self.errors = errors
