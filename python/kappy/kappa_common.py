""" Shared functions of api client for the kappa programming language
"""
__all__ = ['FileMetadata', 'hydrate_file_metadata', 'string_as_file', 'File',
           'hydrate_file', 'SimulationParameter', 'get_plot_parameter',
           'PlotLimit', 'KappaError'] 

import json


class FileMetadata(object):
     # NOTE: Two of these input names should be changed: `id` to `file_id`, or
     # `compile` to `do_compile` to avoid conflicting with buildins.
    def __init__(self, id, position, compile=True, file_version=None):
        if file_version is None:
            file_version = []
        self.id = id
        self.position = position
        self.compile = compile
        self.file_version = file_version

    def toJSON(self):
        return { "id" : self.id ,
                 "compile" : self.compile ,
                 "position" : self.position ,
                 "version" : self.file_version }

    # NOTE: This isn't really needed, it's actually longer than just calling 
    # `<varname>.id`.
    def get_file_id(self):
        return self.id


# NOTE: This is not really necessary in python. The equivalent task could be
# accomplished by simply calling `FileMetadata(**info)` with the appropriate
# keys defined in FileMetadata. as shown.
def hydrate_file_metadata(info):
    """Generate a FileMetadata object using a dict `info`.

    The dict must contain the keys 'id', 'position', and 'compile'.
    """
    return FileMetadata(
        info["id"],
        info["position"],
        info["compile"] ,
        [] #hydrate_file_version(info["version"])
        )


def string_as_file(content,position=1):
    return File(FileMetadata("inlined_input",position),content)


class File(object):

    def __init__(self,
                 file_metadata,
                 file_content):
        self.file_metadata = file_metadata
        self.file_content = file_content

    def toJSON(self):
        return { "metadata" : self.file_metadata.toJSON() ,
                 "content" : self.file_content }

    def get_file_id(self):
        return self.file_metadata.get_file_id()

    def get_content(self):
        return self.file_content


def hydrate_file(info):
    """Generate a File object given a dict `info`.
    
    `info` is a dict with attributes 'metadata' and 'content', which will be
    processed and used to generate a File object, which is returned. The
    'metadata' attribute will be passed to `hydrate_file_metadata`, and must
    therefore have the appropriate items ('id', 'position', and 'compile').
    """
    return File(hydrate_file_metadata(info["metadata"]), info["content"])


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
        return { "plot_period" : self.simulation_plot_period,
                 "pause_condition": self.simulation_pause_condition ,
                 "store_trace": self.simulation_store_trace ,
                 "seed" : self.simulation_seed }

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
        return { "offset" : self.plot_limit_offset ,
                 "nb_points" : self.plot_limit_points }


def get_plot_parameter(plot_limit_offset=None, plot_limit_points=None) :
    return PlotLimit(plot_limit_offset, plot_limit_points)


class KappaError(Exception):
    """ Error returned from the Kappa server
    """
    def __init__(self, errors):
        Exception.__init__(self, errors)
        self.errors = errors
