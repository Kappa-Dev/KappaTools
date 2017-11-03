""" Shared functions of api client for the kappa programming language
"""
__all__ = ['FileMetadata', 'File', 'SimulationParameter', 'PlotLimit',
           'KappaError'] 

import json


class FileMetadata(object):
    """An object to hold the metadata for a file.

    Note that it is commmon to initialized this function with a dict, in the
    form FileMetaData(**metadeta). If so, the dict must have arguments
    matching those below, including at least 'id' and 'position'.

    Init
    ----
    id -- The id of corresponding file.
    position -- where you are to start in the file.
    compile -- (boolean, default True) Indicate whether the file should be
        compiled or not.
    file_version -- (list, default empty/None) the version of the file.

    Methods
    -------
    toJSON -- get a json (dict) representation of this data.
    """

    def __init__(self, id, position, compile=True, version=None):
        if version is None:
            version = []
        self.id = id
        self.position = position
        self.compile = compile
        self.version = version
        return

    @classmethod
    def from_metadata_list(cls, metadata_list):
        """Get metadata objects from a list of metadata."""
        return map(lambda info: cls(**info), metadata_list)

    def toJSON(self):
        """Get a json dict of the attributes of this object."""
        return { "id" : self.id ,
                 "compile" : self.compile ,
                 "position" : self.position ,
                 "version" : self.version }


class File(object):
    """An object that represents a kappa file.

    Init
    ----
    metadata -- this may be either a dict with keys matching the inits of
        FileMetadata, or an existing FileMetadata object.
    content -- The content of the file.

    Class Methods
    -------------
    from_string -- get a file object from a string.

    Methods
    -------
    toJSON -- get a JSON dict of the data in this file.
    get_file_id -- get the id of the file from the metadata.
    get_content -- get the content of the file.
    """

    def __init__(self, metadata, content):
        if isinstance(metadata, FileMetadata):
            self.file_metadata = metadata
        elif isinstance(metadata, dict):
            self.file_metadata = FileMetadata(**metadata)
        else:
            raise KappaError("Incorrect type for metadata. "
                             "Require dict or FileMetadata object, but got "
                             "%s." % type(metadata))
        self.file_content = content
        return

    @classmethod
    def from_string(cls, content, position=1):
        """Convenience method to create a file from a string.
        
        This file object's metadata will have the id 'inlined_input'.
        
        Inputs
        ------
        content -- the content of the file (a string).
        position -- the position to start reading the file (passed to
            FileMetadata).
        """
        return cls(FileMetadata('inlined_input', position), content)

    def toJSON(self):
        """Get a JSON dict of the data in this file."""
        return { "metadata" : self.file_metadata.toJSON() ,
                 "content" : self.file_content }

    def get_file_id(self):
        """Get the id of the file from the metadata."""
        return self.file_metadata.id

    def get_content(self):
        """Get the file's contents."""
        return self.file_content


class SimulationParameter(object):
    # TODO: Add docs.

    def __init__(self, plot_period, pause_condition, seed=None,
                 store_trace = False):
        self.plot_period = plot_period
        self.pause_condition = pause_condition
        self.seed = seed
        self.store_trace = store_trace

    def toJSON(self):
        return { "plot_period" : self.plot_period,
                 "pause_condition": self.pause_condition ,
                 "store_trace": self.store_trace ,
                 "seed" : self.seed }

class PlotLimit(object):
    # TODO: add docs.

    def __init__(self, offset=None, points=None) :
        self.offset = offset
        self.points = points

    def toURL(self):
        if self.offset is not None :
            url_offset = "&plot_limit_offset={0}".format(self.offset)
        else :
            url_offset = ""

        if self.points is not None :
            url_points = "&plot_limit_points={0}".format(self.points)
        else :
            url_points = ""

        url_plot_limit = "{0}{1}".format(url_offset,
                                         url_points)
        return url_plot_limit

    def toJSON(self):
        return { "offset" : self.offset ,
                 "nb_points" : self.points }


class KappaError(Exception):
    """ Error returned from the Kappa server"""
    def __init__(self, errors):
        Exception.__init__(self, errors)
        self.errors = errors
