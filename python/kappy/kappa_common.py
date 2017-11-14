""" Shared functions of api client for the kappa programming language"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['SimulationParameter', 'PlotLimit', 'KappaError']

import sys
import abc
from os import path


KASIM_DIR = path.normpath(
    path.join(path.dirname(path.abspath(__file__)), *([path.pardir]*2))
    )


class FileMetadata(object):
    """An object to hold the metadata for a file.

    Note that it is commmon to initialized this function with a dict, in the
    form FileMetaData(**metadata). If so, the dict must have arguments
    matching those below, including at least 'id' and 'position'.

    Init
    ----
    id -- The id of corresponding file.
    position -- where the file should be inserted in the middle of the
        other files of the model.
        When you add a file at position 'i' in a model that contains 'k >= i'
        files, the new file is indeed at position 'i' and all the files at
        position 'j>=i' are pushed at position 'j+1'.

    options that won't probably stay
    compile -- (boolean, default True) Indicate whether the file should be
        compiled or not next time project will be parsed. To be dropped as
        not compiled files could as well si;ply be deleted...
    file_version -- (list, default empty/None) the version of the file.
        Way too complicated logic meant to deal with the case where several
        clients works at the same time on the same stuff (in the REST API).
        Can be safely ignored in Std mode, Should not be trusted in Rest
        mode...in one word, to replace and erase!

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
        return {"id": self.id,
                "compile": self.compile,
                "position": self.position,
                "version": self.version}


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
    def from_string(cls, content, position=1, file_id=None):
        """
        Convenience method to create a file from a string.

        This file object's metadata will have the id 'inlined_input'.

        Inputs
        ------
        content -- the content of the file (a string).
        position -- (default 1) rank among all files of the model while parsing
            see FileMetadata
        file_id -- (default 'inlined_input') the file_id that will be used by
            kappa.
        """
        if file_id is None:
            file_id = 'inlined_input'
        return cls(FileMetadata(file_id, position), content)

    @classmethod
    def from_file(cls, fpath, position=1, file_id=None):
        """
        Convience method to create a kappa file object from a file on disk

        Inputs
        ------
        fpath -- path to the file on disk
        position -- (default 1) rank among all files of the model while parsing
            see FileMetadata
        file_id -- (default = fpath) the file_id that will be used by kappa.
        """
        if file_id is None:
            file_id = fpath
        with open(fpath) as f:
            code = f.read()
            file_content = str(code)
            file_metadata = FileMetadata(file_id, position)
            return cls(file_metadata, file_content)

    def toJSON(self):
        """Get a JSON dict of the data in this file."""
        return {"metadata": self.file_metadata.toJSON(),
                "content": self.file_content}

    def get_file_id(self):
        """Get the id of the file from the metadata."""
        return self.file_metadata.id

    def get_content(self):
        """Get the file's contents."""
        return self.file_content


class SimulationParameter(object):
    """Parameters needed to run a simulation

    Init
    ----
    plot_period -- (float) How often values of observables should be computed
        during the simulation
    pause_condition -- (string representing a boolean kappa expression)
        When the simulation will stop itself and wait for further actions.
    seed -- (int optionnal) specify the seed of the random number generator
        used by the simulator
    store_trace -- (boolean) Because simulation traces become huge, you must
       specify before starting a simulation whether you may query it later
    """

    def __init__(self, plot_period, pause_condition, seed=None,
                 store_trace=False):
        self.plot_period = plot_period
        self.pause_condition = pause_condition
        self.seed = seed
        self.store_trace = store_trace

    def toJSON(self):
        return {"plot_period": self.plot_period,
                "pause_condition": self.pause_condition,
                "store_trace": self.store_trace,
                "seed": self.seed}


class PlotLimit(object):
    """Parameters of plot query


    Init
    ----
    points -- maximum number of column that the reply should
       contains. (None means unlimited)
    offset -- At what column number the reply should start
       (None means return the end of the simulation)
    """

    def __init__(self, offset=None, points=None):
        self.offset = offset
        self.points = points

    def toURL(self):
        if self.offset is not None:
            url_offset = "&plot_limit_offset={0}".format(self.offset)
        else:
            url_offset = ""

        if self.points is not None:
            url_points = "&plot_limit_points={0}".format(self.points)
        else:
            url_points = ""

        url_plot_limit = "{0}{1}".format(url_offset,
                                         url_points)
        return url_plot_limit

    def toJSON(self):
        return {"offset": self.offset,
                "nb_points": self.points}


class KappaError(Exception):
    """ Error returned from the Kappa server"""
    def __init__(self, errors):
        Exception.__init__(self, errors)
        self.errors = errors


if sys.version_info >= (3, 4):
    ABC = abc.ABC
else:
    ABC = abc.ABCMeta('ABC'.encode(), (), {})


class KappaApi(ABC):
    """General api for a kappa interface."""
    def __init__(self):
        self.__default_param = None
        return

    def add_model_string(self, model_str, position=1, file_id=None):
        ret_data = self.file_create(File.from_string(model_str, position,
                                                     file_id))
        return ret_data

    def add_model_file(self, model_fpath, position=1, file_id=None):
        ret_data = self.file_create(File.from_file(model_fpath, position,
                                                   file_id))
        return ret_data

    def set_default_sim_param(self, *args, **kwargs):
        """Set the simulatin default simulation parameters.

        You can pass one of two things in as input:
        - a kappa_common.SimulationParameter instance
        - the arguments and keyword argument to create such an instance.

        The parameters you specify will be used by default in simulations run
        by this client.
        """
        if len(args) is 1 and isinstance(args[0], SimulationParameter):
            self.__default_param = args[0]
        else:
            self.__default_param = SimulationParameter(*args, **kwargs)
        return

    def get_default_sim_param(self):
        """Get the default SimulationParameter instance."""
        if self.__default_param is None:
            raise KappaError("Default simulation parameter not yet set.")
        return self.__default_param

    @abc.abstractmethod
    def project_parse(self, overwrites=None): pass

    @abc.abstractmethod
    def file_create(self, file_object): pass

    @abc.abstractmethod
    def file_delete(self, file_id): pass

    @abc.abstractmethod
    def file_get(self, file_id): pass

    @abc.abstractmethod
    def file_info(self): pass

    @abc.abstractmethod
    def simulation_delete(self): pass

    @abc.abstractmethod
    def simulation_file_line(self, file_line_id): pass

    @abc.abstractmethod
    def simulation_DIN(self, DIN_id): pass

    @abc.abstractmethod
    def simulation_log_messages(self): pass

    @abc.abstractmethod
    def simulation_plot(self, limit=None): pass

    @abc.abstractmethod
    def simulation_snapshot(self, snapshot_id): pass

    @abc.abstractmethod
    def simulation_info(self): pass

    @abc.abstractmethod
    def simulation_info_file_line(self): pass

    @abc.abstractmethod
    def simulation_DINs(self): pass

    @abc.abstractmethod
    def simulation_snapshots(self): pass

    @abc.abstractmethod
    def simulation_pause(self): pass

    @abc.abstractmethod
    def simulation_perturbation(self, perturbation_code): pass

    @abc.abstractmethod
    def simulation_start(self, simulation_parameter=None): pass

    @abc.abstractmethod
    def simulation_continue(self, pause_condition): pass

    @abc.abstractmethod
    def analyses_dead_rules(self): pass

    @abc.abstractmethod
    def analyses_constraints_list(self): pass

    @abc.abstractmethod
    def analyses_contact_map(self, accuracy=None): pass

    @abc.abstractmethod
    def analyses_influence_map(self, accuracy=None): pass
