""" Shared functions of api client for the kappa programming language"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['SimulationParameter', 'PlotLimit', 'KappaError']

import sys
import abc
import uuid
from os import path, environ
from time import sleep
from datetime import datetime


if sys.version_info >= (3, 4):
    ABC = abc.ABC
else:
    ABC = abc.ABCMeta('ABC'.encode(), (), {})


KAPPY_DIR = path.dirname(path.abspath(__file__))
KAPPY_DIR = environ.get('KAPPY_DIR', KAPPY_DIR)
KASIM_DIR = path.normpath(path.join(KAPPY_DIR, *([path.pardir]*2)))
KASIM_DIR = environ.get('KASIM_DIR', KASIM_DIR)


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

    Methods
    -------
    toJSON -- get a json (dict) representation of this data.
    """

    def __init__(self, id, position):
        self.id = id
        self.position = position
        return

    @classmethod
    def from_metadata_list(cls, metadata_list):
        """Get metadata objects from a list of metadata."""
        return map(lambda info: cls(**info), metadata_list)

    def toJSON(self):
        """Get a json dict of the attributes of this object."""
        return {"id": self.id,
                "position": self.position}


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
    get_id -- get the id of the file from the metadata.
    get_position -- get the position of the file from the metadata.
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

    def get_id(self):
        """Get the id of the file from the metadata."""
        return self.file_metadata.id

    def get_position(self):
        """Get the id of the file from the metadata."""
        return self.file_metadata.position

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


class KappaApi(ABC):
    """General api for a kappa interface."""
    def __init__(self):
        self.__default_param = None
        return

    @classmethod
    def _fix_docs(this_abc, child_class):
        """Make api method docs inheritted.

        Specifically, insepect.getdoc will return values inheritted from this
        abc for standardized api methods.
        """
        # After python 3.5, this is basically handled automatically
        if sys.version_info >= (3, 5):
            return child_class

        if not issubclass(child_class, this_abc):
            raise KappaError('Cannot fix docs of class that is not decendent.')

        # This method is modified from solution given in
        # https://stackoverflow.com/a/8101598/8863865
        for name, child_func in vars(child_class).items():
            if callable(child_func) and not child_func.__doc__:
                if name in this_abc.__abstractmethods__:
                    parent_func = getattr(this_abc, name)
                    child_func.__doc__ = parent_func.__doc__
        return child_class

    @classmethod
    def make_unique_id(cls, name):
        return "%s-%s" % (name, uuid.uuid1())

    def add_model_string(self, model_str, position=1, file_id=None):
        """Add a kappa model given in a string to the project."""
        if file_id is None:
            file_id = self.make_unique_id('inlined_input')
        ret_data = self.file_create(File.from_string(model_str, position,
                                                     file_id))
        return ret_data

    def add_model_file(self, model_fpath, position=1, file_id=None):
        """Add a kappa model from a file at given path to the project."""
        if file_id is None:
            file_id = self.make_unique_id('file_input')
        ret_data = self.file_create(File.from_file(model_fpath, position,
                                                   file_id))
        return ret_data

    def set_default_sim_param(self, *args, **kwargs):
        """Set the simulation default simulation parameters.

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

    def get_is_sim_running(self):
        """Check if the current simulation is running."""
        sim_info = self.simulation_info()
        try:
            progress_info = sim_info['simulation_info_progress']
            ret = progress_info['simulation_progress_is_running']
        except KeyError:  # Simulation has not been created.
            ret = False
        return ret

    def wait_for_simulation_stop(self, timeout=None):
        """Block until the simulation is done or timeout seconds exceeded.

        If the simulation stops before timeout, siminfo is returned.
        """
        start = datetime.now()
        while self.get_is_sim_running():
            sleep(0.5)
            if timeout is not None:
                if (datetime.now() - start).seconds >= timeout:
                    ret = None
                    break
        else:
            ret = self.simulation_info()
        return ret

    # Abstract methods to standardize the API. Docs given here are applied to
    # the corresponding methods in children by default when @KappaApi._fixdocs
    # is used as a decorator for the class.

    @abc.abstractmethod
    def project_overwrite(self, ast, file_id="model.ka"):
        """
        Overwrite the project with the given AST

        ast -- the ast in the format returned by project_parse
        file_id -- a virtual file name in which the ast will be dump
        """

    @abc.abstractmethod
    def project_parse(self, **kwargs):
        """
        Parses the project

        kwargs -- list of algebraic variables to overwrite
        Each element has the form variable_name=numerical_val
        """

    @abc.abstractmethod
    def file_create(self, file_object):
        """
        Add a file to the project

        file_object -- a Kappa_common.File
        """

    @abc.abstractmethod
    def file_delete(self, file_id):
        """
        Remove a file from the project
        """

    @abc.abstractmethod
    def file_get(self, file_id):
        """
        Returns file file_id stored in the project
        """

    @abc.abstractmethod
    def file_info(self):
        """
        Lists the files of the project (returns a FileMetadata array)
        """

    @abc.abstractmethod
    def simulation_delete(self):
        """
        Deletes running/paused simulation
        """

    @abc.abstractmethod
    def simulation_file_line(self, file_line_id):
        """
        Returns the file file_line_id generated by $PRINT interventions
        """

    @abc.abstractmethod
    def simulation_DIN(self, DIN_id):
        """
        Returns a given generated DIN
        """

    @abc.abstractmethod
    def simulation_log_messages(self):
        """
        Returns simulation log
        """

    @abc.abstractmethod
    def simulation_plot(self, limit=None):
        """
        Returns the plot data of the simulation

        Note: No actual plot is produced as a result of this function call.

        Inputs
        ------
        limit -- optionnal boundaries to only get a subplot
        format: { offset : 100, nb_points : 500 }
        returns the last points if offset is Null

        Returns
        -------
        simulation_results -- a json containing the data from the simulation.
        """

    @abc.abstractmethod
    def simulation_snapshot(self, snapshot_id):
        """
        Returns a given generated snapshot
        """

    @abc.abstractmethod
    def simulation_info(self):
        """
        Returns state and progress of the simulation
        """

    @abc.abstractmethod
    def simulation_info_file_line(self):
        """
        Lists files generated by $PRINT during the simulation
        """

    @abc.abstractmethod
    def simulation_DINs(self):
        """
        Lists DIN generated during the simulation
        """

    @abc.abstractmethod
    def simulation_snapshots(self):
        """
        Lists snapshots generated during the simulation
        """

    @abc.abstractmethod
    def simulation_pause(self):
        """
        Pauses a simulation
        """

    @abc.abstractmethod
    def simulation_intervention(self, intervention_code):
        """
        Fires a intervention in a paused simulation
        """

    @abc.abstractmethod
    def simulation_start(self, simulation_parameter=None):
        """Start the simulation from the last parsed model.

        Inputs
        ------
        simulation_parameter -- (optional) a kappa_common.SimulationParameter
            instance. The default is set using the `set_default_sim_param`
            method.
        """

    @abc.abstractmethod
    def simulation_continue(self, pause_condition):
        """
        Restarts a paused simulation
        """

    @abc.abstractmethod
    def analyses_dead_rules(self):
        """
        Returns the dead rules of the last parsed model
        """

    @abc.abstractmethod
    def analyses_constraints_list(self):
        """
        Returns a bunch of invarients on the last parsed model
        """

    @abc.abstractmethod
    def analyses_contact_map(self, accuracy=None):
        """
        Returns the contact of the last parsed model

        Input
        -----
        accuracy -- \"high\" means take into account reachability from
           initial state. \"low\" means don't.
        """

    @abc.abstractmethod
    def analyses_influence_map(self, accuracy=None):
        """
        Returns the influence_map of the last parsed model

        Input
        -----
        accuracy -- level can be \"low\", \"medium\", \"high\" or \"full\".
            Default is medium.
        """

    @abc.abstractmethod
    def analyses_potential_polymers(self):
        """
        Returns the list of potential polymers of the last parsed model
        """
