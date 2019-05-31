"""Client for the kappa programming language through standard channel api"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['KappaStd']

import subprocess
import threading
import json

from os import path, listdir

from kappy.kappa_common import KappaError, PlotLimit, FileMetadata, File, \
                               KappaApi, KASIM_DIR, KAPPY_DIR


def find_agent_bin():
    agent_names = ['KaSimAgent', 'KaSaAgent']
    bin_dir = None
    for potential_dir in [KAPPY_DIR, KASIM_DIR]:
        bin_dir = path.join(potential_dir, 'bin')
        if not path.exists(bin_dir):
            continue
        contents = listdir(bin_dir)
        if all([agent in contents for agent in agent_names]):
            break
    return bin_dir


BIN_DIR = find_agent_bin()


@KappaApi._fix_docs
class KappaStd(KappaApi):
    """Kappa tools driver run locally.

    kappa_bin_path -- where to find kappa executables
        (None means use the binaries bundled in the package)
    delimiter -- What to use to delimit messages (must not appears in
        message body default '\\x1e')
    args -- arguments to pass to kappa executables
    """
    def __init__(self, kappa_bin_path=None, delimiter='\x1e', args=None):
        self.delimiter = delimiter
        self.project_ast = None
        self.analyses_to_init = True
        if kappa_bin_path is None:
            if BIN_DIR is None:
                # binaries must either exist in kappy directory or
                # their location must be passed to this class
                raise KappaError("Kappa binaries not found.")
            kappa_bin_path = BIN_DIR
        sim_args = [path.join(kappa_bin_path, "KaSimAgent"),
                    "--delimiter",
                    "\\x{:02x}".format(ord(self.delimiter)),
                    "--log",
                    "-", ]
        if args:
            sim_args = sim_args + args
        self.lock = threading.Lock()
        self.message_id = 0
        self.sim_agent = subprocess.Popen(sim_args,
                                          stdin=subprocess.PIPE,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT)
        sa_args = [path.join(kappa_bin_path, "KaSaAgent"), "--delimiter",
                   "\\x{:02x}".format(ord(self.delimiter)), ]
        if args:
            sa_args = sa_args + args
        self.sa_agent = subprocess.Popen(sa_args,
                                         stdin=subprocess.PIPE,
                                         stdout=subprocess.PIPE,
                                         stderr=subprocess.STDOUT)
        model_args = [path.join(kappa_bin_path, "KaMoHa"), "--delimiter",
                   "\\x{:02x}".format(ord(self.delimiter)), ]
        if args:
            model_args = model_args + args
        self.model_agent = subprocess.Popen(model_args,
                                         stdin=subprocess.PIPE,
                                         stdout=subprocess.PIPE,
                                         stderr=subprocess.STDOUT)
        return

    def __del__(self):
        self.shutdown()

    def _get_message_id(self):
        self.message_id += 1
        return self.message_id

    def _dispatch(self, method, args=None):
        if args is not None:
            data = [method, args]
        else:
            data = method

        try:
            self.lock.acquire()
            message_id = self._get_message_id()
            message = {'id': message_id, 'data': data}
            message = "{0}{1}".format(json.dumps(message), self.delimiter)
            self.sim_agent.stdin.write(message.encode('utf-8'))
            self.sim_agent.stdin.flush()
            buff = bytearray()
            c = self.sim_agent.stdout.read(1)
            while c != self.delimiter.encode('utf-8') and c:
                buff.extend(c)
                c = self.sim_agent.stdout.read(1)
            response = json.loads(buff.decode('utf-8'))
            if response["id"] != message_id:
                raise KappaError(
                        "expect id {0} got {1}".format(response["id"],
                                                       message_id)
                        )
            else:
                return self.projection(response["data"])[1]

        finally:
            self.lock.release()

    def _dispatch_sa(self, data):
        try:
            self.lock.acquire()
            message_id = self._get_message_id()
            message = {'id': message_id, 'data': data}
            message = "{0}{1}".format(json.dumps(message), self.delimiter)
            self.sa_agent.stdin.write(message.encode('utf-8'))
            self.sa_agent.stdin.flush()
            buff = bytearray()
            c = self.sa_agent.stdout.read(1)
            while c != self.delimiter.encode('utf-8') and c:
                buff.extend(c)
                c = self.sa_agent.stdout.read(1)
            response = json.loads(buff.decode('utf-8'))
            if response['code'] == "SUCCESS":
                return response['data']
            else:
                raise KappaError(response['data'])

        finally:
            self.lock.release()

    def _dispatch_model(self, data):
        try:
            self.lock.acquire()
            message_id = self._get_message_id()
            message = [ message_id, data]
            message = "{0}{1}".format(json.dumps(message), self.delimiter)
            self.model_agent.stdin.write(message.encode('utf-8'))
            self.model_agent.stdin.flush()
            buff = bytearray()
            c = self.model_agent.stdout.read(1)
            while c != self.delimiter.encode('utf-8') and c:
                buff.extend(c)
                c = self.model_agent.stdout.read(1)
            response = json.loads(buff.decode('utf-8'))
            if isinstance(response,str):
                raise KappaError(response)
            elif response[0] != message_id:
                raise KappaError(
                        "expect id {0} got {1}".format(response[0],
                                                       message_id)
                        )
            else:
                return self.projection(response[1])

        finally:
            self.lock.release()

    def shutdown(self):
        """Shut down kappa instance.

        Given a key to a kappa service shutdown a running kappa instance.
        """
        if hasattr(self, 'sim_agent'):
            self.sim_agent.stdin.close()
            self.sim_agent.stdout.close()
            self.sim_agent.terminate()
            self.sim_agent.wait()
        if hasattr(self, 'sa_agent'):
            self.sa_agent.stdin.close()
            self.sa_agent.stdout.close()
            self.sa_agent.terminate()
            self.sa_agent.wait()
        if hasattr(self, 'model_agent'):
            self.model_agent.stdin.close()
            self.model_agent.stdout.close()
            self.model_agent.terminate()
            self.model_agent.wait()

    def projection(self, result_data):
        data = result_data[1]
        if result_data[0] == "Ok":
            return data
        else:
            raise KappaError(data)

    def _analyses_init(self):
        """
        Initialize the static analyser thanks to the result of project_parse
        """
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        result = self._dispatch_sa(["INIT", self.project_ast])
        self.analyses_to_init = False
        return result

    # Standardized API methods. Docs are provided by parent.

    def project_parse(self, **kwargs):
        overwrites = list(kwargs.items())
        reply = self._dispatch_model(["ProjectParse"])
        self.project_ast = reply
        self._dispatch("ProjectLoad",[reply,overwrites])
        return reply

    def project_overwrite(self, ast, file_id="model.ka"):
        self._dispatch_model(["ProjectOverwrite",file_id,ast])
        self.project_ast = ast
        self.analyses_to_init = True
        self._dispatch("ProjectLoad",[ast,[]])

    def file_create(self, file_):
        self.project_ast = None
        self.analyses_to_init = True
        return self._dispatch_model(["FileCreate", file_.get_position(), file_.get_id(), file_.get_content()])

    def file_delete(self, file_id):
        self.project_ast = None
        self.analyses_to_init = True
        return self._dispatch_model(["FileDelete", file_id])

    def file_get(self, file_id):
        f = self._dispatch_model(["FileGet", file_id])
        return File(FileMetadata(file_id,f[1]),f[0])

    def file_info(self):
        info = self._dispatch_model(["FileCatalog"])
        return FileMetadata.from_metadata_list(info)

    def simulation_delete(self):
        return self._dispatch("SimulationDelete")

    def simulation_file_line(self, file_line_id):
        return self._dispatch("SimulationDetailFileLine", file_line_id)

    def simulation_DIN(self, DIN_id):
        return self._dispatch("SimulationDetailDIN", DIN_id)

    def simulation_log_messages(self):
        return self._dispatch("SimulationDetailLogMessage")

    def simulation_plot(self, limit=None):
        if limit is not None:
            parameter = limit.toJSON()
        else:
            parameter = PlotLimit().toJSON()
        return self._dispatch("SimulationDetailPlot", parameter)

    def simulation_snapshot(self, snapshot_id):
        return self._dispatch("SimulationDetailSnapshot", snapshot_id)

    def simulation_info(self):
        return self._dispatch("SimulationInfo")

    def simulation_info_file_line(self):
        return self._dispatch("SimulationCatalogFileLine")

    def simulation_DINs(self):
        return self._dispatch("SimulationCatalogDIN")

    def simulation_snapshots(self):
        return self._dispatch("SimulationCatalogSnapshot")

    def simulation_pause(self):
        return self._dispatch("SimulationPause")

    def simulation_intervention(self, intervention_code):
        return self._dispatch("SimulationIntervention",
                              {"intervention_code": intervention_code})

    def simulation_start(self, simulation_parameter=None):
        if simulation_parameter is None:
            simulation_parameter = self.get_default_sim_param()
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        return self._dispatch("SimulationStart",
                              simulation_parameter.toJSON())

    def simulation_continue(self, pause_condition):
        return self._dispatch("SimulationContinue", pause_condition)

    def analyses_dead_rules(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._dispatch_sa(["DEAD_RULES"])

    def analyses_constraints_list(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._dispatch_sa(["CONSTRAINTS"])

    def analyses_contact_map(self, accuracy=None):
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = ["CONTACT_MAP"]
        else:
            cmd = ["CONTACT_MAP", accuracy]
        return self._dispatch_sa(cmd)

    def analyses_influence_map(self, accuracy=None):
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = ["INFLUENCE_MAP"]
        else:
            cmd = ["INFLUENCE_MAP", accuracy]
        return self._dispatch_sa(cmd)

    def analyses_potential_polymers(self, accuracy_cm="high", accuracy_scc="high"):
        if self.analyses_to_init:
            self._analyses_init()
        return self._dispatch_sa(["POLYMERS",accuracy_cm, accuracy_scc])
