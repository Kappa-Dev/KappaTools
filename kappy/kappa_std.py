"""Client for the kappa programming language through standard channel api"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['KappaStd']

import subprocess
import threading
import json

import os
from io import DEFAULT_BUFFER_SIZE

from .kappa_common import KappaError, PlotLimit, FileMetadata, File, \
                               KappaApi, KASIM_DIR, KAPPY_DIR

from .kappa_graph import KappaSnapshot

def find_agent_bin():
    agent_names = ['KappaSwitchman']
    bin_dir = None
    for potential_dir in [KAPPY_DIR, KASIM_DIR]:
        bin_dir = os.path.join(potential_dir, 'bin')
        if not os.path.exists(bin_dir):
            continue
        contents = os.listdir(bin_dir)
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
        if kappa_bin_path is None:
            if BIN_DIR is None:
                # binaries must either exist in kappy directory or
                # their location must be passed to this class
                raise KappaError("Kappa binaries not found.")
            kappa_bin_path = BIN_DIR
        switch_args = [os.path.join(kappa_bin_path, "KappaSwitchman"),
                    "--delimiter",
                    "\\x{:02x}".format(ord(self.delimiter)) ]
        if args:
            switch_args = switch_args + args
        self.lock = threading.Lock()
        self.message_id = 0
        self.switch_agent = subprocess.Popen(switch_args,
                                             stdin=subprocess.PIPE,
                                             stdout=subprocess.PIPE,
                                             stderr=subprocess.STDOUT)
        return

    def __del__(self):
        self.shutdown()

    def _get_message_id(self):
        self.message_id += 1
        return self.message_id

    def _read_stdout(self, agent_to_read):
        """Read from stdout of an agent. This function reads the output in
        large chunks (the default buffer size) until it encounters the
        delimiter. This is more efficient than reading the output one
        character at a time.

        """
        buff = bytearray()
        delim_val = self.delimiter.encode('utf-8')
        c = agent_to_read.stdout.read1(DEFAULT_BUFFER_SIZE)
        while (not c.endswith(delim_val)) and c:
            buff.extend(c)
            c = agent_to_read.stdout.read1(DEFAULT_BUFFER_SIZE)
        # strip the end character
        if c: buff.extend(c[0:-1])
        return buff

    def _dispatch(self, data):
        try:
            self.lock.acquire()
            message_id = self._get_message_id()
            message = [ message_id, data]
            message = "{0}{1}".format(json.dumps(message), self.delimiter)
            self.switch_agent.stdin.write(message.encode('utf-8'))
            self.switch_agent.stdin.flush()
            buff = self._read_stdout(self.switch_agent)
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

        Given a key to a kappa service shutdown a running kappa
        instance.

        """
        if hasattr(self, 'switch_agent'):
            self.switch_agent.stdin.close()
            self.switch_agent.stdout.close()
            self.switch_agent.terminate()
            self.switch_agent.wait()

    def projection(self, result_data):
        data = result_data[1]
        if result_data[0] == "Ok":
            return data
        else:
            raise KappaError(data)

    # Standardized API methods. Docs are provided by parent.

    def project_parse(self, sharing_level = "compatible_patterns", **kwargs):
        overwrites = list(kwargs.items())
        self._dispatch(["ProjectParse",sharing_level,overwrites])

    def project_overwrite(self, ast, file_id="model.ka"):
        self._dispatch(["ProjectOverwrite",file_id,ast])

    def file_create(self, file_):
        return self._dispatch(["FileCreate", file_.get_position(), file_.get_id(), file_.get_content()])

    def file_delete(self, file_id):
        return self._dispatch(["FileDelete", file_id])

    def file_get(self, file_id):
        f = self._dispatch(["FileGet", file_id])
        return File(FileMetadata(file_id,f[1]),f[0])

    def file_info(self):
        info = self._dispatch(["FileCatalog"])
        return FileMetadata.from_metadata_list(info)

    def simulation_delete(self):
        return self._dispatch(["SimulationDelete"])

    def simulation_file_line(self, file_line_id):
        return self._dispatch(["SimulationDetailFileLine", file_line_id])

    def simulation_DIN(self, DIN_id):
        return self._dispatch(["SimulationDetailDIN", DIN_id])

    def simulation_log_messages(self):
        return self._dispatch(["SimulationDetailLogMessage"])

    def simulation_plot(self, limit=None):
        if limit is not None:
            parameter = limit.toJSON()
        else:
            parameter = PlotLimit().toJSON()
        return self._dispatch(["SimulationDetailPlot", parameter])

    def simulation_snapshot(self, snapshot_id):
        return KappaSnapshot.from_JSONDecoder(
            self._dispatch(["SimulationDetailSnapshot", snapshot_id])
        )

    def simulation_info(self):
        return self._dispatch(["SimulationInfo"])

    def simulation_info_file_line(self):
        return self._dispatch(["SimulationCatalogFileLine"])

    def simulation_DINs(self):
        return self._dispatch(["SimulationCatalogDIN"])

    def simulation_snapshots(self):
        return self._dispatch(["SimulationCatalogSnapshot"])

    def simulation_pause(self):
        return self._dispatch(["SimulationPause"])

    def simulation_intervention(self, intervention_code):
        return self._dispatch(["SimulationIntervention", intervention_code])

    def simulation_start(self, simulation_parameter=None):
        if simulation_parameter is None:
            simulation_parameter = self.get_default_sim_param()
        return self._dispatch(["SimulationStart",
                               simulation_parameter.toJSON()])

    def simulation_continue(self, pause_condition):
        return self._dispatch(["SimulationContinue", pause_condition])

    def analyses_dead_rules(self):
        return self._dispatch(["DEAD_RULES"])

    def analyses_constraints_list(self):
        return self._dispatch(["CONSTRAINTS"])

    def analyses_contact_map(self, accuracy=None):
        cmd = ["CONTACT_MAP", accuracy]
        return self._dispatch(cmd)

    def analyses_influence_map(self, accuracy=None):
        cmd = ["INFLUENCE_MAP", accuracy]
        return self._dispatch(cmd)

    def analyses_potential_polymers(self, accuracy_cm="high", accuracy_scc="high"):
        return self._dispatch(["POLYMERS",accuracy_cm, accuracy_scc])
