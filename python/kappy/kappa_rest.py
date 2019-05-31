"""Web api client for the kappa programming language"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['KappaRest']

import json

from os import path
from requests import exceptions, request

from kappy.kappa_common import KappaError, PlotLimit, KappaApi, File, \
                               FileMetadata


@KappaApi._fix_docs
class KappaRest(KappaApi):
    """Client to a Kappa tools driver run as a server.

    Same interface as KappaStd (documented there)
    + a few extra methods to get general information about the server

    endpoint -- The url to the kappa server.
    project_id -- An identifier for this particular project.
    """
    def __init__(self, endpoint, project_id=None):
        self.url = "{0}/v2".format(endpoint)
        if project_id is None:
            project_id = self.make_unique_id('project')
        self.project_id = project_id
        self.project_ast = None
        self.analyses_to_init = True
        if project_id not in self.project_info():
            self._project_create()
        return

    def __del__(self):
        try:
            self._project_delete()
        except Exception:
            print("Faild to delete project, system probably shut down or "
                  "project was already deleted.")

    def _dispatch(self, method, sub_url=None, data=None):
        if sub_url is not None:
            url = path.join(self.url, sub_url)
        else:
            url = self.url
        if data is not None:
            data = json.dumps(data)
        try:
            r = request(method, url, data=data)
        except exceptions.HTTPError as e:
            msg = e.read()
            raise KappaError(json.loads(msg, encoding='utf-8'))
        details = r.json(encoding='utf-8')
        if 400 <= r.status_code < 500:
            raise KappaError(details)
        else:
            return details

    def _get(self, sub_url=None, data=None):
        """Thin wrapper around _dispatch function using method GET"""
        return self._dispatch('GET', sub_url, data)

    def _post(self, sub_url=None, data=None):
        """Thin wrapper around _dispatch function using method POST"""
        return self._dispatch("POST", sub_url, data)

    def _delete(self, sub_url=None, data=None):
        """Thin wrapper around _dispatch function using method DELETE"""
        return self._dispatch("DELETE", sub_url, data)

    def _put(self, sub_url=None, data=None):
        """Thin wrapper around _dispatch function using method PUT"""
        return self._dispatch("PUT", sub_url, data)

    def in_project(self, *elements):
        """Method to ease navigating the path structure within a project."""
        return path.join('projects', self.project_id, *elements)

    def shutdown(self, key):
        """Shut down kappa instance.

        Given a key to a kappa service shutdown a running kappa instance.
        """
        parse_url = "{0}/shutdown".format(self.url)
        try:
            r = request("POST", parse_url, data=key.encode('utf-8'))
        except exceptions.HTTPError as exception:
            raise KappaError(exception.reason)
        if r.status_code == 200:
            return r.text
        elif r.status_code == 400:
            print(r.text, r.reason)
            raise KappaError(r.text)
        elif r.status_code == 401:
            raise KappaError(r.text)
        else:
            raise KappaError(r.reason)

    def get_info(self):
        """Get a json dict with info about the kappa server."""
        return self._get()

    def _project_create(self):
        """Create this project with given."""
        return self._post('projects', {"project_id": self.project_id})

    def project_info(self):
        """Get json with info about all the projects."""
        return self._get('projects')

    def _project_delete(self):
        """Delete this project.

        Note that the project can still be recreated with `project_create`
        method. The effect of these two commands would be to clear the project.
        """
        return self._delete(self.in_project())

    def _analyses_init(self):
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        result = self._put(self.in_project('analyses'), self.project_ast)
        self.analyses_to_init = False
        return result

    # Standardized API methods. Docs are provided by parent.

    def project_parse(self, **kwargs):
        overwrites = '&'.join('%s=%s' % (key, value) for (key, value) in kwargs.items())
        reply = self._post(self.in_project('parse'))
        self.project_ast = reply
        self._post(self.in_project('?'.join(['load',overwrites])), reply)
        return reply

    def project_overwrite(self, ast, file_id="model.ka"):
        self._post(self.in_project('overwrite',file_id),ast)
        self.project_ast = ast
        self.analyses_to_init = True
        self._post(self.in_project('load'), ast)

    def file_create(self, file_):
        self.project_ast = None
        self.analyses_to_init = True
        return self._put(self.in_project('files', file_.get_id(), str(file_.get_position())), file_.get_content())

    def file_delete(self, file_id):
        self.project_ast = None
        self.analyses_to_init = True
        return self._delete(self.in_project('files', file_id))

    def file_get(self, file_id):
        f = self._get(self.in_project('files', file_id))
        return File(FileMetadata(file_id,f[1]),f[0])

    def file_info(self):
        info = self._get(self.in_project('files'))
        return FileMetadata.from_metadata_list(info)

    def simulation_file_line(self, file_line_id):
        sub_url = self.in_project('simulation', 'file_lines', file_line_id)
        return self._get(sub_url)

    def simulation_DIN(self, DIN_id):
        return self._get(self.in_project('simulation', 'DIN', DIN_id))

    def simulation_log_messages(self):
        return self._get(self.in_project('simulation', 'logmessages'))

    def simulation_plot(self, limit=None):
        if limit is not None:
            parameter = limit.toURL()
        else:
            parameter = PlotLimit().toURL()
        plot_query = "plot?%s" % parameter
        return self._get(self.in_project('simulation', plot_query))

    def simulation_info(self):
        return self._get(self.in_project('simulation'))

    def simulation_info_file_line(self):
        return self._get(self.in_project('simulation', 'file_lines'))

    def simulation_DINs(self):
        return self._get(self.in_project('simulation', 'DIN'))

    def simulation_snapshots(self):
        return self._get(self.in_project('simulation', 'snapshots'))

    def simulation_snapshot(self, snapshot_id):
        return self._get(self.in_project('simulation', 'snapshots',
                                         snapshot_id))

    def simulation_delete(self):
        return self._delete(self.in_project('simulation'))

    def simulation_pause(self):
        return self._put(self.in_project('simulation', 'pause'),
                         {'action': 'pause'})

    def simulation_intervention(self, intervention_code):
        return self._put(self.in_project('simulation', 'intervention'),
                         {'intervention_code': intervention_code})

    def simulation_start(self, simulation_parameter=None):
        if simulation_parameter is None:
            simulation_parameter = self.get_default_sim_param()
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        return self._post(self.in_project('simulation'),
                          simulation_parameter.toJSON())

    def simulation_continue(self, pause_condition):
        return self._put(self.in_project('simulation', 'continue'),
                         pause_condition)

    def analyses_dead_rules(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._get(self.in_project('analyses', 'dead_rules'))

    def analyses_constraints_list(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._get(self.in_project('analyses', 'constraints'))

    def analyses_contact_map(self, accuracy=None):
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = "contact_map"
        else:
            cmd = "contact_map?accuracy=%s" % accuracy
        return self._get(self.in_project('analyses', cmd))

    def analyses_influence_map(self, accuracy=None):
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = "influence_map"
        else:
            cmd = "influence_map?accuracy=%s" % accuracy
        return self._get(self.in_project('analyses', cmd))

    def analyses_potential_polymers(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._get(self.in_project('analyses', "potential_polymers"))
