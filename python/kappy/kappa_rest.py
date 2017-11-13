"""Web api client for the kappa programming language"""
from __future__ import absolute_import, print_function, unicode_literals
from builtins import dict, str

__all__ = ['KappaRest']

import json

from os.path import join
from requests import exceptions, request

from kappy.kappa_common import KappaError, File, FileMetadata, PlotLimit, \
                               KappaApi


class KappaRest(KappaApi):
    """Client to a Kappa tools driver run as a server.

    Same interface as KappaStd (documented there)
    + a few extra methods to get general information about the server

    endpoint -- The url to the kappa server.
    project_id -- An identifier for this particular project.
    """
    def __init__(self, endpoint, project_id):
        self.url = "{0}/v2".format(endpoint)
        self.project_id = project_id
        self.project_ast = None
        self.analyses_to_init = True
        if not project_id in self.project_info():
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
            url = join(self.url, sub_url)
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
        return join('projects', self.project_id, *elements)

    def shutdown(self, key):
        """Shut down kappa instance.

        Given a key to a kappa service shutdown a running kappa instance.
        """
        parse_url = "{0}/shutdown".format(self.url)
        try:
            r = request("POST", parse_url, data=key.encode('utf-8'))
        except exceptions.URLError as exception:
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

    def project_parse(self, overwrites=None):
        if overwrites is None:
            overwrites = []
        reply = self._post(self.in_project('parse'), overwrites)
        self.project_ast = json.loads(reply['boxed_ast'])
        return reply

    def file_create(self, file_object):
        self.project_ast = None
        self.analyses_to_init = True
        return self._post(self.in_project('files'), file_object.toJSON())

    def file_delete(self, file_id):
        self.project_ast = None
        self.analyses_to_init = False
        return self._delete(self.in_project('files', file_id))

    def file_get(self, file_id):
        file_json = self._get(self.in_project('files', file_id))
        return File(**file_json)

    def file_info(self):
        info = self._get(self.in_project('files'))
        return FileMetadata.from_metadata_list(info)

    def simulation_delete(self):
        return self._delete(self.in_project('simulation'))

    def simulation_file_line(self, file_line_id):
        sub_url = self.in_project('simulation', 'file_lines', file_line_id)
        return self._get(sub_url)

    def simulation_DIN(self, flux_map_id):
        return self._get(self.in_project('simulation', 'fluxmaps', flux_map_id))

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
        return self._get(self.in_project('simulation', 'fluxmaps'))

    def simulation_snapshots(self):
        return self._get(self.in_project('simulation', 'snapshots'))

    def simulation_snapshot(self,snapshot_id):
        return self._get(self.in_project('simulation', 'snapshots',
                                            snapshot_id))

    def simulation_delete(self):
        return self._delete(self.in_project('simulation'))

    def simulation_pause(self):
        return self._put(self.in_project('simulation', 'pause'),
                         {'action': 'pause'})

    def simulation_perturbation(self, perturbation_code):
        return self._put(self.in_project('simulation', 'perturbation'),
                         {'perturbation_code': perturbation_code})

    def simulation_start(self, simulation_parameter):
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        return self._post(self.in_project('simulation'),
                          simulation_parameter.toJSON())

    def simulation_continue(self, pause_condition):
        return self._put(self.in_project('simulation', 'continue'),
                         pause_condition)

    def _analyses_init(self):
        if self.project_ast is None:
            raise KappaError("Project not parsed since last modification")
        result = self._put(self.in_project('analyses'), self.project_ast)
        self.analyses_to_init = False
        return result

    def analyses_dead_rules(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._get(self.in_project('analyses', 'dead_rules'))

    def analyses_constraints_list(self):
        if self.analyses_to_init:
            self._analyses_init()
        return self._get(self.in_project('analyses', 'constraints'))

    def analyses_contact_map(self, accuracy=None):
        """
        Returns the contact of the last parsed model

        Input
        -----
        accuracy -- \"high\" means take into account reachability from
           initial state. \"low\" means don't.
        """
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = "contact_map"
        else:
            cmd = "contact_map?accuracy=%s" % accuracy
        return self._get(self.in_project('analyses', cmd))

    def analyses_influence_map(self, accuracy=None):
        """
        Returns the influence_map of the last parsed model

        Input
        -----
        accuracy -- level can be \"low\", \"medium\", \"high\" or \"full\".
            Default is medium.
        """
        if self.analyses_to_init:
            self._analyses_init()
        if accuracy is None:
            cmd = "influence_map"
        else:
            cmd = "influence_map?accuracy=%s" % accuracy
        return self._get(self.in_project('analyses', cmd))
