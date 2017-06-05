""" Web api client for the kappa programming language
"""

import urllib.error
import urllib.request
import urllib.parse
import json
import kappa_common

class KappaRest(object):
    def __init__(self, endpoint, project_id):
        self.url = "{0}/v2".format(endpoint)
        self.project_id = project_id
        self.project_create(project_id)

    def __del__(self):
        self.project_delete()

    def dispatch(self, method, url, data):
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        if data:
            code = json.dumps(data)
            request = urllib.request.Request(url,
                                             data=code.encode("utf-8"))
        else:
            request = urllib.request.Request(url)
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.request.HTTPError as exception:
            message = exception.read()
            error = json.loads(message.decode("utf-8"))
            raise kappa_common.KappaError(error)
        except urllib.request.URLError as exception:
            raise kappa_common.KappaError(exception.reason)
        text = connection.read()
        details = json.loads(text.decode("utf-8"))
        if 400 <= connection.code < 500:
            raise kappa_common.KappaError(details)
        else:
            return details

    def shutdown(self, key):
        """ Shut down kappa instance.  Given a key to a
            kappa service shutdown a running kappa instance.
        """
        method = "POST"
        handler = urllib.request.HTTPHandler()
        opener = urllib.request.build_opener(handler)
        parse_url = "{0}/shutdown".format(self.url)
        request = urllib.request.Request(parse_url, data=key.encode('utf-8'))
        request.get_method = lambda: method
        try:
            connection = opener.open(request)
        except urllib.error.HTTPError as exception:
            connection = exception
        except urllib.error.URLError as exception:
            raise kappa_common.KappaError(exception.reason)
        if connection.code == 200:
            text = connection.read()
            return text
        elif connection.code == 400:
            text = connection.read()
            print(text)
            raise kappa_common.KappaError(text)
        elif connection.code == 401:
            text = connection.read()
            raise kappa_common.KappaError(text)
        else:
            raise exception

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

    def project_delete(self):
        method = "DELETE"
        url = "{0}/projects/{1}".format(self.url,self.project_id)
        body = None
        return(self.dispatch(method,url,body))

    def project_parse(self):
        method = "GET"
        url = "{0}/projects/{1}/parse".format(self.url,self.project_id)
        body = None
        return(self.dispatch(method,url,body))

    def file_create(self,file_object):
        method = "POST"
        url = "{0}/projects/{1}/files".format(self.url,self.project_id)
        body = file_object.toJSON()
        return(self.dispatch(method,url,body))

    def file_delete(self,file_id):
        method = "DELETE"
        url = "{0}/projects/{1}/files/{2}".format(self.url,self.project_id,file_id)
        body = None
        return(self.dispatch(method,url,body))

    def file_get(self,file_id):
        method = "GET"
        url = "{0}/projects/{1}/files/{2}".format(self.url,self.project_id,file_id)
        body = None
        file = self.dispatch(method,url,body)
        return(kappa_common.hydrate_file(file))

    def file_info(self):
        method = "GET"
        url = "{0}/projects/{1}/files".format(self.url,self.project_id)
        body = None
        info = self.dispatch(method,url,body)
        #return(list(map(hydrate_filemetada,info)))
        return(info)

    def simulation_delete(self):
        method = "DELETE"
        url = "{0}/projects/{1}/simulation".format(self.url,self.project_id)
        body = None
        return(self.dispatch(method,url,body))

    def simulation_detail_file_line(self,file_line_id):
        url = "{0}/projects/{1}/simulation/file_lines/{2}".format(self.url,self.project_id,file_line_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_flux_map(self,flux_map_id):
        url = "{0}/projects/{1}/simulation/fluxmaps/{2}".format(self.url,self.project_id,flux_map_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_log_message(self):
        url = "{0}/projects/{1}/simulation/logmessages".format(self.url,self.project_id)
        return(self.dispatch("GET",url,None))

    def simulation_detail_plot(self,plot_parameter = None):
        if plot_parameter :
            parameter = "?{0}".format(plot_parameter.toURL())
        else:
            parameter =  ""
        url = "{0}/projects/{1}/simulation/plot{2}".format(self.url,self.project_id,parameter)
        print(url)
        return(self.dispatch("GET",url,None))

    def simulation_detail_snapshot(self,snapshot_id):
        url = "{0}/projects/{1}/simulation/snapshots/{2}".format(self.url,self.project_id,snapshot_id)
        return(self.dispatch("GET",url,None))

    def simulation_info(self):
        url = "{0}/projects/{1}/simulation".format(self.url,self.project_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_file_line(self):
        url = "{0}/projects/{1}/simulation/file_lines".format(self.url,self.project_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_flux_map(self):
        url = "{0}/projects/{1}/simulation/fluxmaps".format(self.url,self.project_id)
        return(self.dispatch("GET",url,None))

    def simulation_info_snapshot(self):
        url = "{0}/projects/{1}/simulation/snapshots".format(self.url,
                                                             self.project_id)
        return(self.dispatch("GET",url,None))

    def simulation_delete(self):
        url = "{0}/projects/{1}/simulation".format(self.url,
                                                   self.project_id)
        return(self.dispatch("DELETE",url,None))

    def simulation_pause(self):
        message = { "action" : "pause" }
        url = "{0}/projects/{1}/simulation".format(self.url,
                                                   self.project_id)
        return(self.dispatch("PUT",url,message ))

    def simulation_perturbation(self,perturbation_code):
        url = "{0}/projects/{1}/simulation".format(self.url,
                                                   self.project_id)
        message = { "perturbation_code" : perturbation_code }
        return(self.dispatch("PUT",url,message ))

    def simulation_start(self,simulation_parameter):
        url = "{0}/projects/{1}/simulation".format(self.url,self.project_id)
        message = simulation_parameter.toJSON()
        return(self.dispatch("POST",url,message ))
