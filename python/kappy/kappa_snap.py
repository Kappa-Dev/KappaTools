##
## Parse Kappa snapshots
##
import os
import sys
import json
import glob
import gzip
import shutil

from collections import defaultdict, OrderedDict
##
## Helper functions
##
def get_site_from_agent(agent, site_name):
    """
    Return site from 'node_sites' field.
    """
    site_of_interest = None
    node_sites = agent["node_sites"]
    for site_info in node_sites:
        if site_info["site_name"] == site_name:
            site_of_interest = site_info
            break
    return site_of_interest

def parse_site_type(site_type):
    """
    Parse site_type field, i.e., something like:

    ['port', {'port_links': [], 'port_states': ['m']}
    """
    if site_type[0] != "port":
        raise Exception("Only port sites supported for now.")
    return site_type[1]


def parse_node_sites(node_sites):
    """
    Parse node sites into a dictionary, indexed by site name.
   
    e.g., take:

    node_sites = [{"site_name": "x", "site_type": ["port", {"port_links": [[0, 1]], "port_states": []}]}, 
                  {"site_name": "y", "site_type": ["port", {"port_links": [[2, 0]], "port_states": []}]}

    into:

    {"x": {"port_links": [[0, 1]], "port_states": []},
     "y": {"port_links": [[2, 0]], "port_states": []}}
    """
    site_names = []
    sites = []
    for site_info in node_sites:
        # assume we only handle port things for now
        site_name = site_info["site_name"]
        site_type_info = site_info["site_type"]
        if site_type_info[0] != "port":
            raise Exception("Can only handle port sites for now.")
        # take the "port" portion
        site_type_info = site_type_info[1]
        site_names.append(site_name)
        sites.append(site_type_info)
    return (site_names, sites)

def agent_has_site(agent, site_name, site_state):
    """
    return True if agent has site named 'site_name' 
    with state 'site_state' (site state is something like 
    'u' or 'p', not a bond).
    """
    has_site = False
    for site_info in agent["node_sites"]:
        if site_info["site_name"] == site_name:
            # parse site type
            parsed_site_type = parse_site_type(site_info["site_type"])
            # question: why is port_states a list?
            if site_state in parsed_site_type["port_states"]:
                has_site = True
                break
    return has_site

class KappaAgent:
    """
    An `agent` is an object with 2 fields: a string "node_type" and an
    array of `site`s "node_sites". 
    "agent_sites"
    """
    def __init__(self, agent, label=None):
        self.type = agent["node_type"]
        self.label = label
        sites_info = parse_node_sites(agent["node_sites"])
        self.site_names = sites_info[0]
        self.sites = sites_info[1]

    def __str__(self):
        return "KappaAgent(type=%s, site_names=%s, sites=%s)" %(self.type,
                                                                self.site_names,
                                                                str(self.sites))

    def has_site(self, site_type, site_state_val=None):
        """
        True if agent has site (optionally with a given state value).
        """
        if not site_type in self.site_names:
            return False
        if site_state_val is not None:
            if type(site_state_val) != list:
                site_state_val = list(site_state_val)
            # if asked, check that it has the desired site state
            site_ind = self.site_names.index(site_type)
            port_states = self.sites[site_ind]["port_states"]
            return (site_state_val == port_states)
        return True

    def get_site_state(self, site_type):
        if site_type not in self.site_names:
            return None
        site_ind = self.site_names.index(site_type)
        site_state = self.sites[site_ind]["port_states"]
        return site_state

    def get_sink(self, site_type):
        """
        Return sink (using "port_links" field) of agent. The sink
        is represented as [i1, i2] where i1 is the index of the *agent*
        (in coordinates of complex) and i2 is the index of the *port* 
        (in that agent's coordinates, i.e., the coordinates of 'node_sites' field).
        """
        if site_type not in self.site_names:
            # No site type %s on agent %s" %(site_type, self)
            return None
        site_ind = self.site_names.index(site_type)
        port_links = self.sites[site_ind]["port_links"]
        sink_agent_ind = None
        sink_port_ind = None
        if len(port_links) > 0:
            sink_agent_ind, sink_port_ind = port_links[0]
        return (sink_agent_ind, sink_port_ind)

# class KappaSite:
#     """
#     a `site` is an object with 2 fields: a (string) "site_name" and a
#     `site_type` "site_type".
#     """
#     def __init__(self, site_name, site_type):
#         self.site_name = site_name
#         self.site_type = site_type

# class KappaSiteType:
#     """
#     the `site_type` can be
#       * a `"counter"` that we'll skip for now as they are still
#         experimental
#       * a `"port"` that is represented by the construction
#         `["port",{"port_links":[],"port_states":[]}]`    
#     """
#     def __init__(self, site_type_name, port_links, port_states):
#         if site_type_name != "port":
#             raise Exception("Only ports supported for now.")
#         self.site_type_name = site_type_name
#         self.port_links = port_links
#         self.port_states = port_states

def pretty_print(snapshot_json):
    KappaSnapshot(from_str=snapshot_json)
    agents = snapshot_json["snapshot_agents"]

class KappaComplex:
    def __init__(self, comp, abundance=None):
        self.comp = comp
        self.abundance = abundance
        self.size = len(self.comp)
        self.agents = []
        self.__make_agents()

    def __make_agents(self):
        """
        Store complex as list of agent objects.
        """
        n = 0
        for agent in self.comp:
            label = "%s_%d" %(agent["node_type"], n)
            agent_obj = KappaAgent(agent, label=label)
            self.agents.append(agent_obj)
            n += 1

    def __str__(self):
        agents_str = ",".join(map(str, self.agents))
        return agents_str

    def __repr__(self):
        return self.__str__()

    def print_agents(self):
        return ",".join(map(str, self.agents))

    def get_agent_types(self):
        """
        Return a dictionary mapping agent types to the number
        of times they occur in the complex.
        """
        agent_types = defaultdict(int)
        for agent in self.comp:
            agent_types[agent["node_type"]] += 1
        return agent_types

    def get_agents_by_type(self, agent_type, site_states={}):
        """
        Return all agent indices of type 'agent_type'. 

        Kwargs:
        - site_states: mapping from site name to its state.
        """
        agents = []
        for agent in self.agents:
            # if agent is not of required type, move on
            if agent.type != agent_type:
                continue
            matched_sites = True
            if site_states:
                # if we're asked, check that the internal states
                # conditions hold
                for site in site_states:
                    if site not in agent.site_names:
                        raise Exception("Expected site \'%s\' on agent" %(site))
                    agent_site = agent.sites[agent.site_names.index(site)]
                    agent_site_state = agent_site["port_states"]
                    # note: need to convert site states to list because JSON
                    # represents site internal states as lists even though
                    # in our case they can only take on single values
                    if agent_site_state != list(site_states[site]):
                        matched_sites = False
            if matched_sites:
                # the agent matched the site states conditions
                agents.append(agent)
        return agents

    def get_sinks(self, agent):
        """
        Get all agents connected to given agent.
        """
        sinks = []
        for site_name in agent.site_names:
            sink = self.get_connected_agent(agent, site_name)
            if sink is not None:
                sinks.append(sink)
        return sinks

    def get_connected_agent(self, agent, site_name):
        """
        Get all agents connected to agent_obj via the site 
        'site_name'. 
        """
        sink_agent_ind, sink_port_ind = agent.get_sink(site_name)
        connected_agent = None
        if sink_port_ind is not None:
            connected_agent = self.agents[sink_agent_ind]
        return connected_agent

    def is_connected_to(self, agent, site_name, agent_type):
        """
        Return True if the agent is connected to any agent 
        of type 'agent_type' via 'site_name'.
        """
        port_links = agent.get_sink(site_name)
        if port_links is None:
            return False
        if site_name not in agent.site_names:
            return False
        site_ind = agent.site_names.index(site_name)
        port_links = agent.sites[site_ind]["port_links"]
        if len(port_links) == 0:
            return False
        agent_ind, port_ind = port_links[0]
        connected_agent = self.agents[agent_ind]
        return (connected_agent.type == agent_type)

class KappaSnapshot:
    """
    Kappa Snapshot object, parsed from json.
    """
    def __init__(self, from_json=None, from_fname=None, discard_json=True):
        if (not from_json) and (not from_fname):
            raise Exception("Need either JSON or filename.")
        self.fname = from_fname
        self.discard_json = discard_json
        if from_json is None:
            self.snapshot_json = load_json_snap(self.fname)
        else:
            self.snapshot_json = from_json
        self.load_json()
        # make complexes
        self.complexes = []
        self.load_complexes()
        # optional label
        self.label = ""
        # order complexes by size
        self.complexes = sorted(self.complexes, key=lambda c: c[1].size)

    def get_complexes_by_size(self):
        """
        Get complexes by size (largest to smallest). Size here means
        the number of agents.
        """
        return sorted(self.complexes, key=lambda c: c[1].size, reverse=True)

    def get_complexes_by_abundance(self):
        """
        Get complexes by abundance (highest to lowest). Abundance here means copy number.
        """
        return sorted(self.complexes, key=lambda c: c[0], reverse=True)

    def get_complexes_by_order(self, order_func):
        """
        Get complexes by user-defined ordering, as determined
        by order_func.
        """
        return sorted(self.complexes, key=order_func)
            
    def load_json(self):
        self.snapshot_file = self.snapshot_json["snapshot_file"]
        self.snapshot_event = self.snapshot_json["snapshot_event"]
        self.snapshot_time = self.snapshot_json["snapshot_time"]
        self.snapshot_agents = self.snapshot_json["snapshot_agents"]
        self.snapshot_tokens = self.snapshot_json["snapshot_tokens"]

    def delete_raw_json(self):
        """
        Delete raw JSON to save space.
        """
        self.snapshot_agents = None
        self.snapshot_tokens = None
        self.snapshot_str = None

    def load_complexes(self, descending=True):
        """
        Get all complexes with their abundances, sorted from
        most to least abundant.
        """
        self.complexes = []
        for complex_info in self.snapshot_agents:
            complex_abundance = complex_info[0]
            curr_complex = KappaComplex(complex_info[1],
                                        abundance=complex_abundance)
            self.complexes.append((complex_abundance, curr_complex))
        # sort complexes by abundance
        self.complexes.sort(key=lambda k: k[0], reverse=descending)
        # after we've loaded complexes, delete raw JSON
        if self.discard_json:
            self.delete_raw_json()
        return self.complexes

    def print_complexes(self, verbose=False, print_bonds=False):
        print("%d complexes in total" %(len(self.complexes)))
        print("event %d" %(self.snapshot_event))
        for abundance, comp in self.complexes:
            agent_type_str = str(comp.get_agent_types())
            print("Complex(abundance=%d, size=%d, agent_types=%s)" \
                  %(abundance, comp.size, agent_type_str))
            if print_bonds:
                bonds_str = "-".join([agent.type for agent in comp.agents])
                print(bonds_str)
            if verbose:
                for agent in comp.agents:
                    print(" - agent: %s" %(str(agent)))

def count_chain_length(comp, agent_type):
    """
    Count length of chain of 'agent_type' (e.g., 'C').
    Crude heuristic: count the number of agents of the given type.
    """
    chain_length = 0
    for agent in comp:
        if agent["node_type"] == agent_type:
            chain_length += 1
    return chain_length

def get_chain_lens(complexes, agent_type):
    chain_lens = []
    for abundance, comp in complexes:
        chain_len = count_chain_length(comp, agent_type)
        chain_lens.append(chain_lens)
    return chain_lens

##
## utilities for working with snapshot directories
##
def compress_with_gzip(fname, delete_original=True):
    out_fname = None
    with open(fname, "rb") as file_in:
        out_fname = "%s.gz" %(fname)
        with gzip.open(out_fname, "wb") as file_out:
            shutil.copyfileobj(file_in, file_out)
    if delete_original:
        # delete original, uncompressed file if asked
        os.unlink(fname)
    return out_fname
        
def compress_snapshot_dir(dirname, snap_ext="json"):
    if not os.path.isdir(dirname):
        raise Exception("Cannot find directory %s" %(dirname))
    snap_fnames = glob.glob(os.path.join(dirname, "*.%s" %(snap_ext)))
    num_snaps = len(snap_fnames)
    print("Compressing directory %s" %(dirname))
    print("  - found %d files" %(num_snaps))
    for snap_fname in snap_fnames:
        compress_with_gzip(snap_fname)
    print("compression complete.")

def load_json_snap(snap_fname):
    """
    Load JSON snapshot file. Can be a plain JSON file (.json) or 
    a compressed file (.json.gz).
    """
    if not os.path.isfile(snap_fname):
        raise Exception("Cannot find snapshot file %s" %(snap_fname))
    json_str = None
    if snap_fname.endswith(".json.gz"):
        # if it's compressed, uncompress first
        with gzip.open(snap_fname, "rb") as file_in:
            json_str = file_in.read()
    else:
        # read uncompressed file
        with open(snap_fname, "r") as file_in:
            json_str = file_in.read()
    # return JSON object
    snap_json = None
    try:
        snap_json = json.loads(json_str)
    except:
        raise Exception("Is your snapshot in JSON format?")
    return snap_json

def print_complexes(snap_fname, print_bonds=False):
    if not os.path.isfile(snap_fname):
        raise Exception("Cannot find snapshot file %s" %(snap_fname))
    snap_obj = KappaSnapshot(from_fname=snap_fname)
    snap_obj.print_complexes(print_bonds=print_bonds)
            
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-pc", "--print-complexes", type=str,
                        help="Print complexes in snapshot (takes JSON filename).")
    parser.add_argument("--compress-dir", type=str,
                        help="Compress JSON snapshot files in given directory.")
    args = parser.parse_args()
    if args.print_complexes:
        print_complexes(args.print_complexes)
    if args.compress_dir:
        compress_snapshot_dir(args.compress_dir)
        
