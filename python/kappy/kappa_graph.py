from collections import abc

class KappaSite:
    """class for representing one site of a kappa agent (in a complex)"""

    def __init__(self,*,links=None,internals=None,
                 future_link=None,future_internal=None):
        self._links = links
        self._internals = internals
        self._future_link = future_link
        self._future_internal = future_internal

    def __repr__(self):
        return "KappaSite({}{}{}{}{}{}{})".format(
            "" if self._links is None else f"links={self._links!r}",
            ", " if self._links is not None and
            (self._internals is not None or
             self._future_link is not None or
             self._future_internal is not None)
            else "",
            "" if self._internals is None
            else f"internals={self._internals!r}",
            ", " if self._internals is not None and
            (self._future_link is not None or self._future_internal is not None)
            else "",
            "" if self._future_link is None
            else f"future_link={self._future_link!r}",
            ", " if self._future_link is not None and
            self._future_internal is not None
            else "",
            "" if self._future_internal is None
            else f"future_internal={self._future_internal!r}"
        )

    @property
    def internal_states(self):
        return self._internals

    def get_internal_state(self):
        """:return: if any, the internal state of the site when there can only \
        be 1 by invarient (because it is a pattern/rule/snapshot/...). \
        None if there is not.

        """
        if self._internals is None:
            return None
        elif len(self._internals) is 0:
            return None
        else:
            assert len(self._internals) is 1
            return self._internals[0]

    def has_link(self):
        """Linking state is neither free nor unspecified"""
        return bool(self._links)

    def neighbours_in_complex(self,complx):
        """:return: the list of 'KappaAgent' connected to here in [complx]"""
        if type(self._links) is list:
            return [ complx[a] for (a,s) in self._links ]
        else:
            return []

    @staticmethod
    def __str_link_in_complex(line, row, site, trailing, dst):
        out = trailing.pop((((line,row),site),dst),None)
        if out is None:
            free = trailing[None]
            trailing[(dst,((line,row),site))] = free
            trailing[None] = free+1
            return str(free)
        else:
            return str(out)

    def __str_internals(self):
        if self._internals is None:
            if self._future_internal is None: out = ""
            else: out = "{#/"+str(self._future_internal)+"}"
            return out
        elif len(self._internals) == 0:
            assert(self._future_internal is None)
            return ""
        else:
            head = "{"+ ", ".join(self._internals)
            if self._future_internal is None: tail = "}"
            else: tail = "/"+str(self._future_internal)+"}"
            return head+tail

    def _str_in_complex(self, line, row, site, trailing):
        if self._future_link is None: mod = "]"
        elif self._future_link is False: mod = "/.]"
        else: mod = "/"+str(self.future_link)+"]"

        if self._links is None:
            if self._future_link is None: return self.__str_internals()
            else: return "[#"+mod+self.__str_internals()
        elif self._links is True: return "[_"+mod+self.__str_internals()
        elif type(self._links) is list:
            if len(self._links) == 0: return "[."+mod+self.__str_internals()
            else:
                links = ", ".join(
                    [ self.__str_link_in_complex(line, row, site, trailing, x)
                      for x in self._links ] )
                return "["+links+mod+self.__str_internals()
        else:
            site = self._links["site_name"]
            ag = self._links["agent_type"]
            return "["+site+"."+ag+links+mod+self.__str_internals()

    @staticmethod
    def __get_site_name(complx,x):
        if type(x[0]) is list: ag = complx[x[0][0]][x[0][1]]
        else: ag = complx[x[0]]
        return ag["node_sites"][x[1]]["site_name"]

    @classmethod
    def from_JSONDecoder_in_complex(cls,data,complx,*,in_1d):
        if data[0] != "port":
            raise Exception("Can only handle port sites for now")
        raw_links = data[1]["port_links"]
        if type(raw_links) is not list: links = raw_links
        else:
            if in_1d is None:
                if len(raw_links) > 0 and type(raw_links[0][0]) is list:
                    links = [ (tuple(x[0]),cls.__get_site_name(complx,x))
                              for x in raw_links ]
                else:
                    links = [ ((0,x[0]),cls.__get_site_name(complx,x))
                              for x in raw_links ]
            else:
                links = [ ((0,x[0]),cls.__get_site_name(complx,x)) if in_1d
                          else (tuple(x[0]),cls.__get_site_name(complx,x))
                          for x in raw_links ]
        return cls(links=links,internals=data[1]["port_states"])

class KappaAgent(abc.Sequence):
    """class for representing one kappa agent inside a complex

    [len] returns its number of sites.

    [iter] returns an iterator over the tuple '(site_name : str, s : KappaSite)'

    Use 'self[site_name]' to get a site.
"""

    def __init__(self, typ : str, sites : dict):
        self._type = typ
        self._sites = sites

    def __repr__(self):
        return "KappaAgent({},{})".format(
            repr(self._type),
            repr(self._sites)
        )

    def __len__(self):
        return len(self._sites)

    def __getitem__(self,key : str):
        return self._sites[key]

    def __iter__(self):
        return iter(self._sites.items())

    def get_type(self) -> str:
        """Get the type of the agent"""
        return self._type

    def get_neighbours_in_complex(self,complx):
        """list the 'KappaAgent's connected here in [complx]"""
        return [ el for (_,s) in self for el in s.neighbours_in_complex(complx) ]

    def _str_in_complex(self, line, row, trailing):
        sites = [ n + s._str_in_complex(line, row, n, trailing)
                  for (n , s) in self._sites.items() ]
        return self._type + "(" + " ".join(sites) + ")"

    @classmethod
    def from_JSONDecoder_in_complex(cls,data,complx,*,in_1d):
        if data is None: return None
        else:
            sites = dict([
                x["site_name"],
                KappaSite.from_JSONDecoder_in_complex(x["site_type"],complx,in_1d=in_1d)
            ] for x in data["node_sites"])
            return cls(data["node_type"],sites)

class KappaComplexIterator(abc.Iterator):

    def __init__(self, v, *, with_key):
        self._with_key = with_key
        self._line_iter = iter(v)
        self._line = 0
        self._row_iter = None

    def __next__(self):
        if self._row_iter is None:
            self._row = 0
            self._row_iter = iter(next(self._line_iter))
        try:
            o = next(self._row_iter)
            self._row += 1
            while o is None:
                o = next(self._row_iter)
                self._row += 1
            return ((self._line,self._row),o) if self._with_key else o
        except StopIteration:
            self._line += 1
            self._row_iter = None
            self.__next__()

class KappaComplex(abc.Sequence):
    """Class for representing a Kappa connected component

    The string representation is the corresponding Kappa code.

    [len] returns its size (number of agent).

    [iter] returns an iterator on the agents it contains. Use method
    [items()] to get an iterator over the tuples (coordinate,agent).

    Use 'self[coordinate]' to get the agent at 'coordinate'.

    """

    def __init__(self, agents):
        self._agents = agents

    def __repr__(self):
        return "KappaComplex({})".format(repr(self._agents))

    def __str__(self):
        trailing = { None: 0 }
        lines = [ [
            "." if e is None else e._str_in_complex(l,r,trailing)
            for (r,e) in enumerate(line)
        ] for (l,line) in enumerate(self._agents) ]
        return "\\ ".join(map(", ".join,lines))

    def __getitem__(self,key):
        if type(key) is tuple:
            (l,r) = key
            return self._agents[l][r]
        else: return self._agents[0][key]

    def __iter__(self):
        return KappaComplexIterator(self._agents,with_key=False)

    def __len__(self):
        r = 0
        for _ in self: r += 1
        return r

    def items(self):
        """An iterator with coordinates"""
        return KappaComplexIterator(self._agents,with_key=True)

    @classmethod
    def from_JSONDecoder(cls,data):
        if len(data) > 0 and type(data[0]) is dict:
            return cls([ [
                KappaAgent.from_JSONDecoder_in_complex(x,data,in_1d=True)
                for x in data
            ] ])
        else:
            return cls([ [
                KappaAgent.from_JSONDecoder_in_complex(x,data,in_1d=False)
                for x in line
            ] for line in data ])

class KappaSnapshot:
    """class for representing a kappa snapshot

    The string representation is the corresponding Kappa code.
    """

    def __init__(self, *, time : float, event : int,
                 complexes : list = [], tokens : dict = {}):
        self._time = time
        self._event = event
        self._complexes = complexes
        self._tokens = tokens

    def __repr__(self):
        return "KappaSnapshot(time={},event={},complexes={},tokens={})".format(
            repr(self._time),
            repr(self._event),
            repr(self._complexes),
            repr(self._tokens)
        )

    def __str__(self):
        event = "// Snapshot [Event: {0:d}]\n".format(self._event)
        time = '%def: "T0" "{0:f}"\n\n'.format(self._time)
        complexes = "".join(
            [ "%init: {0:d} {1}\n".format(n,c) for (n,c) in self._complexes ]
        )
        tokens = "".join(
            [ "%init: {0:d} {1}\n".format(n,t) for (t,n) in self._tokens.items() ]
        )
        return event+time+complexes+tokens

    @property
    def time(self) -> float:
        """Get the simulation time at which the snapshot was taken

        """
        return self._time

    @property
    def event(self) -> float:
        """Get after how many simulation event the snapshot was taken

        """
        return self._time

    @property
    def complexes(self):
        """Get the list of complexes. return a list of pairs '(abundance: int,
        complex : KappaComplex)'

        """
        return self._complexes

    def get_complexes_by_size(self):
        """Get complexes by size (largest to smallest). Size here means the
        number of agents.

        """
        return sorted(self._complexes, key=lambda c: len(c[1]), reverse=True)

    def get_complexes_by_abundance(self):
        """Get complexes by abundance (highest to lowest). Abundance here
        means copy number.

        """
        return sorted(self._complexes, key=lambda c: c[0], reverse=True)

    @property
    def tokens(self):
        """Get the dictionnary of 'token : str -> abundance : int'

        """
        return self._tokens

    def get_token_abundance(self,token : str):
        """Get the abuance of 'token'

        """
        return self._tokens[token]

    @classmethod
    def from_JSONDecoder(cls,data):
        complexes = [ (x[0], KappaComplex.from_JSONDecoder(x[1]))
                      for x in data["snapshot_agents"] ]
        tokens = dict( [ x[1], x[0] ] for x in data["snapshot_tokens"] )
        return cls(time=data["snapshot_time"], event=data["snapshot_event"],
                   complexes=complexes, tokens=tokens)
