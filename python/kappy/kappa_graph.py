class KappaSite:
    """class for representing one site of a kappa agent"""

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

    @classmethod
    def from_JSONDecoder(cls,data):
        if data[0] != "port":
            raise Exception("Can only handle port sites for now")
        raw_links = data[1]["port_links"]
        if type(raw_links) is not list: links = raw_links
        else: links = [ (tuple(x[0]),x[1]) for x in raw_links ]
        return cls(links=links,internals=data[1]["port_states"])

class KappaAgent:
    """class for representing one kappa agent inside a complex"""

    def __init__(self, typ : str, sites : dict):
        self._type = typ
        self._sites = sites

    def __repr__(self):
        return "KappaAgent({},{})".format(
            repr(self._type),
            repr(self._sites)
        )

    @classmethod
    def from_JSONDecoder(cls,data):
        if data is None: return None
        else:
            sites = dict([x["site_name"], KappaSite.from_JSONDecoder(x["site_type"])]
                         for x in data["node_sites"])
            return cls(data["node_type"],sites)

class KappaComplex:
    """class for representing a kappa connected component"""

    def __init__(self, agents : list):
        self._agents = agents

    def __repr__(self):
        return "KappaComplex({})".format(repr(self._agents))

    @classmethod
    def from_JSONDecoder(cls,data):
        return cls([ [ KappaAgent.from_JSONDecoder(x) for x in line ]
                     for line in data ])


class KappaSnapshot:
    """class for representing a kappa snapshot"""

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

    @classmethod
    def from_JSONDecoder(cls,data):
        complexes = [ (x[0], KappaComplex.from_JSONDecoder(x[1]))
                      for x in data["snapshot_agents"] ]
        tokens = dict( [ x[1], x[0] ] for x in data["snapshot_tokens"] )
        return cls(time=data["snapshot_time"], event=data["snapshot_event"],
                   complexes=complexes, tokens=tokens)
