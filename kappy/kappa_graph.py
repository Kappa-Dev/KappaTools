from collections import abc
import re

ident_re =  r'[_~][a-zA-Z0-9_~+-]+|[a-zA-Z][a-zA-Z0-9_~+-]*'
line_comment_re = r'//[^\n]*\n'
non_nested_block_comment_re = r'/\*(?:[^*]*|\*+[^/])*\*/'
whitespace_re = r'(?:' + line_comment_re + '|' + non_nested_block_comment_re + '|\s)+'

def smallest_non_empty(dic):
    out = None
    for id in dic:
        va = dic[id]
        l = len(va)
        if l > 0:
            if out is None:
                out = (id,va)
            else:
                if len(out[1]) > l:
                    out = (id,va)
    return out

class KappaSyntaxError(ValueError):
    pass

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

    def is_more_specific_than(self,ref,*,mapping=None,todos=None):
        if ref._internals is None or \
           all(x in self._internals for x in ref._internals):
            if ref._links is None:
                return True
            elif ref._links is True:
                return not (self._links is None or self._links == [])
            elif type(ref._links) is list :
                if type(self._links) is list:
                    if len(ref._links) is 0:
                        return len(self._links) is 0
                    else:
                        assert len(ref._links) is 1, \
                            "Sigma graph compare not implemented"
                        if len(self._links) is 1:
                            assert mapping is not None, "Missing mapping"
                            (r_ag,r_si) = ref._links[0]
                            (ag,si) = self._links[0]
                            ag_dst=mapping.get(r_ag)
                            if not ag_dst:
                                ag_dst=ag
                                mapping[r_ag] = ag
                                todos.append((r_ag,ag))
                            return si == r_si and ag == ag_dst
                        else: return False
                else: return False
            else:
                site = ref._links["site_name"]
                ag = ref._links["agent_type"]
                assert False,"Sorry, I can't deal with site_types."
        else: return False

    def is_equal(self,ref,*,mapping=None,todos=None):
        if (isinstance(ref._internals, type(self._internals))
            and ref._internals == self._internals):
            if type(ref._links) is list :
                if type(self._links) is list:
                    if len(ref._links) is 0:
                        return len(self._links) is 0
                    else:
                        assert len(ref._links) is 1, \
                            "Sigma graph compare not implemented"
                        if len(self._links) is 1:
                            assert mapping is not None, "Missing mapping"
                            (r_ag,r_si) = ref._links[0]
                            (ag,si) = self._links[0]
                            ag_dst=mapping.get(r_ag)
                            if not ag_dst:
                                ag_dst=ag
                                mapping[r_ag] = ag
                                todos.append((r_ag,ag))
                            return si == r_si and ag == ag_dst
                        else: return False
                else: return False
            else:
                return (isinstance(ref._links,type(self._links))
                        and ref._links == self._links)
        else: return False

    @property
    def internal_states(self):
        return self._internals

    def get_internal_state(self) -> str:
        """:returns: if any, the internal state of the site when there can only \
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

    def has_link(self) -> bool:
        """:returns: whether the link state is neither free nor unspecified?"""
        return bool(self._links)

    def neighbours_in_complex(self,complx):
        """:returns: the list of ``KappaAgent`` connected to here in ``complx``

        """
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

    @classmethod
    def from_string_in_complex(cls, expression: str,
                               position, dangling, completed):
        # define patterns that make up a site
        int_state_pat = \
            r'(?: ?{ ?(' +ident_re+ r'|#) ?(?: ?(/ ?)(' +ident_re+ r') ?)?} ?)?'
        bnd_state_pat = r' ?\[ ?(.|_|#|\d+) ?(?:(/ ?)(.|\d+) ?)?\] ?'
        port_pat = r'^' + int_state_pat + bnd_state_pat + int_state_pat + r'$'
        # parse assuming full site declaration, with bond state declared
        g = re.match(port_pat, expression)
        # if that fails, try parsing with bond state declared as a wildcard
        if not g:
            g = re.match(port_pat, expression + '[#]')
        # if that fails, throw an error
        if not g:
            raise KappaSyntaxError('Invalid site declaration <'+expression+'>')
        # figure out what type of bond operation is being performed
        if g.group(4) == '.':
            links = []
        elif g.group(4) == '#':
            links = None
        elif g.group(4) == '_':
            links = True
        else:
            link = int(g.group(4))
            dst = dangling.pop(link,None)
            if dst is None:
                links = link
                dangling[link] = position
            else:
                links = [ dst ]
                completed[dst] = position
        #if g.group(5): # if there's an operation
        #    self._future_link = g.group(6)
        # figure out what type of internal state operation is being performed
        internals = None
        if g.group(1) and not g.group(1) == '#':
            internals = [ g.group(1) ]
        #self._future_internal = g.group(3) if g.group(3) else ''
        elif g.group(7) and not g.group(7) == '#':
            internals = [ g.group(7) ]
        #self._future_int_state = g.group(9) if g.group(9) else ''
        return cls(links=links,internals=internals)

class KappaAgent(abc.Sequence):
    """class for representing one kappa agent inside a complex

    ``abc.Sized``: ``len`` returns its number of sites.

    ``abc.Iterable``: ``iter`` returns an iterator over the tuple
    ``(site_name : str, s : KappaSite)``

    Use ``self[site_name]`` to get a site.

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

    def is_more_specific_than(self,ref,*,mapping=None,todos=None):
        if ref._type==self._type:
            return all (self._sites.get(na,KappaSite()).\
                        is_more_specific_than(ref._sites[na],
                                              mapping=mapping,
                                              todos=todos)
                        for na in ref._sites)
        else: return False

    def is_equal(self,ref,*,mapping=None,todos=None):
        if ref._type==self._type:
            return all (self._sites.get(na,KappaSite()).\
                        is_equal(ref._sites[na], mapping=mapping, todos=todos)
                        for na in ref._sites)
        else: return False

    def get_type(self) -> str:
        """::returns: the type of the agent"""
        return self._type

    def get_neighbours_in_complex(self,complx):
        """Destination of the edges.

        :param KappaComplex complx: Complex ``self`` is part of

        :returns: list of ``KappaAgent``

        .. warning::
          potential duplicates and self listing.

        """
        return [ el for (_,s) in self
                 for el in s.neighbours_in_complex(complx) ]

    def _str_in_complex(self, line, row, trailing):
        sites = [ n + self._sites[n]._str_in_complex(line, row, n, trailing)
                  for n in self._sites ]
        return self._type + "(" + " ".join(sites) + ")"

    @classmethod
    def from_JSONDecoder_in_complex(cls,data,complx,*,in_1d):
        if data is None: return None
        else:
            sites = dict([
                x["site_name"],
                KappaSite.from_JSONDecoder_in_complex(x["site_type"],
                                                      complx, in_1d=in_1d)
            ] for x in data["node_sites"])
            return cls(data["node_type"],sites)

    @classmethod
    def from_string_in_complex(cls, expression: str,
                               position, dangling, completed):
        if re.match(r'^\.$', expression.strip()):
            return None
        else:
            # Check if kappa expression's name & overall structure is valid
            agent_name_pat = ident_re
            agent_sign_pat = r'\(([^()]*)\)'
            agent_oper_pat = r'(\+|-)?'
            agent_pat = \
                r'^(' +agent_name_pat+ r')' +agent_sign_pat+agent_oper_pat+ r'$'
            matches = re.match(agent_pat, expression.strip())
            if not matches:
                matches = re.match(agent_pat, expression.strip() + '()')
                if not matches:
                    raise KappaSyntaxError('Invalid agent declaration <' +
                                           expression + '>')

            # process & assign to variables
            agent_name = matches.group(1)
            # process agent signature
            ag_signature = matches.group(2)
            if ag_signature == '':
                site_list = []
            else:
                site_block_re = r'('+ident_re+')((?:\[[^\]]+\]|{[^}]+})+) ?,? ?'
                site_list = re.finditer(site_block_re,ag_signature)
            agent_signature = {}
            for id, match in enumerate(site_list):
                name=match.group(1)
                site = KappaSite.from_string_in_complex(match.group(2),
                                                        (position,name),
                                                        dangling,
                                                        completed)
                agent_signature[name]=site
            # process abundance operator, if present
            #abundance_change = matches.group(3) if matches.group(3) else ''
            return cls(agent_name,agent_signature)


class KappaComplexIterator(abc.Iterator):

    def __init__(self, v, *, with_key):
        self._with_key = with_key
        self._line_iter = iter(v)
        self._line = 0
        self._row_iter = None

    def __next__(self):
        if self._row_iter is None:
            self._row = -1
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

allocated_once_for_all_empty_list=[]

class KappaComplex(abc.Sequence):
    """Class for representing a Kappa connected component

    The string representation is the corresponding Kappa code.

    ``abc.Sized``: ``len`` returns the number of agents in the complex
    (the size).

    ``abc.Iterable``: ``iter`` returns an iterator on the agents it
    contains. Use method ``items()`` to get an iterator over the
    tuples ``(coordinates,agent)``.

    ``abc.Container``: ``x in y`` returns whether the pattern ``x``
    occurs in ``y`` (in term of "Kappa embedding"). Use
    ``y.find_pattern(x)`` to get the embeddings.

    Use ``self[coordinates]`` to get the agent at ``coordinates``.

    """

    def __init__(self, agents):
        self._agents = agents
        self._nodes_by_type = {}
        i=0
        for line in agents:
            j=0
            for el in line:
                if el:
                    self._nodes_by_type.setdefault(el.get_type(),[]).append((i,j))
                j += 1
            i += 1

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
        for line in self._agents:
            for el in line:
                if el: r +=1
        return r

    def items(self):
        """
        :returns: an Iterator where the items are turples (coordinates,agent)

        """
        return KappaComplexIterator(self._agents,with_key=True)

    def agent_ids_of_type(self,type : str):
        """:returns: the list of coordinates of agents of type ``type``"""
        return self._nodes_by_type[type]

    def agent_ids_by_type(self):
        """:returns: a dictionary from ``type : str`` to coordinates of agents \
        of type ``type``

        """
        return self._nodes_by_type

    def contains_at_pos(self,id,ref,ref_id):
        mapping={ ref_id : id }
        todos=[(ref_id,id)]
        while len(todos) > 0:
            (rag,ag)=todos.pop()
            if not self[ag].is_more_specific_than(ref[rag],
                                                  mapping=mapping,
                                                  todos=todos):
                return None
        return mapping

    def __eq_rooted(self,id,ref,ref_id):
        mapping={ ref_id : id }
        todos=[(ref_id,id)]
        while len(todos) > 0:
            (rag,ag)=todos.pop()
            if not self[ag].is_equal(ref[rag], mapping=mapping, todos=todos):
                return None
        return mapping

    def find_pattern(self,ref):
        """:returns: a list of dictionnary \
        ``coordinates -> coordinates``. Each dictionnary represents an \
        embedding of ``ref`` in the complex.

        """
        ag_ty,ref_ids = smallest_non_empty(ref._nodes_by_type)
        candidates = self._nodes_by_type.get(ag_ty,allocated_once_for_all_empty_list)
        return [ x for x in [ self.contains_at_pos(cand,ref,ref_ids[0])
                              for cand in candidates ]
                 if x ]

    def __contains__(self,pattern):
        return not len(self.find_pattern(pattern)) == 0

    def __same_sum_formula(a,b):
        if len(a._nodes_by_type) != len(b._nodes_by_type): return False
        for ty in a._nodes_by_type:
            if (len(b._nodes_by_type.get(ty,allocated_once_for_all_empty_list))
                != len(a._nodes_by_type[ty])):
                return False
        return True

    def __eq__(a,b):
        if a.__same_sum_formula(b):
            ag_ty,ref_ids = smallest_non_empty(a._nodes_by_type)
            ref_id=ref_ids[0]
            for cand in b._nodes_by_type[ag_ty]:
                if b.__eq_rooted(cand,a,ref_id): return True
        return False

    @classmethod
    def from_JSONDecoder(cls,data):
        """ Build a KappaComplex from its JSON representation"""
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

    @classmethod
    def from_string(cls,expression):
        """Build a KappaComplex from its Kappa syntax"""
        # remove comments, new_lines, multiple spaces
        expression = re.sub(whitespace_re, ' ', expression)
        # get the set of agents making up this complex
        agent_name_pat = r' ?' + ident_re
        agent_sign_pat = r' ?\([^()]*\)'
        matches = re.findall(agent_name_pat+agent_sign_pat, expression.strip())
        if len(matches) == 0:
            raise KappaSyntaxError('Complex <' + self._raw_expression +
                                   '> appears to have zero agents.')
        agent_list = []
        dangling = {}
        completed = {}
        for id, item in enumerate(matches):
            agent = KappaAgent.from_string_in_complex(item,(0,id),
                                                      dangling,
                                                      completed)
            agent_list.append(agent)
        if len(dangling) is not 0:
            raise KappaSyntaxError('Dangling link <' + str(dangling.popitem()) +
                                   '> in complex <' + expression + '>.')
        for (((col,row),si),dst) in completed.items():
            site = agent_list[row][si]
            agent_list[row]._sites[si] = KappaSite(links=[dst],
                                                   internals=site.internal_states)
        return cls([ agent_list ])

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
            [ "%init: {0:d} {1}\n".format(self._tokens[t],t) for t in self._tokens ]
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
        """Get the list of complexes.

        :returns: a list of pairs ``(abundance: int, complex : KappaComplex)``

        """
        return self._complexes

    def get_complexes_by_size(self):
        """Get complexes by size. Size here means the number of agents.

        :returns: dict ``size -> list_of_pair_abundance_complexes_of_that_size``

        """
        out = {}
        for (abundance,complx) in self._complexes:
            size = len(complx)
            out.setdefault(size,[]).append((abundance,complx))
        return out

    def get_complexes_by_abundance(self):
        """Get complexes by abundance. Abundance here means copy number.

        :returns: dict ``abundance -> list_of_complexes_that_abundant``

        """
        out = {}
        for (abundance,complx) in self._complexes:
            out.setdefault(abundance,[]).append(complx)
        return out

    def get_largest_complexes(self):
        """:returns: list of the largest KappaComplexes with their abundance.

        """
        return (max(self.get_complexes_by_size().items(),
                    key=lambda c: c[0]))

    def get_smallest_complexes(self):
        """:returns: list of the smallest KappaComplexes with their adundance.

        """
        return (min(self.get_complexes_by_size().items(),
                    key=lambda c: c[0]))

    def get_most_abundant_complexes(self):
        """:returns: list of the most abundant KappaComplexes."""
        return (max(self.get_complexes_by_abundance().items(),
                    key=lambda c: c[0]))

    def get_least_abundant_complexes(self):
        """:returns: list of the least abundant KappaComplexes."""
        return (min(self.get_complexes_by_abundance().items(),
                    key=lambda c: c[0]))

    def get_size_distribution(self):
        """:returns: dict ``size : int -> number_of_complexes_of_that_size``

        """
        out = {}
        for (abundance,complx) in self._complexes:
            size = len(complx)
            out[size] = out.get(size,0) + abundance
        return out

    def get_total_mass(self) -> int:
        """Get the total number of agents"""
        out = 0
        for (abundance,complx) in self._complexes:
            out += abundance * len(complx)
        return out

    @property
    def tokens(self):
        """Get the dictionnary ``token : str -> abundance : int``

        """
        return self._tokens

    def get_token_abundance(self,token : str) -> int:
        """Get the abundance of ``token``

        """
        return self._tokens[token]

    @classmethod
    def from_JSONDecoder(cls,data):
        complexes = [ (x[0], KappaComplex.from_JSONDecoder(x[1]))
                      for x in data["snapshot_agents"] ]
        tokens = dict( [ x[1], x[0] ] for x in data["snapshot_tokens"] )
        return cls(time=data["snapshot_time"], event=data["snapshot_event"],
                   complexes=complexes, tokens=tokens)
