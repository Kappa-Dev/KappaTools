(**Module for pattern expressions*)

type agent
type t

val empty_agent: agent 

val set_root_of_cc : t -> t
val root_of_cc : t -> int -> int option

(* links structure *) 
val graph: t -> (int*int) Mods.Int2Map.t 

(**[ids_of_name name mix] returns all agent id's in mixture [mix] whose name is equal to [name] *)
val ids_of_name : int*int -> t -> Mods.IntSet.t

(**[agents mix] returns a map of agents in the mixture [mix]*)
val agents : t -> agent Mods.IntMap.t 

(**[agent_of_id i mix] returns the agent identified by [i] in mixture [mix]*)
val agent_of_id : int -> t -> agent

(**[name ag] returns the name of agent [ag]*)
val name : agent -> int

(**The empty mixture*)
val empty : int option -> t
val is_empty : t -> bool

(**Whether mixture's cc's should be embedded in a connected graph*)
val unary : t -> bool
val set_unary : t -> t

val size_of_cc : int -> t -> int

val get_id : t -> int

(**[arity mix] returns the number of connected components in [mix]*)
val arity : t -> int

(**[component_of_id id mix] returns the number of the connected component containing [id]. NB [component_of_id id mix] always returns 0 if [arity mix]=1*)
val component_of_id : int -> t -> int

(**[interface ag] returns the map site_id->(int state,lnk state)*)
val interface : agent -> (int option * Node.lnk_t) Mods.IntMap.t

(**[is_bound (a_i,s_j) mix] returns [true] if site [s_j] of agent [a_i] is bound (or belongs to a semi link) in mixture [mix]*)
val is_bound : (int*int) -> t -> bool

(**[to_string mix] displays a string representing mixture [mix]*)
val to_kappa : bool -> t -> Environment.t -> string

(**[site_defined site_id mix is_added env]*)
val site_defined : int -> agent -> bool -> Environment.t -> (int option * Node.lnk_t) option

(**[compose i ag mix edg cstr_opt] add agent [ag] with identifier [i] to mixture [mix] with edges [edg] represented as a map (id,j)->(k,l) where k is an agent identifier and j,l are site indices*)
(**[cstr_opt] is used to add [constraints] to construct a mixture that requires some side checks upon matching*)
val compose : int -> agent -> t -> (int*int) Mods.Int2Map.t -> t

(**[follow_in_spanning_tree (root_ag,root_site) (id,i) mix] *)
val follow_in_spanning_tree : int -> (int*int) -> t -> (int*int) option
val follow : (int*int) -> t -> (int*int) option

(**[fold_interface f ag cont] iterator of agent [ag] with continuation [cont]*)
val fold_interface : (int -> (int option * Node.lnk_t) -> 'a -> 'a) -> agent -> 'a -> 'a

(**[create_agent name port_map] returns an agent with name [name] and interface specified by the map [port_map] mapping site [i] to [(internal_state*link_state)]
with None representing absence of information *)
val create_agent : int -> (int option * Node.lnk_t) Mods.IntMap.t -> agent

(**[enum_alternate_anchors mix] Initialize the field [enum_cov] of mixture mix*)
val enum_alternate_anchors : t -> t

(**[internal_edges i mix] returns internal edges completing the spanning tree of mixture [mix] using root [i]*)
val internal_edges : int -> t -> (int*int) Mods.Int2Map.t

(**for debugging*)
val dump_span : (*Environment.t ->*) t -> unit
