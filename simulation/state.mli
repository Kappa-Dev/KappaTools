open Mods

type t

module Embedding:
sig
  type info = {map: int IntMap.t ;
	       roots : IntSet.t ;
	       components : IntSet.t IntMap.t option;
	       depth_map : int IntMap.t option}
  type t = DISJOINT of info
	 | CONNEX of info
	 | AMBIGUOUS of info

  val empty : t
  val map_of : t -> int IntMap.t
end

val rule_of_id : int -> t -> Primitives.rule
val kappa_of_id : int -> t -> Mixture.t
val total_activity : t -> float
val get_graph : t -> Graph.SiteGraph.t
val get_nl_injections : t -> (InjProdHeap.t option) array
val maybe_find_perturbation : int -> t -> Primitives.perturbation option
val remove_perturbation : int -> t -> t
val all_perturbations : t -> IntSet.t

val value :
  t -> Counter.t -> ?time:float -> Environment.t ->
  'a Primitives.variable -> 'a

val instance_number : int -> t -> Environment.t -> Nbr.t
val nl_instance_number : int -> t -> Environment.t -> Nbr.t

val unsilence_rule : t -> Primitives.rule -> Counter.t -> Environment.t -> unit
val is_complete : int -> t -> bool
val fold_graph : (int -> Node.t -> 'a -> 'a) -> t -> 'a -> 'a

val draw_rule :
  t -> Counter.t -> Environment.t -> (Primitives.rule * Embedding.t) option * t

val update_activity :
  t -> ?cause:int -> int -> Counter.t -> Environment.t -> unit
val update_dep :
  t -> ?cause:int -> Term.dep_type -> IntSet.t ->
  Counter.t -> Environment.t -> Environment.t * IntSet.t
val update_dep_value : t -> Counter.t -> Environment.t ->
		       Nbr.t Primitives.variable -> Term.dep_type -> unit

val select_injection : float * Nbr.t Primitives.variable option ->
		       float * Nbr.t Primitives.variable option -> t ->
		       Mixture.t -> Counter.t -> Environment.t -> Embedding.t
val apply :
  t -> Primitives.rule -> Embedding.t -> Counter.t -> Environment.t ->
  Environment.t * t * Int2Set.t * Embedding.t * int IntMap.t * IntSet.t
val positive_update :
  ?with_tracked:(int * int IntMap.t) list ->
  t -> Primitives.rule -> int IntMap.t * int IntMap.t ->
  Int2Set.t * Int2Set.t -> Counter.t -> Environment.t ->
  Environment.t * t * IntSet.t * InjectionHeap.content list *
    (int * int IntMap.t) list
val instances_of_square :
  ?disjoint:bool -> int -> int -> t ->
  Environment.t -> (int IntMap.t * IntSet.t * InjectionHeap.content list) list

val initialize :
  Graph.SiteGraph.t -> float array -> Primitives.rule list ->
  Mixture.t list ->
  (((int -> Nbr.t) ->
    (int -> Nbr.t) -> float -> int -> int -> float -> (int -> Nbr.t) -> Nbr.t) *
     bool * Nbr.t option * 'a * string)
    list ->
  Primitives.perturbation list * Primitives.rule list ->
  Counter.t -> Environment.t -> t * Environment.t

val dump_rules : t -> Environment.t -> unit
val snapshot : t -> Counter.t -> out_channel -> bool -> Environment.t -> unit
val dump : t -> Counter.t -> Environment.t -> unit
val dot_of_flux : out_channel -> t -> Environment.t -> unit
val dot_of_influence_map : out_channel -> t -> Environment.t -> unit

val print_observables_header : out_channel -> t -> unit
val print_observables_values :
  out_channel -> float -> Environment.t -> Counter.t -> t -> unit

module Safe : sig
  type check_options = {rule_act : bool ; lifts : bool ; unary : bool}

  val check : int -> check_options
  val check_invariants : check_options -> t -> Counter.t -> Environment.t -> unit
end
