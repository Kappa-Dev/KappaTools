(** Domain to navigate in the graph *)

type cc
type t = cc (**type for domain points*)

type work (**type for partial domain*)

module ContentAgent  : sig
  type t

  val get_sort : t -> int

  val print :
    ?sigs:Signature.s -> ?with_id:unit -> Format.formatter -> t -> unit
  val print_site :
    ?sigs:Signature.s -> t -> Format.formatter -> int -> unit
  val print_internal :
    ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

  val rename : work -> cc -> Renaming.t -> t -> t
end

module Env : sig
  type t

  val empty : Signature.s -> t
  val finalize : t -> t

  val sigs : t -> Signature.s
  val nb_ag : t -> int
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit

end

(** {6 Create a connected component} *)

val begin_new : Env.t -> work
(** Starts creation *)

val new_node : work -> int -> (ContentAgent.t*work)
(** [new_node wk node_type] *)

val new_link :
  work -> (ContentAgent.t * int) -> (ContentAgent.t * int) -> work
(** [new_link wk (node, site_id) (node', site_id')] *)

val new_free : work -> (ContentAgent.t * int) -> work
val new_internal_state : work -> (ContentAgent.t * int) -> int -> work
(** [new_link_type work (node,site) type] *)

val finish_new : ?origin:Operator.rev_dep -> work -> (Env.t*Renaming.t*t)

(** {6 Use a connected component } *)

val is_equal_canonicals : t -> t -> bool
val print : ?sigs:Signature.s -> ?with_id:unit -> Format.formatter -> t -> unit
(** [print ~sigs ?with_id:None form cc] *)

val print_dot : Signature.s -> Format.formatter -> t -> unit

val find_root_type : t -> int option

module Matching : sig
  type t
  val empty : t
  val debug_print : Format.formatter -> t -> unit
  val get : (ContentAgent.t * int) -> t -> int
  val reconstruct : Edges.t -> t -> int -> cc -> int -> t option
  (** [reconstruct graph matching_of_previous_cc cc_id_in_rule cc root ]*)

  val get_all : Edges.t -> t -> cc -> int -> int list
  (** [get_all graph matching_of_previous_cc cc root ]*)

  val get_all_with_types : Edges.t -> t -> cc -> int -> (int * int) list

  type cache
  val empty_cache : cache

  val observables_from_agent :
    Env.t -> Edges.t ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache) -> Edges.agent ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_free domain graph sort agent]
    the int * int in the return list and the following ones
    is a Instantiation.concrete *)

  val observables_from_free :
    Env.t -> Edges.t ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache) ->
    Edges.agent -> int ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_free domain graph sort agent site] *)

  val observables_from_internal :
    Env.t -> Edges.t ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache) ->
     Edges.agent -> int -> int ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_internal domain graph sort agent site internal_state] *)

  val observables_from_link :
    Env.t -> Edges.t ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache) ->
     Edges.agent -> int -> Edges.agent -> int ->
    (((cc * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_link domain graph sort ag site sort' ag' site'] *)
end

module Set : SetMap.Set with type elt=t
module Map : SetMap.Map with type elt=t
