(** Domain to navigate in the graph *)

type cc
type t = cc (**type for domain points*)

type work (**type for partial domain*)

module ContentAgent  : sig
  type t

  val get_sort : t -> int

  val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
  val print_site :
    ?sigs:Signature.s -> t -> Format.formatter -> int -> unit
  val print_internal :
    ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit

  val rename : work -> cc -> Renaming.t -> t -> t
end

module Env : sig
  type t

  val empty : Signature.s -> t
  val sigs : t -> Signature.s
  val nb_ag : t -> int
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit

end

(** {6 Create a connected component} *)

val begin_new : Env.t -> work
(** Starts creation *)
val new_node : work -> int (** node_type *) -> (ContentAgent.t*work)
val new_link :
  work -> (ContentAgent.t * int) (** node * site id *) -> (ContentAgent.t * int) -> work
val new_free : work -> (ContentAgent.t * int) -> work
val new_internal_state : work -> (ContentAgent.t * int) -> int -> work
(** [new_link_type work (node,site) type] *)
val finish_new : ?origin:Operator.rev_dep -> work -> (Env.t*Renaming.t*t)

(** {6 Use a connected component } *)

val print : ?sigs:Signature.s -> bool -> Format.formatter -> t -> unit
(** [print ~sigs print_id form cc] *)
val print_dot : Signature.s -> Format.formatter -> t -> unit

module Matching : sig
  type t
  val empty : t
  val debug_print : Format.formatter -> t -> unit
  val get : (ContentAgent.t * int) -> t -> int
  val reconstruct : Edges.t -> t -> int -> cc -> int -> t option

  val observables_from_free :
    Env.t -> Edges.t -> int -> int -> int -> ((cc * int) list * Operator.DepSet.t)
  (** [observables_from_free domain graph sort agent site] *)
  val observables_from_internal :
    Env.t -> Edges.t -> int -> int -> int -> int -> ((cc * int) list * Operator.DepSet.t)
  (** [observables_from_internal domain graph sort agent site internal_state] *)
  val observables_from_link :
    Env.t -> Edges.t -> int -> int -> int -> int -> int -> int ->
    ((cc * int) list * Operator.DepSet.t)
  (** [observables_from_link domain graph sort ag site sort' ag' site'] *)
end

module Map : MapExt.S with type key=t
