type cc
type t = cc

type work
type node

type 'id place = Existing of 'id
	       | Fresh of int * int (* type, id *)

val rename_place : work -> cc -> Dipping.t -> node place -> node place

module Env : sig
  type t

  val empty : Signature.s -> t
  val sigs : t -> Signature.s
  val nb_ag : t -> int
  val print : Format.formatter -> t -> unit
  val print_dot : Format.formatter -> t -> unit
end

(** {5 Create a connected component } *)
val begin_new : Env.t -> work
val new_node : work -> int (** node_type *) -> (node*work)
val new_link :
  work -> (node * int) (** node * site id *) -> (node * int) -> work
val new_free : work -> (node * int) -> work
val new_internal_state : work -> (node * int) -> int -> work
(** [new_link_type work (node,site) type] *)
val finish_new : work -> (Env.t*Dipping.t*t)

(** {5 Use a connected component } *)
(*val equal : t -> t -> bool*)
val print_place : Signature.s -> Format.formatter -> node place -> unit
val print_place_site :
  Signature.s -> node place -> Format.formatter -> int -> unit
val print_place_internal :
  Signature.s -> node place -> int -> Format.formatter -> int -> unit
val print : bool -> Signature.s -> Format.formatter -> t -> unit
val print_dot : Signature.s -> Format.formatter -> t -> unit
