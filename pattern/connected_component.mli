type cc
type t = cc

type work

module Node  : sig
  type t

  val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
  val print_site :
    ?sigs:Signature.s -> t -> Format.formatter -> int -> unit
  val print_internal :
    ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit
  val rename : work -> cc -> Dipping.t -> t -> t
end

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
val new_node : work -> int (** node_type *) -> (Node.t*work)
val new_link :
  work -> (Node.t * int) (** node * site id *) -> (Node.t * int) -> work
val new_free : work -> (Node.t * int) -> work
val new_internal_state : work -> (Node.t * int) -> int -> work
(** [new_link_type work (node,site) type] *)
val finish_new : work -> (Env.t*Dipping.t*t)

(** {5 Use a connected component } *)
val print : bool -> Signature.s -> Format.formatter -> t -> unit
val print_dot : Signature.s -> Format.formatter -> t -> unit

module Map : MapExt.S with type key=t
