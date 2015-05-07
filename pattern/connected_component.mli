type cc
type t = cc

type work

module Node  : sig
  type t

  val get_sort : t -> int

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
(** [print print_id sigs form cc] *)
val print_dot : Signature.s -> Format.formatter -> t -> unit

module Matching : sig
  type t
  val empty : t
  val get : Node.t -> t -> int
  val reconstruct : Edges.t -> t -> cc -> int -> t option

  val observables_from_free :
    Env.t -> Edges.t -> int -> int -> int -> (cc * int) list
  (** [observables_from_free domain graph sort agent site] *)
  val observables_from_internal :
    Env.t -> Edges.t -> int -> int -> int -> int -> (cc * int) list
  (** [observables_from_internal domain graph sort agent site internal_state] *)
  val observables_from_link :
    Env.t -> Edges.t -> int -> int -> int -> int -> int -> int -> (cc * int) list
  (** [observables_from_link domain graph sort ag site sort' ag' site'] *)
end

module Map : MapExt.S with type key=t
