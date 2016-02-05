(** Functions from a subset of nat to a subset of nat *)

exception Undefined
exception NotBijective
exception Clashing

type t

val empty : t
val is_identity : t -> bool
val identity : int list -> t

val cyclic_permutation_from_list : stop_at:int -> int list -> t
(** very specific use case for Connected_component.remove_ag_cc *)

val add : int -> int -> t -> t
(** @raise Clashing in debug mode *)

val compose : bool -> t -> t -> t
(** @raise Undefined *)

val apply : t -> int -> int
(** @raise Undefined *)

val mem : int -> t -> bool
val inverse : t -> t
(** @raise NotBijective *)

val compare : t -> t -> int
val equal : t -> t -> bool

val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
val to_list : t -> (int*int) list

val print : Format.formatter -> t -> unit
(** prints only non identity points *)

val print_full : Format.formatter -> t -> unit
