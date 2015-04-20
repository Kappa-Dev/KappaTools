exception Undefined
exception NotBijective
type t

val empty : t
val is_identity : t -> bool
val identity : int list -> t
val add : int -> int -> t -> t
val compose : t -> t -> t
(** @raise Undefined *)
val apply : t -> int -> int
(** @raise Undefined *)
val mem : int -> t -> bool
val inverse : t -> t
(** @raise NotBijective *)

val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
val to_list : t -> (int*int) list

val print : Format.formatter -> t -> unit
(** prints only non identity points *)
val print_full : Format.formatter -> t -> unit
