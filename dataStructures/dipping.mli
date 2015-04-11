exception Undefined
type t

val empty : t
val is_identity : t -> bool
val identity : int list -> t
val add : int -> int -> t -> t
val compose : t -> t -> t
val apply : t -> int -> int
val mem : int -> t -> bool

(* val fold : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a *)
val to_list : t -> (int*int) list

val print : Format.formatter -> t -> unit
(** prints only non identity points *)
val print_full : Format.formatter -> t -> unit
