exception Undefined
type t

val empty : t
val identity : int list -> t
val add : int -> int -> t -> t
val compose : t -> t -> t
val apply : t -> int -> int
