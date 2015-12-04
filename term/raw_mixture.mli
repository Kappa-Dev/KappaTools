type internal = int option
type link = FREE | VAL of int
type agent =
    { a_id: int; a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

val equal : t -> t -> bool
val print : compact:bool -> Signature.s -> Format.formatter -> t -> unit
val print_dot : Signature.s -> int -> Format.formatter -> t -> unit
