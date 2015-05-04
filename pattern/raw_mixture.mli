type internal = int option
type link = FREE | VAL of int
type agent =
    { a_id: int; a_type: int; a_ports: link array; a_ints: internal array; }
type t = agent list

val print : Signature.s -> Format.formatter -> t -> unit
