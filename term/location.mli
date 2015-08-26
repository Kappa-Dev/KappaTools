type t
type 'a annot = 'a * t
type 'a maybe = ?pos:t -> 'a

val of_pos : Lexing.position -> Lexing.position -> t
val dummy : t

val dummy_annot : 'a -> 'a annot
val has_dummy_annot : 'a annot -> bool

val print : Format.formatter -> t -> unit
val print_annot :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a annot -> unit
