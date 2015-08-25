type 'a annot = 'a * (Lexing.position * Lexing.position)
type 'a maybe = ?pos:(Lexing.position * Lexing.position) -> 'a

val dummy_annot : 'a -> 'a annot
val has_dummy_annot : 'a annot -> bool

val print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a annot -> unit
