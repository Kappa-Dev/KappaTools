type t = { mutable delimiter: char }

val options : t -> (string * Arg.spec * string) list
val default : t
