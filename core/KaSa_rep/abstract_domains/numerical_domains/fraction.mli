type fraction = { num: int; den: int }
type ffraction = Frac of fraction | Infinity | Unknown | Minfinity

val zero : fraction
val string_of : fraction -> string
val fsup : fraction -> fraction -> fraction
val finfeq : fraction -> fraction -> bool
val fplus : fraction -> fraction -> fraction
val ffois : fraction -> fraction -> fraction
val fmoins : fraction -> fraction -> fraction
val fdiv : fraction -> fraction -> fraction
val ffmax : ffraction -> ffraction -> ffraction
val ffinf : ffraction -> ffraction -> bool
val ffmin : ffraction -> ffraction -> ffraction
val ffneg : ffraction -> ffraction
val ffdiv : ffraction -> ffraction -> ffraction
val ffplus : ffraction -> fraction -> ffraction -> ffraction
val cell_int : ffraction -> int
val floor_int : ffraction -> int
val trunc : ffraction -> int
