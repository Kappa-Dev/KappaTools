type 'a bot_or_not = Bot | Not_bot of 'a
type maybe_bool = Sure_value of bool | Maybe
type 'a top_or_not = Top | Not_top of 'a
type 'a flat_lattice = Val of 'a | Any | Undefined

val lub : 'a flat_lattice -> 'a flat_lattice -> 'a flat_lattice

val glb_list :
  'a list flat_lattice -> 'a list flat_lattice -> 'a list flat_lattice
