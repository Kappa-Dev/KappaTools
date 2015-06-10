type tree
val create: int -> tree
val total: tree -> float
val copy: tree -> tree
val copy_in: tree -> tree -> tree
val add: int -> float -> tree -> unit
val random: tree -> int * float
val find : int -> tree -> float
val is_infinite : int -> tree -> bool
val debug_print : Format.formatter -> tree -> unit
