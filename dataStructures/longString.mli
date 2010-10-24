type t 
val concat : ?sep:char -> string -> t -> t
val print : t -> unit
val printf : ?no_reverse:bool -> out_channel -> t -> unit
val to_string : t -> string
val empty : t
val is_empty : t -> bool
