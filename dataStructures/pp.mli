val list : (Format.formatter -> unit) ->
	   (Format.formatter -> 'a -> unit) ->
	   Format.formatter -> 'a list -> unit
val set : ('b -> 'a list) -> (Format.formatter -> unit) ->
	  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'b -> unit
val string : Format.formatter -> string -> unit
val int : Format.formatter -> int -> unit
val comma : Format.formatter -> unit
val colon : Format.formatter -> unit
val space : Format.formatter -> unit
val empty : Format.formatter -> unit
val error : (Format.formatter -> 'a -> unit) -> 'a Term.with_pos -> unit
val position : Format.formatter -> (Lexing.position * Lexing.position) -> unit

val list_to_string : (unit -> string) ->
		     (unit -> 'a -> string) -> unit -> 'a list -> string
val set_to_string : ('b -> 'a list) -> (unit -> string) ->
		    (unit -> 'a -> string) -> unit -> 'b -> string
