open Format

val list : (formatter -> unit) -> (formatter -> 'a -> unit) ->
	   formatter -> 'a list -> unit
val set : ('b -> 'a list) -> (formatter -> unit) -> (formatter -> 'a -> unit) ->
	  formatter -> 'b -> unit
val hashtbl : (formatter -> unit) -> (formatter -> 'a * 'b -> unit) ->
	  formatter -> ('a,'b) Hashtbl.t -> unit

val option : (formatter -> 'a -> unit) -> formatter -> 'a option -> unit
val comma : formatter -> unit
val colon : formatter -> unit
val space : formatter -> unit
val empty : formatter -> unit

val array : (formatter -> 'a -> unit) -> formatter -> 'a array -> unit

val error : (formatter -> 'a -> unit) -> 'a Term.with_pos -> unit
val position : formatter -> (Lexing.position * Lexing.position) -> unit

val list_to_string : (unit -> string) ->
		     (unit -> 'a -> string) -> unit -> 'a list -> string
val set_to_string : ('b -> 'a list) -> (unit -> string) ->
		    (unit -> 'a -> string) -> unit -> 'b -> string
