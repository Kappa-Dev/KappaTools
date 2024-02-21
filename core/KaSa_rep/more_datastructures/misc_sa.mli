val const_unit : 'a -> unit

val array_of_list :
  ('a -> 'b -> int -> 'c * 'd) ->
  ('a -> 'c -> int -> 'e -> 'd -> 'c * 'd) ->
  'a ->
  'b ->
  'e list ->
  'c * 'd

val unsome : 'a * 'b option -> ('a -> 'a * 'b) -> 'a * 'b
val trace : Remanent_parameters_sig.parameters -> (unit -> string) -> unit
val inter_list : ('a -> 'b -> int) -> 'a list -> 'b list -> 'a list
val list_0_n : int -> int list
val list_minus : 'a list -> 'a list -> 'a list
val print_comma : Remanent_parameters_sig.parameters -> bool -> string -> unit
val fetch_array : int -> 'a option array -> 'a -> 'a
