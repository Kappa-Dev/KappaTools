val fst_option : ('a * 'b) option -> 'a option
val snd_option : ('a * 'b) option -> 'b option
val escape_label_in_dot : string -> string

val make_id_compatible_with_dot_format :
  Remanent_parameters_sig.parameters -> 'a -> string -> 'a * string

val sorted_parts_of_list : int -> 'a list -> 'a list list

val sort_list :
  ('a -> 'b -> 'c -> 'b * 'd) -> 'a -> 'b -> 'c list -> 'b * 'c list
