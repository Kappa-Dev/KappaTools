val t_string : Remanent_parameters_sig.parameters -> string -> unit
val t_int : Remanent_parameters_sig.parameters -> int -> unit
val comp_pair : 'a * 'b -> 'a * 'b -> int
val list_it : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val true_map : ('a -> 'b) -> 'a list -> 'b list

val max_of_list :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a list ->
  Exception_without_parameter.method_handler * 'a option

val sum_list : int list -> int
val member2 : 'a -> ('a * 'b) list -> bool
val all_differents : int list -> bool
val dolist : ('a -> 'a) list -> 'a -> 'a
val applati : 'a list list -> 'a list
val forall : ('a -> bool) -> 'a list -> bool

val tr :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a list ->
  'a list ->
  'a ->
  Exception_without_parameter.method_handler * 'a option

val paire_of_list : 'a list -> 'b list -> ('a * 'b) list -> ('a * 'b) list
val produit_predicat : ('a -> 'a -> bool) -> 'a * 'a -> 'a * 'a -> bool
val union_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val intersec_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val flat_map_zip : ('a -> 'b list * 'c list) -> 'a list -> 'b list * 'c list
val mix_list : 'a list list list -> 'a list list
val copy_table : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t
val insert_sort : ('a -> 'a -> bool) -> 'a list -> 'a -> 'a list
val insert_list : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
val fusion : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val sub_list : ('a -> 'a -> bool) -> 'a list -> 'a -> 'a list
val filtre : ('a -> bool) -> 'a list -> 'a list
val rev : 'a list -> 'a list

val flap_map :
  Remanent_parameters_sig.parameters ->
  'a ->
  ('b -> 'c) ->
  'b list list ->
  'a * 'c list

val map_list : ('a -> 'b) -> 'a list -> 'b list

val compte_list :
  Remanent_parameters_sig.parameters -> 'a -> 'b list -> 'a * ('b * int) list

val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val vide : ('a -> bool) -> 'a list -> 'a list
