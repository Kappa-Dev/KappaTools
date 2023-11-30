val overwrite :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  (int, 'b) List_sig.precell List_sig.prelist ->
  'b List_sig.cell ->
  (int -> 'b List_sig.list) ->
  'c ->
  Exception_without_parameter.method_handler
  * ('d * 'e * 'b List_sig.list * 'c) option) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'c ->
  'b List_sig.list * 'b List_sig.list ->
  Exception_without_parameter.method_handler * ('c * 'b List_sig.list option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'c ->
  'b List_sig.list * 'b List_sig.list ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * 'c) ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'c ->
  'b List_sig.list ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * ('c * 'b List_sig.list option)

val length :
  'a ->
  ('b -> 'c -> 'd -> 'e List_sig.list -> 'c * ('d * int option)) ->
  ('b -> 'c -> 'd -> 'e List_sig.list -> int -> 'c * 'd) ->
  'c ->
  'b ->
  'd ->
  'e List_sig.list ->
  'c * ('d * int)

val extensional_without_asso :
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * ('a * int list option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  'b List_sig.list ->
  int list ->
  Exception_without_parameter.method_handler * 'a) ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'a ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * ('a * int list option)

val extensional_with_asso :
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * ('a * (int * 'b) list option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  'b List_sig.list ->
  (int * 'b) list ->
  Exception_without_parameter.method_handler * 'a) ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'a ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler * ('a * (int * 'b) list option)

val build_reversed_sorted_list :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  (int, 'b) List_sig.precell List_sig.prelist ->
  'b List_sig.cell ->
  (int -> 'b List_sig.list) ->
  'c ->
  Exception_without_parameter.method_handler
  * ('d * 'e * 'b List_sig.list * 'c) option) ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'c ->
  (int * 'b) list ->
  Exception_without_parameter.method_handler * ('c * 'b List_sig.list)

val build_sorted_list :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  (int, 'b) List_sig.precell List_sig.prelist ->
  'b List_sig.cell ->
  (int -> 'b List_sig.list) ->
  'c ->
  Exception_without_parameter.method_handler
  * ('d * 'e * 'b List_sig.list * 'c) option) ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'c ->
  (int * 'b) list ->
  Exception_without_parameter.method_handler * ('c * 'b List_sig.list)

val build_list :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  (int, 'b) List_sig.precell List_sig.prelist ->
  'b List_sig.cell ->
  (int -> 'b List_sig.list) ->
  'c ->
  Exception_without_parameter.method_handler
  * ('d * 'e * 'b List_sig.list * 'c) option) ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'c ->
  (int * 'b) list ->
  Exception_without_parameter.method_handler * ('c * 'b List_sig.list)

val print_variables_list :
  Remanent_parameters_sig.parameters -> 'a List_sig.list -> unit

val print_cell :
  Remanent_parameters_sig.parameters ->
  ('a List_sig.list, 'a) List_sig.precell List_sig.prelist ->
  unit

val print_association_list :
  Remanent_parameters_sig.parameters -> int List_sig.list -> unit
