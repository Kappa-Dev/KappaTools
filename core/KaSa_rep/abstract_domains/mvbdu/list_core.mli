val sanity_check : bool
val test_workbench : bool
val list_equal : 'a -> 'a -> bool

val get_skeleton :
  ('a List_sig.list, 'b) List_sig.precell List_sig.prelist ->
  (int, 'b) List_sig.precell List_sig.prelist

val build_list :
  ('a ->
  ('b -> 'b -> int) ->
  'c ->
  'd List_sig.cell ->
  (int -> 'd List_sig.list) ->
  'e ->
  'f) ->
  'a ->
  'e ->
  'c ->
  'd List_sig.cell ->
  'f

val update_association_dictionary :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler ->
  'c ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler

val update_range_dictionary :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler ->
  'd ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler

val update_variables_dictionary :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler ->
  'e ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler

val print_list :
  'a ->
  ('a -> Remanent_parameters_sig.parameters -> 'b) ->
  (int -> string) ->
  ('c -> string) ->
  Remanent_parameters_sig.parameters ->
  'c List_sig.list ->
  'b

val id_of_list : 'a List_sig.list -> int
