val sanity_check : bool
val test_workbench : bool
val print_flag : Remanent_parameters_sig.parameters -> bool -> unit

val get_skeleton :
  (('a Mvbdu_sig.mvbdu, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  ((int, 'd) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu

val compress_node :
  ('a ->
  ('b -> 'b -> int) ->
  ((int, 'c) Mvbdu_sig.precell, 'd) Mvbdu_sig.premvbdu ->
  'd Mvbdu_sig.cell ->
  (int -> 'd Mvbdu_sig.mvbdu) ->
  'e ->
  'a * (int * 'd Mvbdu_sig.cell * 'd Mvbdu_sig.mvbdu * 'e) option) ->
  'a ->
  'e ->
  (('d Mvbdu_sig.mvbdu, 'f) Mvbdu_sig.precell, 'd) Mvbdu_sig.premvbdu ->
  'a * (int * 'd Mvbdu_sig.cell * 'd Mvbdu_sig.mvbdu * 'e) option

val build_already_compressed_cell :
  ('a ->
  ('b -> 'b -> int) ->
  'c ->
  'd Mvbdu_sig.cell ->
  (int -> 'd Mvbdu_sig.mvbdu) ->
  'e ->
  'f) ->
  'a ->
  'e ->
  'c ->
  'd Mvbdu_sig.cell ->
  'f

val print_mvbdu :
  'a ->
  ('a -> Remanent_parameters_sig.parameters -> 'b -> 'a) ->
  (int -> string) ->
  Remanent_parameters_sig.parameters ->
  'b Mvbdu_sig.mvbdu ->
  'a

val id_of_mvbdu : 'a Mvbdu_sig.mvbdu -> int

val update_dictionary :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler ->
  'b ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g) Memo_sig.handler

val last_entry :
  'a ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler ->
  'i ->
  ('a -> 'i -> 'c -> 'j) ->
  'j

val mvbdu_equal : 'a -> 'a -> bool
