type 'a bot_or_not = Bot | Not_bot of 'a
type maybe_bool = Sure_value of bool | Maybe
type 'a top_or_not = Top | Not_top of 'a
type 'a flat_lattice = Val of 'a | Any | Undefined

val lub : 'a flat_lattice -> 'a flat_lattice -> 'a flat_lattice

val lub_with_or :
  ('parameters -> 'handler -> 'error -> 'a -> 'a -> 'error * 'handler * 'a) ->
  'parameters ->
  'handler ->
  'error ->
  ('e * 'a) flat_lattice ->
  ('e * 'a) flat_lattice ->
  'error * 'handler * ('e * 'a) flat_lattice

val glb_list_with_and :
  ('parameters -> 'handler -> 'error -> 'a -> 'a -> 'error * 'handler * 'a) ->
  'parameters ->
  'handler ->
  'error ->
  ('e * 'a) list flat_lattice ->
  ('e * 'a) list flat_lattice ->
  'error * 'handler * ('e * 'a) list flat_lattice

val print_elt :
  (Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  'key ->
  'error * 'handler) ->
  (Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  'data ->
  'error * 'handler) ->
  Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  ('key * 'data) flat_lattice ->
  'error * 'handler

val print_list :
  (Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  'key ->
  'error * 'handler) ->
  (Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  'data ->
  'error * 'handler) ->
  Remanent_parameters_sig.parameters ->
  'handler ->
  'error ->
  ('key * 'data) list flat_lattice ->
  'error * 'handler
