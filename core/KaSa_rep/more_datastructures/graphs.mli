type node

val node_of_int : int -> node
val int_of_node : node -> int

module NodeMap : SetMap.Map with type elt = node

module Nodearray :
  Int_storage.Storage with type key = node and type dimension = int

type ('node_label, 'edge_label) graph

val create :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  (node -> 'node_label) ->
  node list ->
  (node * 'edge_label * node) list ->
  Exception.exceptions_caught_and_uncaught * ('node_label, 'edge_label) graph

val add_bridges :
  ?low:int Nodearray.t ->
  ?pre:int Nodearray.t ->
  ?on_stack:bool Nodearray.t ->
  ?scc:int Nodearray.t ->
  ('a * 'b * 'a -> 'c -> 'c) ->
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  ('a -> string) ->
  ('b -> string) ->
  ('a, 'b) graph ->
  'c ->
  Exception.exceptions_caught_and_uncaught
  * int Nodearray.t
  * int Nodearray.t
  * bool Nodearray.t
  * int Nodearray.t
  * 'c

val compute_scc :
  ?low:int Nodearray.t ->
  ?pre:int Nodearray.t ->
  ?on_stack:bool Nodearray.t ->
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  ('a -> string) ->
  ('a, 'b) graph ->
  Exception.exceptions_caught_and_uncaught
  * int Nodearray.t
  * int Nodearray.t
  * bool Nodearray.t
  * Nodearray.key list list
