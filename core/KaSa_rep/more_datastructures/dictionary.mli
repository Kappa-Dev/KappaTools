module type Dictionary = sig
  type key
  type value
  type ('a, 'b) dictionary

  val init : unit -> ('a, 'b) dictionary

  val member :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    value ->
    ('a, 'b) dictionary ->
    Exception.method_handler * bool

  val allocate :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.method_handler * (key * 'a * 'b * ('a, 'b) dictionary) option

  val allocate_uniquely :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.method_handler * (key * 'a * 'b * ('a, 'b) dictionary) option

  (*  val allocate_f_id: Exception.method_handler -> ('a -> 'a -> int) -> value -> (int -> 'a) -> 'a dictionary -> Exception.method_handler * (int * 'a * 'a dictionary) option*)
  val allocate_bool :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.method_handler
    * (bool * (key * 'a * 'b * ('a, 'b) dictionary) option)

  val unsafe_allocate :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.method_handler * (key * 'a * 'b * ('a, 'b) dictionary)

  val translate :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    ('a, 'b) dictionary ->
    Exception.method_handler * (value * 'a * 'b) option

  val stabilize : ('a, 'b) dictionary -> ('a, 'b) dictionary

  val iter :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    value ->
    'a ->
    'b ->
    Exception.method_handler) ->
    ('a, 'b) dictionary ->
    Exception.method_handler

  val last_entry :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('a, 'b) dictionary ->
    Exception.method_handler * key

  val fold :
    (value -> 'a * 'b -> key -> 'c -> 'c) -> ('a, 'b) dictionary -> 'c -> 'c
end

module Dictionary_of_Ord : functor (O : SetMap.OrderedType) ->
  Dictionary with type key = int and type value = O.t
