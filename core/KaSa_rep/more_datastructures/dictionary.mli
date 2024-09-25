module type Dictionary = sig
  type key
  type value
  type ('a, 'b) dictionary

  val init : unit -> ('a, 'b) dictionary

  val member :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    value ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught * bool

  val allocate :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught
    * (key * 'a * 'b * ('a, 'b) dictionary) option

  val allocate_uniquely :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught
    * (key * 'a * 'b * ('a, 'b) dictionary) option

  (*  val allocate_f_id: Exception.exceptions_caught_and_uncaught -> ('a -> 'a -> int) -> value -> (int -> 'a) -> 'a dictionary -> Exception.exceptions_caught_and_uncaught * (int * 'a * 'a dictionary) option*)
  val allocate_bool :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('a -> 'a -> key) ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught
    * (bool * (key * 'a * 'b * ('a, 'b) dictionary) option)

  val unsafe_allocate :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    value ->
    'a ->
    (key -> 'b) ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught
    * (key * 'a * 'b * ('a, 'b) dictionary)

  val translate :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught * (value * 'a * 'b) option

  val stabilize : ('a, 'b) dictionary -> ('a, 'b) dictionary

  val iter :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    value ->
    'a ->
    'b ->
    Exception.exceptions_caught_and_uncaught) ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught

  val last_entry :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('a, 'b) dictionary ->
    Exception.exceptions_caught_and_uncaught * key

  val fold :
    (value -> 'a * 'b -> key -> 'c -> 'c) -> ('a, 'b) dictionary -> 'c -> 'c
end

module Dictionary_of_Ord : functor (O : SetMap.OrderedType) ->
  Dictionary with type key = int and type value = O.t
