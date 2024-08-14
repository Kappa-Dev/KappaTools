module type Hash = sig
  type key
  type 'a hash

  val create : int -> 'a hash

  val add :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.exceptions_caught_and_uncaught * 'a hash

  val overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.exceptions_caught_and_uncaught * 'a hash

  val add_or_overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.exceptions_caught_and_uncaught * 'a hash

  val find_option :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    'a hash ->
    Exception.exceptions_caught_and_uncaught * (int * 'a) option

  val find_option_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    'a hash ->
    Exception.exceptions_caught_and_uncaught * (int * 'a) option

  val find_option_log_on_the_fly : key -> 'a hash -> (int * 'a) option
  val iter : (key -> 'a -> int -> unit) -> 'a hash -> unit
  val fold : (key -> 'b -> int -> 'a -> 'a) -> 'b hash -> 'a -> 'a
end

module Hash_of_Ord : functor (O : SetMap.OrderedType) ->
  Hash with type key = O.t
