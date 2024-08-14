module type Union_find = sig
  type key
  type dimension
  type t

  val create :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    dimension ->
    Exception.exceptions_caught_and_uncaught * t

  val union_list :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    t ->
    key list ->
    Exception.exceptions_caught_and_uncaught * t

  val iteri :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    (Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    key ->
    Exception.exceptions_caught_and_uncaught) ->
    t ->
    Exception.exceptions_caught_and_uncaught

  val get_representent :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    key ->
    t ->
    Exception.exceptions_caught_and_uncaught * t * key
end

module Make : functor (Storage : Int_storage.Storage) ->
  Union_find
    with type key = Storage.key
     and type t = Storage.key Storage.t
     and type dimension = Storage.dimension
