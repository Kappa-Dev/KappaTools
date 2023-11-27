module type Union_find = sig
  type key
  type dimension
  type t

  val create :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    dimension ->
    Exception.method_handler * t

  val union_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    key list ->
    Exception.method_handler * t

  val iteri :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    key ->
    Exception.method_handler) ->
    t ->
    Exception.method_handler

  val get_representent :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    t ->
    Exception.method_handler * t * key
end

module Make : functor (Storage : Int_storage.Storage) ->
  Union_find
    with type key = Storage.key
     and type t = Storage.key Storage.t
     and type dimension = Storage.dimension
