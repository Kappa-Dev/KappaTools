val local_trace : bool

module IntWL = Mods.IntMap

module type Work_list = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    t ->
    Exception.method_handler * t

  val pop :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    Exception.method_handler * (elt option * t)

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val print_wl : Remanent_parameters_sig.parameters -> t -> unit
end

module WlMake : functor (O : SetMap.OrderedType with type t = int) ->
  Work_list with type elt = O.t
