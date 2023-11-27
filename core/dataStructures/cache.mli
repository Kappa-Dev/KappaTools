module type Cache = sig
  module O : SetMap.OrderedType

  type t

  val last : t -> O.t option
  val create : int option -> t
  val add : O.t -> t -> t
  val fold : (O.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (O.t -> unit) -> t -> unit
end

module Cache : functor (OO : SetMap.OrderedType) -> Cache with type O.t = OO.t
