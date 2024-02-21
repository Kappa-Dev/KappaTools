module type Hash = sig
  type hashed_list
  type elt
  type cache

  val int_of_hashed_list : hashed_list -> int
  val compare : hashed_list -> hashed_list -> int
  val init : unit -> cache
  val hash : cache -> elt list -> cache * hashed_list
  val cons : cache -> elt -> hashed_list -> cache * hashed_list
  val empty : hashed_list
  val print : Format.formatter -> hashed_list -> unit
  val print_cache : Format.formatter -> cache -> unit
end

module Make (A : SetMap.OrderedType) : Hash with type elt = A.t
