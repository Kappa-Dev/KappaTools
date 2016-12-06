(** Datastructures' functors instantiation *)

val int_compare : int -> int -> int
val int_pair_compare : (int*int) -> (int*int) -> int

module StringSetMap : SetMap.S with type elt = string
module StringSet : SetMap.Set with type elt = string
module StringMap : SetMap.Map with type elt = string and type set = StringSet.t

module IntSetMap : SetMap.S with type elt = int
module IntSet : SetMap.Set with type elt = int
module IntMap : SetMap.Map with type elt = int and type set = IntSet.t

module Int2SetMap : SetMap.S with type elt = int*int
module Int2Set : SetMap.Set with type elt = int*int
module Int2Map : SetMap.Map with type elt = int*int and type set = Int2Set.t

module CharSetMap : SetMap.S with type elt = char
module CharSet : SetMap.Set with type elt = char
module CharMap : SetMap.Map with type elt = char and type set = CharSet.t

module DynArray : GenArray.GenArray
