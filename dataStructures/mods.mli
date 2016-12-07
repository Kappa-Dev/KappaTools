(** Datastructures' functors instantiation *)

val int_compare : int -> int -> int
val int_pair_compare : (int*int) -> (int*int) -> int

module StringSetMap : SetMap.S with type elt = string
module StringSet = StringSetMap.Set
module StringMap = StringSetMap.Map

module IntSetMap : SetMap.S with type elt = int
module IntSet = IntSetMap.Set
module IntMap = IntSetMap.Map

module Int2SetMap : SetMap.S with type elt = int*int
module Int2Set = Int2SetMap.Set
module Int2Map = Int2SetMap.Map

module CharSetMap : SetMap.S with type elt = char
module CharSet = CharSetMap.Set
module CharMap = CharSetMap.Map

module DynArray : GenArray.GenArray
