(* versions of the module with logging primitives *)
(* type are shared among both versions *)
module LoggedStringSetMap = Map_wrapper.Make (Mods.StringSetMap)
(** Time-stamp: <Jul 02 2016> *)

module LoggedStringSet = LoggedStringSetMap.Set
module LoggedStringMap = LoggedStringSetMap.Map
module LoggedIntSetMap = Map_wrapper.Make (Mods.IntSetMap)
module LoggedIntSet = LoggedIntSetMap.Set
module LoggedIntMap = LoggedIntSetMap.Map
module LoggedInt2SetMap = Map_wrapper.Make (Mods.Int2SetMap)
module LoggedInt2Set = LoggedInt2SetMap.Set
module LoggedInt2Map = LoggedInt2SetMap.Map
module LoggedCharSetMap = Map_wrapper.Make (Mods.CharSetMap)
module LoggedCharSet = LoggedCharSetMap.Set
module LoggedCharMap = LoggedCharSetMap.Map

(* versions of the module with logging primitives *)
(* type are hidden, cannot be used with not logged versions *)
module ParanoStringSetMap = Map_wrapper.Make (Mods.StringSetMap)
module ParanoStringSet = ParanoStringSetMap.Set
module ParanoStringMap = ParanoStringSetMap.Map
module ParanoIntSetMap = Map_wrapper.Make (Mods.IntSetMap)
module ParanoIntSet = ParanoIntSetMap.Set
module ParanoIntMap = ParanoIntSetMap.Map
module ParanoInt2SetMap = Map_wrapper.Make (Mods.Int2SetMap)
module ParanoInt2Set = ParanoInt2SetMap.Set
module ParanoInt2Map = ParanoInt2SetMap.Map
module ParanoCharSetMap = Map_wrapper.Make (Mods.CharSetMap)
module ParanoCharSet = ParanoCharSetMap.Set
module ParanoCharMap = ParanoCharSetMap.Map
