open Mods
(* versions of the module with logging primitives *)
(* type are shared among both versions *) 
module LoggedStringSetMap = Map_wrapper.Make(StringSetMap) 
module LoggedStringSet = LoggedStringSetMap.Set
module LoggedStringMap = LoggedStringSetMap.Map
module LoggedIntSetMap = Map_wrapper.Make(IntSetMap)
module LoggedIntSet = LoggedIntSetMap.Set
module LoggedIntMap = LoggedIntSetMap.Map
module LoggedInt2SetMap = Map_wrapper.Make(Int2SetMap)					  
module LoggedInt2Set = LoggedInt2SetMap.Set
module LoggedInt2Map = LoggedInt2SetMap.Map
module LoggedCharSetMap = Map_wrapper.Make (CharSetMap)
module LoggedCharSet = LoggedCharSetMap.Set
module LoggedCharMap = LoggedCharSetMap.Map

(* versions of the module with logging primitives *)
(* type are hidden, cannot be used with not logged versions *)
module ParanoStringSetMap = Map_wrapper.Make (SetMap.Make (String))
module ParanoStringSet = ParanoStringSetMap.Set
module ParanoStringMap = ParanoStringSetMap.Map
module ParanoIntSetMap =
  Map_wrapper.Make (SetMap.Make (struct type t = int let compare = int_compare end))
module ParanoIntSet = ParanoIntSetMap.Set			
module ParanoIntMap = ParanoIntSetMap.Map
module ParanoInt2SetMap = Map_wrapper.Make (SetMap.Make (struct type t = int*int let compare = int_pair_compare end))
module ParanoInt2Set = ParanoInt2SetMap.Set
module ParanoInt2Map = ParanoInt2SetMap.Map
module ParanoCharSetMap = Map_wrapper.Make (SetMap.Make (struct type t = char let compare = compare end))
module ParanoCharSet = ParanoCharSetMap.Set
module ParanoCharMap = ParanoCharSetMap.Map
