type t
type id = int
type thresholds_state (*= int option * int Mods.IntMap.t *)
type update = { id: id * id option; previous_threshold: id; current_threshold: id }
type updates = update list
type 'a pos_neg
type cache 

type previous_threshold_value = int Array.t * int Array.t Array.t 
type weight 
val init : unit -> t

(*val do_it: unit -> unit *)
val dummy_cache : cache
val copy : t -> t
val create : t -> id -> int -> t
val bind : t -> id -> id -> t
val unbind : t -> id -> id -> t
val degrade : neighbor:(id -> id list) -> t -> id -> t

val flush :
  neighbor:(id -> id list) -> thresholds:(weight -> weight) -> t -> t * updates

val print_all : Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val print_update : Format.formatter -> t -> unit
val build_threshold : int -> Mods.IntSet.t * Mods.IntSet.t Mods.IntMap.t -> previous_threshold_value
val eval_threshold : previous_threshold_value -> weight -> weight 

val is_connected : t -> id -> id -> t * bool
val get_between_thresholds : cache -> int option -> id -> id -> (id  * id option * bool) pos_neg
val init_between_thresholds : int -> Mods.IntSet.t * Mods.IntSet.t Mods.IntMap.t -> cache
val get_positive_update : 'a pos_neg -> 'a list
val get_negative_update : 'a pos_neg -> 'a list
val json_of_cache : cache -> Yojson.Basic.t
val cache_of_json : Yojson.Basic.t -> cache
