type t
type id = int
type update = { id: id; previous_threshold: int; current_threshold: int }
type updates = update list

val init : unit -> t
(*val do_it: unit -> unit *)

val copy : t -> t
val create : t -> id -> t
val bind : t -> id -> id -> t
val unbind : t -> id -> id -> t
val degrade : neighbor:(id -> id list) -> t -> id -> t

val flush :
  neighbor:(id -> id list) -> thresholds:(int -> int) -> t -> t * updates

val print_all : Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val print_update : Format.formatter -> t -> unit
val build_threshold : Mods.IntSet.t -> int Array.t
val eval_threshold : int Array.t -> int -> int
val size_of_cc : t -> id -> t * int option
val is_connected : t -> id -> id -> t * bool
