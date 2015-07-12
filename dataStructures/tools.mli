(** {5 Old fashion positions } *)
type pos = string * int * int
val no_pos : pos
val fn : pos -> string
val ln : pos -> int
val cn : pos -> int

(** {5 Combinators on primitive types *)
val option_map : ('a -> 'b) -> 'a option -> 'b option
val iteri : (int -> unit) -> int -> unit
val array_map_of_list : ('a -> 'b) -> 'a list -> 'b array
val array_fold_lefti :
  (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val array_filter : (int -> 'a -> bool) -> 'a array -> int list
val array_fold_left_mapi :
  (int -> 'a -> 'b -> 'a * 'c) -> 'a -> 'b array -> 'a * 'c array
val array_fold_left2i :
  (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val list_last : 'a list -> 'a
val list_exists_uniq :
  ('a -> bool) -> 'a list -> bool
val list_rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
val list_map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val list_fold_right_map :
  ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val list_fold_left2 :
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a

(** not tail rec but don't allocate if unecessary *)
val list_smart_filter : ('a -> bool) -> 'a list -> 'a list
val list_smart_map : ('a -> 'a) -> 'a list -> 'a list

(** {5 Misc utilities } *)
val bit_rep_size : int -> int (**number of bits used to represent n in base 2*)
val pow : int -> int -> int
val pow64 : Int64.t -> Int64.t -> Int64.t
val read_input : unit -> string
val replace_space : string -> string
