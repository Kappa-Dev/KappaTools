(** {5 Combinators on primitive types *)
val option_map : ('a -> 'b) -> 'a option -> 'b option
val iteri : (int -> unit) -> int -> unit
val recti : (int -> 'a -> 'a) -> 'a -> int -> 'a
(** [recti f n x] = f 0 (f 1 (.. (f (n-1) x))) *)

val array_map_of_list : ('a -> 'b) -> 'a list -> 'b array
val array_rev_of_list : 'a list -> 'a array
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
  ('a -> 'b -> 'c * 'b) -> 'a list -> 'b -> 'c list * 'b
val list_fold_left2 :
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val list_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val list_random : 'a list -> 'a

(** not tail rec but don't allocate if unecessary *)
val list_smart_filter : ('a -> bool) -> 'a list -> 'a list
val list_smart_map : ('a -> 'a) -> 'a list -> 'a list

(** {5 Misc utilities } *)
val pow : int -> int -> int
val pow64 : Int64.t -> Int64.t -> Int64.t
val read_input : unit -> string
val min_pos_int_not_zero: (int*'a) -> (int*'a) -> (int*'a)
val max_pos_int_not_zero: (int*'a) -> (int*'a) -> (int*'a)
