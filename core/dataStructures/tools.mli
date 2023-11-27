(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Misc utilities *)

(** {5 Combinators on primitive types} *)

val iteri : (int -> unit) -> int -> unit

val recti : ('a -> int -> 'a) -> 'a -> int -> 'a
(** [recti f x n] = f (f (f .. (f (f x 0) 1) ..) (n-1) *)

val array_compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int
val array_map_of_list : ('a -> 'b) -> 'a list -> 'b array
val array_rev_map_of_list : ('a -> 'b) -> 'a list -> 'b array
val array_rev_of_list : 'a list -> 'a array
val array_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val array_fold_righti : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b
val array_filter : (int -> 'a -> bool) -> 'a array -> int list

val array_fold_left_mapi :
  (int -> 'a -> 'b -> 'a * 'c) -> 'a -> 'b array -> 'a * 'c array

val array_fold_left2i :
  (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

val array_min_equal_not_null :
  (int * 'a list) array -> (int * 'b list) array -> ('a list * 'b list) option

(** {5 Misc utilities } *)

val float_is_zero : float -> bool
val pow : int -> int -> int
val pow64 : Int64.t -> Int64.t -> Int64.t
val not_an_id : string -> bool
val read_input : unit -> string
val min_pos_int_not_zero : int * 'a -> int * 'a -> int * 'a
val max_pos_int_not_zero : int * 'a -> int * 'a -> int * 'a
val fold_over_permutations : (int list -> 'a -> 'a) -> int list -> 'a -> 'a
val gcd_2 : int -> int -> int
val lcm : int list -> int
val fact : int -> int
val cantor_pairing : int -> int -> int

val get_product_image_occ :
  int -> (int -> int -> int) -> (int -> int) -> 'a list -> int

val get_product_image_occ_2 :
  int -> (int -> int -> int) -> (int -> int -> int) -> 'a list -> 'a list -> int

val get_interval_list : (int -> bool) -> int -> int -> (int * int) list
val string_split_on_char : char -> string -> string * string option
val lowercase : string -> string
val capitalize : string -> string

val smash_duplicate_in_ordered_list :
  ('a -> 'a -> int) -> ('a * int) list -> ('a * int) list

val chop_suffix_or_extension : string -> string -> string

val find_available_name :
  already_there:(string -> bool) ->
  string ->
  facultative:string ->
  ext:string option ->
  string

val default_message_delimter : char
val get_ref : int ref -> int
val remove_double_elements : 'a list -> 'a list
val sort_by_priority : ('a -> int) -> int -> 'a list -> 'a list
