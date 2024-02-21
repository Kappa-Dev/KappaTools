(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val last : 'a list -> 'a
val pop_last : 'a list -> 'a list * 'a
val cons_option : 'a option -> 'a list -> 'a list
val exists_uniq : ('a -> bool) -> 'a list -> bool
val find_option : ('a -> bool) -> 'a list -> 'a option
val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
val fold_right_map : ('a -> 'b -> 'c * 'b) -> 'a list -> 'b -> 'c list * 'b
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val map_option : ('a -> 'b option) -> 'a list -> 'b list
val random : Random.State.t -> 'a list -> 'a

val merge_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Set union of 2 sorted list
  When l1 is included in l2:
  [merge_uniq l1 l2 == merge_uniq l2 l1 == l2] *)

val smart_filter : ('a -> bool) -> 'a list -> 'a list
(** not tail rec but don't allocate if unecessary *)

val smart_map : ('a -> 'a) -> 'a list -> 'a list
val remove_suffix_after_last_occurrence : ('a -> bool) -> 'a list -> 'a list
val remove_consecutive_double : 'a list -> 'a list

module Infix : sig
  val ( $$ ) : 'a option -> 'a list -> 'a list
  (** [cons_option] *)
end
