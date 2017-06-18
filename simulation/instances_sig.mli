(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module type S = sig
  type t

  val empty : Model.t -> t

  val debug_print : Format.formatter -> t -> unit

  val incorporate_extra_pattern : t -> Pattern.id -> IntCollection.t -> unit

  (* returns the rectangular approximated quantity of instances of an
     observable *)
  val number : t -> Pattern.id array -> int
  (** Can be used *)

  (* Redistrubute instances per connected component whien the
     connectivity changes

     None means connectivity has not changed (aka nothing to do)

     The hash table is there for delayed update of activity. It stores all
     the connected component that have changed during this event
     loop.*)
  val break_apart_cc :
    t -> Edges.t -> (int, unit) Hashtbl.t -> (int * int) option -> t
  val merge_cc :
    t -> (int, unit) Hashtbl.t -> (int * int) option -> t

  val update_roots :
    t -> bool -> Pattern.Set.t -> Edges.t -> (int, unit) Hashtbl.t ->
    Pattern.id -> int -> unit
  (** [update_roots state is_positive_update domain graph cc_cache id root] *)

  val all_injections :
    ?excp:Pattern.id * int -> ?unary_rate:'a * int option -> t ->
    Pattern.Env.t -> Edges.t -> Pattern.id array -> (Matching.t * int list) list
  (** ~excp(pattern,root) constraints [pattern] to be injected only on
      [root].  Only the horizon part (the int option one) is important
      of unary_rate returns the matching into the mixture and the
      roots of the patterns (for conviencience to check binary
      instances of rules with molecular ambiguity) *)

  val adjust_rule_instances :
    rule_id:int -> ?unary_rate:'a * int option ->
    t -> Pattern.Env.t -> Edges.t -> Pattern.id array -> int * t
  (** returns the Instances.t where you've stored the exact (binary when
      unary_rate is Some) matchings and the number of these
      matchings *)

  val adjust_unary_rule_instances :
    rule_id:int -> ?max_distance:int -> t -> Pattern.Env.t -> Edges.t ->
    Pattern.id array -> int * t
  (** returns the Instances.t where you've stored the exact unary
      matchings and the number of these matchings *)

  val compute_unary_number :
    t -> (int, unit) Hashtbl.t -> Primitives.elementary_rule -> int -> int * t
  (** [compute_unary_number state modified_ccs_cache rule cc] *)

  val pop_exact_matchings : t -> int -> t
  (** exact matchings of [rule_id] are not valid anymore *)

  val pick_an_instance :
    t -> Random.State.t -> Pattern.Env.t -> Edges.t -> ?rule_id:int ->
    Primitives.elementary_rule -> (Matching.t * int list) option

  val pick_an_unary_instance :
    t -> Random.State.t -> Pattern.Env.t -> Edges.t -> rule_id:int ->
    Primitives.elementary_rule -> Matching.t option * Edges.path option

end