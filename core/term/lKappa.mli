(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Intermediate representation of model on wich sanity has been checked *)

type ('a, 'annot) link =
  | ANY_FREE
  | LNK_VALUE of int * 'annot
  | LNK_FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a * 'a  (** port * agent_type *)

type switching = Linked of int | Freed | Maintained | Erased

type rule_internal =
  (*state*)
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int

type rule_agent = {
  ra_type: int; (*agent_id*)
  ra_erased: bool;
  ra_ports: ((int, int * int) link Locality.annot * switching) array;
  (*((link nb, (dst_site,dst_ag_type)), _) , switch*)
  ra_ints: rule_internal array;
  ra_syntax:
    (((int, int * int) link Locality.annot * switching) array
    * rule_internal array)
    option;
}
(** A representation of 'left-hand-side' agent that stores how
 everything is transformed. In an observable (a mixture in an alg_expr),
 everything is [Maintained] (represented by [I_VAL_CHANGED (i,i)] for
 internal states).

The field ra_syntax represents how the user describe the agent
before compilation. Therefore, [compil_of_ast] in this module
generates rule_agent where ra_syntax is [Some (Array.copy ra_ports,
Array.copy ra_ints)]. *)

type rule_mixture = rule_agent list

val forbid_modification : Locality.t -> 'a option -> unit
val several_internal_states : Locality.t -> 'a

val not_enough_specified :
  status:string -> side:string -> string -> string * Locality.t -> 'a

val several_occurence_of_site : string -> string * Locality.t -> 'a
val counter_misused : string -> string * Locality.t -> 'a
val link_only_one_occurence : int -> Locality.t -> 'a
val link_should_be_removed : int -> string -> string * Locality.t -> 'a
val agent_to_erased : Signature.s -> rule_agent -> rule_agent
val to_erased : Signature.s -> rule_mixture -> rule_mixture
val to_maintained : rule_mixture -> rule_mixture
val to_raw_mixture : Signature.s -> rule_mixture -> Raw_mixture.t
val copy_rule_agent : rule_agent -> rule_agent

val print_rule_mixture :
  noCounters:bool ->
  Signature.s ->
  ltypes:bool ->
  Raw_mixture.t ->
  Format.formatter ->
  rule_mixture ->
  unit

type rule = {
  r_mix: rule_mixture;
  r_created: Raw_mixture.t;
  r_delta_tokens: ((rule_mixture, int) Alg_expr.e Locality.annot * int) list;
  r_rate: (rule_mixture, int) Alg_expr.e Locality.annot;
  r_un_rate:
    ((rule_mixture, int) Alg_expr.e Locality.annot
    * (rule_mixture, int) Alg_expr.e Locality.annot option)
    option;
  r_editStyle: bool;
}

val print_link :
  ('a -> Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) link ->
  unit

val link_to_json :
  ('a -> 'a -> Yojson.Basic.t) ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t list) ->
  ('a, 'b) link ->
  Yojson.Basic.t
(** Fragile: the list MUST NOT be a singleton *)

val link_of_json :
  ('a -> Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t list -> 'b) ->
  Yojson.Basic.t ->
  ('a, 'b) link

val print_rates :
  noCounters:bool ->
  Signature.s ->
  (Format.formatter -> int -> unit) ->
  (Format.formatter -> int -> unit) ->
  Format.formatter ->
  rule ->
  unit

val print_rule :
  noCounters:bool ->
  full:bool ->
  Signature.s ->
  (Format.formatter -> int -> unit) ->
  (Format.formatter -> int -> unit) ->
  Format.formatter ->
  rule ->
  unit

val rule_to_json : filenames:int Mods.StringMap.t -> rule -> Yojson.Basic.t
val rule_of_json : filenames:string array -> Yojson.Basic.t -> rule
val max_link_id : rule_mixture -> int
