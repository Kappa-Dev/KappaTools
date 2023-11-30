(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'a rule_agent_counters = {
  ra: 'a;
  ra_counters: (Ast.counter * LKappa.switching) option array;
}

val compile :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool ->
  Ast.parsing_compil ->
  Ast.parsing_compil * bool

val make_counter : int -> string -> Ast.counter

val remove_counter_rule :
  Signature.s ->
  LKappa.rule_agent rule_agent_counters list ->
  Raw_mixture.agent rule_agent_counters list ->
  LKappa.rule_agent list * Raw_mixture.agent list

val counters_perturbations :
  Signature.s ->
  Ast.mixture ->
  ( LKappa.rule_mixture,
    Raw_mixture.agent list,
    int,
    LKappa.rule )
  Ast.perturbation
  list

val annotate_dropped_counters :
  Signature.t ->
  Ast.counter list ->
  LKappa.rule_agent ->
  int ->
  string ->
  (int -> unit) option ->
  LKappa.rule_agent rule_agent_counters

val annotate_edit_counters :
  Signature.s ->
  string * Locality.t ->
  Ast.counter list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent rule_agent_counters

val annotate_created_counters :
  Signature.s ->
  string * Locality.t ->
  Ast.counter list ->
  (int -> int -> int -> int -> unit) ->
  Raw_mixture.agent ->
  Raw_mixture.agent rule_agent_counters

val annotate_counters_with_diff :
  Signature.s ->
  string Locality.annot ->
  Ast.counter list ->
  Ast.counter list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent rule_agent_counters

val add_counter_to_contact_map :
  Signature.s -> (int -> int -> int -> int -> unit) -> unit
