(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'a with_agent_counters = {
  agent: 'a;
  counters: (Ast.counter * LKappa.switching) option array;
}
(** [with_agent_counters] annotates a agent type with rule agent counters if relevant. Used mainly with [rule_agent] and [Raw_mixture.agent].
 *
 * Usually in the code a trailing underscore in a variable name marks that we have this data added, such as [rule_agent_] with [rule_agent_.agent] being a [rule_agent] *)

type rule_mixture_with_agent_counters =
  LKappa.rule_agent with_agent_counters list

type raw_mixture_with_agent_counters =
  Raw_mixture.agent with_agent_counters list

val has_counters : Ast.parsing_compil -> bool

val split_counter_variables_into_separate_rules :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  debug_mode:bool ->
  Ast.parsing_compil ->
  Ast.parsing_compil

val make_counter : int -> string -> Ast.counter

val compile_counter_in_rule :
  Signature.s ->
  LKappa.rule_agent with_agent_counters list ->
  Raw_mixture.agent with_agent_counters list ->
  LKappa.rule_agent list * Raw_mixture.agent list

val counters_perturbations :
  Signature.s ->
  Ast.mixture ->
  (LKappa.rule_mixture, Raw_mixture.t, int, LKappa.rule) Ast.perturbation list

val annotate_dropped_counters :
  Signature.t ->
  Ast.counter list ->
  LKappa.rule_agent ->
  int ->
  string ->
  (int -> unit) option ->
  LKappa.rule_agent with_agent_counters

val annotate_edit_counters :
  Signature.s ->
  string Loc.annoted ->
  Ast.counter list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent with_agent_counters

val annotate_created_counters :
  Signature.s ->
  string Loc.annoted ->
  Ast.counter list ->
  (int -> int -> int -> int -> unit) ->
  Raw_mixture.agent ->
  Raw_mixture.agent with_agent_counters

val annotate_counters_with_diff :
  Signature.s ->
  string Loc.annoted ->
  Ast.counter list ->
  Ast.counter list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent with_agent_counters

val add_counter_to_contact_map :
  Signature.s -> (int -> int -> int -> int -> unit) -> unit
