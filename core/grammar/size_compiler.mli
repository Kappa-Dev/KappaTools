(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type size_predicate_site = int * Loc.t
type 'a with_size_predicates = { agent: 'a }

type rule_mixture_with_size_predicates =
  LKappa.rule_agent with_size_predicates list

type raw_mixture_with_size_predicates =
  Raw_mixture.agent with_size_predicates list

val has_size_predicates : Ast.parsing_compil -> bool

val compile_size_predicate_in_rule :
  Signature.s ->
  Size_info.size_sig option array array ->
  LKappa.rule_agent with_size_predicates list ->
  Raw_mixture.agent with_size_predicates list ->
  size_predicate_site option ->
  LKappa.rule_agent list * Raw_mixture.agent list

val annotate_dropped_size_predicates :
  Signature.t ->
  Ast.threshold list ->
  LKappa.rule_agent ->
  int ->
  string ->
  (int -> unit) option ->
  LKappa.rule_agent with_size_predicates

val annotate_edit_size_predicates :
  Signature.s ->
  string Loc.annoted ->
  Ast.threshold list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent with_size_predicates

val annotate_created_size_predicates :
  Signature.s ->
  string Loc.annoted ->
  Ast.threshold list ->
  (int -> int -> int -> int -> unit) ->
  Raw_mixture.agent ->
  Raw_mixture.agent with_size_predicates

val annotate_size_predicates_with_diff :
  Signature.s ->
  string Loc.annoted ->
  Ast.threshold list ->
  Ast.threshold list ->
  LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent with_size_predicates

(*val add_size_predicates_to_contact_map :
  Signature.s -> (int -> int -> int -> int -> unit) -> unit*)
