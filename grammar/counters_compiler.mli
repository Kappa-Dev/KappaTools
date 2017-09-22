(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val compile : Ast.parsing_compil -> Ast.parsing_compil * bool

val incr_agent : Signature.s -> int * int * int * int

val remove_counter_rule :
  Signature.s -> LKappa.rule_agent LKappa.rule_agent_counters list ->
  Raw_mixture.agent LKappa.rule_agent_counters list ->
  LKappa.rule_agent list *  Raw_mixture.agent list

val counters_perturbations :
  Signature.s -> Ast.agent list ->
  (LKappa.rule_agent list, Raw_mixture.agent list, int) Ast.perturbation list

val annotate_dropped_counters :
  Signature.t -> Ast.counter list ->  LKappa.rule_agent -> int -> string ->
  (int -> unit) option -> LKappa.rule_agent LKappa.rule_agent_counters

val annotate_edit_counters :
  Signature.s -> string * Locality.t -> Ast.counter list -> LKappa.rule_agent ->
  (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent LKappa.rule_agent_counters

val annotate_created_counters :
  Signature.s -> string * Locality.t -> Ast.counter list ->
  (int -> int -> int -> int -> unit) -> Raw_mixture.agent ->
  Raw_mixture.agent LKappa.rule_agent_counters

val annotate_counters_with_diff :
  Signature.s -> string Locality.annot -> Ast.counter list -> Ast.counter list ->
  LKappa.rule_agent -> (int -> int -> int -> int -> unit) ->
  LKappa.rule_agent LKappa.rule_agent_counters

val add_incr :
  (string Locality.annot * string Locality.annot list) list ->
  string Locality.annot *
    (unit NamedDecls.t *
       ((string Locality.annot * string Locality.annot) list) *
         (int * int) option) NamedDecls.t

val add_counter_to_contact_map :
  Signature.s -> (int -> int -> int -> int -> unit) -> unit
