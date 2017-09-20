(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val compile : Ast.parsing_compil -> Ast.parsing_compil * bool

val incr_agent : Signature.s -> int * int * int * int
val agent_with_counters : string * Locality.t -> Signature.s -> bool

val remove_counter_rule :
  Signature.s -> bool -> LKappa.rule_agent_counters list ->
  Raw_mixture.agent list ->
  LKappa.rule_agent list *  Raw_mixture.agent list

val counters_perturbations :
  Signature.s -> Ast.agent list ->
  (LKappa.rule_agent list, Raw_mixture.agent list, int) Ast.perturbation list

val counters_rules :
  Signature.s -> bool ->
  (string Locality.annot option *
     LKappa.rule_agent_counters LKappa.rule Locality.annot) list ->
  (string Locality.annot option *
     LKappa.rule_agent LKappa.rule Locality.annot) list
