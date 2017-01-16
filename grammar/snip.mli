(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Kappa pattern compiler *)

val connected_components_sum_of_ambiguous_mixture :
  Signature.contact_map -> Pattern.PreEnv.t -> ?origin:Operator.rev_dep ->
  LKappa.rule_mixture ->
  Pattern.PreEnv.t *
  (Pattern.id array * Instantiation.abstract Instantiation.test list list) list

val connected_components_sum_of_ambiguous_rule :
  Signature.contact_map -> Pattern.PreEnv.t -> ?origin:Operator.rev_dep ->
  LKappa.rule_mixture -> Raw_mixture.t ->
  (Operator.rev_dep option * Pattern.id array *
     (Instantiation.abstract Instantiation.event) *
       (Instantiation.abstract Primitives.Transformation.t list *
          Instantiation.abstract Primitives.Transformation.t list))
    list *   (Pattern.PreEnv.t * Operator.rev_dep option)
