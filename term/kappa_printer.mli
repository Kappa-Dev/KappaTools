(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Printers (user readable) of Kappa compiled units *)

val alg_expr : ?env:Model.t -> Format.formatter -> Primitives.alg_expr -> unit

val bool_expr : ?env:Model.t -> Format.formatter ->
  (Pattern.id array list,int) Alg_expr.bool -> unit

val print_expr_val :
  ('a -> Nbr.t) -> Format.formatter -> 'a Primitives.print_expr list -> unit

val elementary_rule :
  ?env:Model.t -> Format.formatter -> Primitives.elementary_rule -> unit

val modification :
  ?env:Model.t -> Format.formatter -> Primitives.modification -> unit

val perturbation :
  ?env:Model.t -> Format.formatter -> Primitives.perturbation -> unit

val env : Format.formatter -> Model.t -> unit

val env_kappa : Contact_map.t -> Format.formatter -> Model.t -> unit
