(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Printers (user readable) of Kappa compiled units *)

val alg_expr :
  noCounters:bool ->
  ?env:Model.t ->
  Format.formatter ->
  Primitives.alg_expr ->
  unit

val bool_expr :
  noCounters:bool ->
  ?env:Model.t ->
  Format.formatter ->
  (Pattern.id array list, int) Alg_expr.bool ->
  unit

val print_expr_val :
  ('a -> Nbr.t) -> Format.formatter -> 'a Primitives.print_expr list -> unit

val elementary_rule :
  noCounters:bool ->
  ?env:Model.t ->
  Format.formatter ->
  Primitives.elementary_rule ->
  unit

val decompiled_rule :
  noCounters:bool ->
  full:bool ->
  Model.t ->
  Format.formatter ->
  Primitives.elementary_rule ->
  unit

val modification :
  noCounters:bool ->
  ?env:Model.t ->
  Format.formatter ->
  Primitives.modification ->
  unit

val perturbation :
  noCounters:bool ->
  ?env:Model.t ->
  Format.formatter ->
  Primitives.perturbation ->
  unit

val env : noCounters:bool -> Format.formatter -> Model.t -> unit
val env_kappa : noCounters:bool -> Format.formatter -> Model.t -> unit
val decompiled_env : noCounters:bool -> Format.formatter -> Model.t -> unit
