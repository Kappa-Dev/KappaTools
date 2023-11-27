(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Primitives for handling rule rates when detecting symmetries *)

val divide_expr_by_int :
  ('mix, 'id) Alg_expr.e Locality.annot ->
  int ->
  ('mix, 'id) Alg_expr.e Locality.annot
(* Partial normal form for expressions *)
(* We only deal with constant, single alg_var multiplied/divided by a constant,
   sum of two expr either both constant or dealing with the same alg_var *)
(* I think this is enough to deal with symmetries *)
(* We may be more complete later *)

val simplify :
  ('mix, 'id) Alg_expr.e Locality.annot -> ('mix, 'id) Alg_expr.e Locality.annot

type ('mix, 'id) corrected_rate_const

(* printer *)
val print :
  (Format.formatter -> ('mix, 'id) Alg_expr.e Locality.annot option -> unit) ->
  Format.formatter ->
  ('mix, 'id) corrected_rate_const option ->
  unit

(* conversion *)
val get_corrected_rate :
  ('mix, 'id) Alg_expr.e Locality.annot ->
  ('mix, 'id) corrected_rate_const option

(* partial equality test *)
(* true means "yes they are equal" *)
(* false means "either equal, or not"*)

val necessarily_equal :
  ('mix, 'id) corrected_rate_const option ->
  ('mix, 'id) corrected_rate_const option ->
  bool

(** derivation *)

val dep :
  'set ->
  ('mix -> 'set -> 'set) ->
  ('id -> 'set -> 'set) ->
  ('set -> 'set -> 'set) ->
  ('id -> 'set) ->
  ?time_var:'mix ->
  ('mix, 'id) Alg_expr.e Locality.annot ->
  'set

val diff_token :
  ('mix, 'id) Alg_expr.e Locality.annot ->
  'id ->
  ('mix, 'id) Alg_expr.e Locality.annot

val diff_mixture :
  ?time_var:'mix ->
  ('mix, 'id) Alg_expr.e Locality.annot ->
  'mix ->
  ('mix, 'id) Alg_expr.e Locality.annot

val fold_over_mixtures_in_alg_exprs :
  (Pattern.id -> 'a -> 'a) -> Model.t -> 'a -> 'a
