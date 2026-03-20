(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Primitives for handling rule rates when detecting symmetries *)

val divide_expr_by_int :
  ('mix, 'id) Alg_expr.e Loc.annoted ->
  int ->
  ('mix, 'id) Alg_expr.e Loc.annoted
(* Partial normal form for expressions *)
(* We only deal with constant, single alg_var multiplied/divided by a constant,
   sum of two expr either both constant or dealing with the same alg_var *)
(* I think this is enough to deal with symmetries *)
(* We may be more complete later *)

val simplify :
  ('mix, 'id) Alg_expr.e Loc.annoted -> ('mix, 'id) Alg_expr.e Loc.annoted

type ('mix, 'id) corrected_rate_const

(* printer *)
val print :
  (Format.formatter -> ('mix, 'id) Alg_expr.e Loc.annoted option -> unit) ->
  Format.formatter ->
  ('mix, 'id) corrected_rate_const option ->
  unit

(* conversion *)
val get_corrected_rate :
  ('mix, 'id) Alg_expr.e Loc.annoted -> ('mix, 'id) corrected_rate_const option

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
  ('mix, 'id) Alg_expr.e Loc.annoted ->
  'set

val diff_token :
  ('mix, 'id) Alg_expr.e Loc.annoted ->
  'id ->
  ('mix, 'id) Alg_expr.e Loc.annoted

val diff_mixture :
  ?time_var:'mix ->
  ('mix, 'id) Alg_expr.e Loc.annoted ->
  'mix ->
  ('mix, 'id) Alg_expr.e Loc.annoted

val fold_over_mixtures_in_alg_exprs :
  (Pattern.id -> 'a -> 'a) -> Model.t -> 'a -> 'a

val rename_pos_alg_expr: 
((Loc.t -> Loc.t option) -> 'a -> 'a) -> 
((Loc.t -> Loc.t option) -> 'b -> 'b) -> 
(Loc.t -> Loc.t option) -> ('a,'b) Alg_expr.e -> ('a,'b) Alg_expr.e 

val rename_pos_bool: 
((Loc.t -> Loc.t option) -> 'a -> 'a) -> 
((Loc.t -> Loc.t option) -> 'b -> 'b) -> 
(Loc.t -> Loc.t option) -> ('a,'b) Alg_expr.bool -> ('a,'b) Alg_expr.bool


val rename_pos_alg_expr_with_errors: 
('parameters -> 'errors -> (Loc.t -> Loc.t option) -> 'a -> 'errors * 'a) -> 
('parameters -> 'errors -> (Loc.t -> Loc.t option) -> 'b -> 'errors * 'b) -> 
'parameters -> 'errors -> (Loc.t -> Loc.t option) -> ('a,'b) Alg_expr.e -> 'errors * ('a,'b) Alg_expr.e 

val rename_pos_bool_with_errors: 
('parameters -> 'errors -> (Loc.t -> Loc.t option) -> 'a -> 'errors * 'a) -> 
('parameters -> 'errors -> (Loc.t -> Loc.t option) -> 'b -> 'errors * 'b) -> 
'parameters -> 'errors -> (Loc.t -> Loc.t option) -> ('a,'b) Alg_expr.bool -> 'errors * ('a,'b) Alg_expr.bool