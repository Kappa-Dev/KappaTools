(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Algebraic expression computation *)

(** As soon as you've got an graph available, I'll probably prefer use
{!module:Rule_interpreter}.value_* *)

val value_alg :
  Counter.t ->
  ?time:float ->
  get_alg:(int -> Primitives.alg_expr) ->
  get_mix:(Pattern.id array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) ->
  Primitives.alg_expr ->
  Nbr.t
(** [value_alg c ?t get_alg get_mix get_tok expr_alg] with [get_mix [interp1;...;interpn]] *)

val value_bool :
  Counter.t ->
  ?time:float ->
  get_alg:(int -> Primitives.alg_expr) ->
  get_mix:(Pattern.id array list -> Nbr.t) ->
  get_tok:(int -> Nbr.t) ->
  (Pattern.id array list, int) Alg_expr.bool ->
  bool
