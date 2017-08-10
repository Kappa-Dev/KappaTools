(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Compiled algebraic expression *)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR
                  | TMAX_VAR | EMAX_VAR
type bin_bool_op = AND | OR
type un_bool_op = NOT
type compare_op = GREATER | SMALLER | EQUAL | DIFF

(** {6 Printers} *)

val print_bin_alg_op : Format.formatter -> bin_alg_op -> unit
val print_un_alg_op : Format.formatter -> un_alg_op -> unit
val print_state_alg_op : Format.formatter -> state_alg_op -> unit
val print_bin_bool_op : Format.formatter -> bin_bool_op -> unit
val print_un_bool_op : Format.formatter -> un_bool_op -> unit
val print_compare_op : Format.formatter -> compare_op -> unit

(** {6 Json } *)

val bin_alg_op_to_json : bin_alg_op -> Yojson.Basic.json
val bin_alg_op_of_json : Yojson.Basic.json -> bin_alg_op
val un_alg_op_to_json : un_alg_op -> Yojson.Basic.json
val un_alg_op_of_json : Yojson.Basic.json -> un_alg_op
val state_alg_op_to_json : state_alg_op -> Yojson.Basic.json
val state_alg_op_of_json : Yojson.Basic.json -> state_alg_op
val bin_bool_op_to_json : bin_bool_op -> Yojson.Basic.json
val bin_bool_op_of_json : Yojson.Basic.json -> bin_bool_op
val un_bool_op_to_json : un_bool_op -> Yojson.Basic.json
val un_bool_op_of_json : Yojson.Basic.json -> un_bool_op
val compare_op_to_json : compare_op -> Yojson.Basic.json
val compare_op_of_json : Yojson.Basic.json -> compare_op

(** {6 Dependencies management} *)

type rev_dep = ALG of int | RULE of int | PERT of int
module DepSet : SetMap.Set with type elt = rev_dep
val print_rev_dep : Format.formatter -> rev_dep -> unit
val rev_dep_to_yojson : rev_dep -> Yojson.Basic.json
val rev_dep_of_yojson : Yojson.Basic.json -> rev_dep

val depset_to_yojson : DepSet.t -> Yojson.Basic.json
val depset_of_yojson : Yojson.Basic.json -> DepSet.t
