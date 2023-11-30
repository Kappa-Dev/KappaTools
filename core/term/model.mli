(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Compiled representation of a full Kappa model *)

type t

val init :
  filenames:string list ->
  Pattern.Env.t ->
  unit NamedDecls.t ->
  Primitives.alg_expr Loc.annoted NamedDecls.t ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array ->
  (string Loc.annoted option * LKappa.rule Loc.annoted) array
  * Primitives.elementary_rule array ->
  Primitives.alg_expr Loc.annoted array ->
  Primitives.perturbation array ->
  Contact_map.t ->
  t
(** [init sigs tokens algs dependencies (ast_rules,rules) obs perts]
 *)

val deconstruct :
  t ->
  string list
  * Pattern.Env.t
  * unit NamedDecls.t
  * Primitives.alg_expr Loc.annoted NamedDecls.t
  * (Operator.DepSet.t
    * Operator.DepSet.t
    * Operator.DepSet.t array
    * Operator.DepSet.t array)
  * ((string Loc.annoted option * LKappa.rule Loc.annoted) array
    * Primitives.elementary_rule array)
  * Primitives.alg_expr Loc.annoted array
  * Primitives.perturbation array
  * Contact_map.t

val nb_tokens : t -> int
val nb_algs : t -> int
val nb_rules : t -> int
val nb_syntactic_rules : t -> int
val nb_perturbations : t -> int
val domain : t -> Pattern.Env.t
val get_obs : t -> Primitives.alg_expr Loc.annoted array
val get_rules : t -> Primitives.elementary_rule array
val new_domain : Pattern.Env.t -> t -> t
val signatures : t -> Signature.s
val tokens_finder : t -> int Mods.StringMap.t
val algs_finder : t -> int Mods.StringMap.t
val contact_map : t -> Contact_map.t
val get_alg : t -> int -> Primitives.alg_expr
val get_algs : t -> (string * Primitives.alg_expr Loc.annoted) array
val get_perturbation : t -> int -> Primitives.perturbation
val get_rule : t -> int -> Primitives.elementary_rule
val get_ast_rule : t -> int -> LKappa.rule

val get_ast_rule_with_label :
  t -> int -> string Loc.annoted option * LKappa.rule Loc.annoted

val get_ast_rule_rate_pos : unary:bool -> t -> int -> Loc.t
val map_observables : (Primitives.alg_expr -> 'a) -> t -> 'a array

val fold_rules :
  (int -> 'a -> Primitives.elementary_rule -> 'a) -> 'a -> t -> 'a

val fold_ast_rules : (int -> 'a -> LKappa.rule -> 'a) -> 'a -> t -> 'a

val fold_perturbations :
  (int -> 'a -> Primitives.perturbation -> 'a) -> 'a -> t -> 'a

val get_alg_reverse_dependencies : t -> int -> Operator.DepSet.t
val get_token_reverse_dependencies : t -> int -> Operator.DepSet.t

val all_dependencies :
  t ->
  Operator.DepSet.t
  * Operator.DepSet.t
  * Operator.DepSet.t array
  * Operator.DepSet.t array

val num_of_agent : string Loc.annoted -> t -> int
val num_of_alg : string Loc.annoted -> t -> int
val num_of_token : string Loc.annoted -> t -> int
val nums_of_rule : string -> t -> int list

val print_ast_rule :
  noCounters:bool -> ?env:t -> Format.formatter -> int -> unit
(** The int is the ast_rule_id *)

val print_rule : noCounters:bool -> ?env:t -> Format.formatter -> int -> unit
(** Same as above but the int is this time the rule_id *)

val print_agent : ?env:t -> Format.formatter -> int -> unit
val print_alg : ?env:t -> Format.formatter -> int -> unit
val print_token : ?env:t -> Format.formatter -> int -> unit

val print :
  noCounters:bool ->
  (t -> Format.formatter -> Primitives.alg_expr -> unit) ->
  (t -> Format.formatter -> Primitives.elementary_rule -> unit) ->
  (t -> Format.formatter -> Primitives.perturbation -> unit) ->
  Format.formatter ->
  t ->
  unit

val print_kappa :
  noCounters:bool ->
  (t -> Format.formatter -> Primitives.alg_expr -> unit) ->
  ?pr_rule:(t -> Format.formatter -> Primitives.elementary_rule -> unit) ->
  (t -> Format.formatter -> Primitives.perturbation -> unit) ->
  Format.formatter ->
  t ->
  unit

val to_yojson : t -> Yojson.Basic.t
val of_yojson : Yojson.Basic.t -> t
val check_if_counter_is_filled_enough : t -> unit
val overwrite_vars : (int * Primitives.alg_expr) list -> t -> t

val propagate_constant :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  ?max_time:float ->
  ?max_events:int ->
  updated_vars:int list ->
  alg_overwrite:(int * Primitives.alg_expr) list ->
  t ->
  t
(** [propagate_constant updated_vars overwrite_vars env] *)

val fold_mixture_in_expr : ('a -> Pattern.id array list -> 'a) -> 'a -> t -> 'a
val unary_patterns : t -> Pattern.Set.t
