(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Intermediate representation of model on wich sanity has been checked *)

type switching =
  | Linked of int Locality.annot | Freed | Maintained | Erased

type rule_internal = (*state*)
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int

type rule_agent =
  { ra_type: int; (*agent_id*)
    ra_erased: bool;
    ra_ports: ((int,int*int) Ast.link Locality.annot * switching) array;
    (*(state, _) , switch*)
    ra_ints: rule_internal array;
    ra_syntax: (((int,int*int) Ast.link Locality.annot * switching) array *
                rule_internal array) option;
  }
(** A representation of 'left-hand-side' agent that stores how
 everything is transformed. In an observable (a mixture in an alg_expr),
 everything is [Maintained] (represented by [I_VAL_CHANGED (i,i)] for
 internal states).

The field ra_syntax represents how the user describe the agent
before compilation. Therefore, [compil_of_ast] in this module
generates rule_agent where ra_syntax is [Some (Array.copy ra_ports,
Array.copy ra_ints)]. *)

type rule_mixture = rule_agent list

val to_erased : Signature.s -> rule_mixture -> rule_mixture
val to_maintained : rule_mixture -> rule_mixture
val to_raw_mixture : Signature.s -> rule_mixture -> Raw_mixture.t
val of_raw_mixture : Raw_mixture.t -> rule_mixture
val copy_rule_agent : rule_agent -> rule_agent
val print_rule_mixture :
  Signature.s -> ltypes:bool -> Format.formatter -> rule_agent list -> unit

type rule =
  { r_mix: rule_mixture;
    r_created: Raw_mixture.t;
    r_delta_tokens :
      ((rule_mixture,int) Alg_expr.e Locality.annot * int) list;
    r_rate : (rule_mixture,int) Alg_expr.e Locality.annot;
    r_un_rate : ((rule_mixture,int) Alg_expr.e Locality.annot
                 * (rule_mixture,int) Alg_expr.e Locality.annot option) option;
  }

val print_intf_lhs :
  ltypes:bool -> Signature.s ->
  int -> Format.formatter ->
  (((int, int * int) Ast.link Locality.annot * switching) array *
   rule_internal array) -> unit

val get_agent_lhs : rule_agent ->
  int *
  (((int, int * int) Ast.link Locality.annot * switching) array *
   rule_internal array)

val print_agent_lhs :
  ltypes:bool -> Signature.s -> Format.formatter ->
  rule_agent -> unit

val print_agent_rhs :
  ltypes:bool -> Signature.s -> Format.formatter ->
  rule_agent -> unit

val print_rule_agent :
  Signature.s -> ltypes:bool -> Format.formatter ->
  rule_agent -> unit

val print_rhs :
  ltypes:bool ->
  Signature.s ->
  Raw_mixture.t -> Format.formatter -> rule_agent list -> unit

val print_rates : Signature.s -> (Format.formatter -> int -> unit)
  -> (Format.formatter -> int -> unit) -> Format.formatter -> rule -> unit

val print_rule :
  full:bool -> Signature.s -> (Format.formatter -> int -> unit) ->
  (Format.formatter -> int -> unit) -> Format.formatter -> rule -> unit

val rule_to_json : rule -> Yojson.Basic.json
val rule_of_json : Yojson.Basic.json -> rule

val bool_expr_of_ast :
  Signature.s -> int Mods.StringMap.t -> int Mods.StringMap.t ->
  ?max_allowed_var: int ->
  (Ast.mixture, string) Alg_expr.bool Locality.annot ->
  (rule_agent list, int) Alg_expr.bool Locality.annot

val modif_expr_of_ast :
  Signature.s -> int Mods.StringMap.t -> int Mods.StringMap.t ->
  Contact_map.t -> (Ast.mixture, string) Ast.modif_expr -> int list ->
  (rule_agent list, int) Ast.modif_expr * int list

val compil_of_ast :
  (string * Nbr.t) list -> Ast.parsing_compil ->
  Signature.s * Contact_map.t * unit NamedDecls.t * int list *
  (Ast.agent, rule_agent list, int, rule, unit) Ast.compil
(** [compil_of_ast variable_overwrite ast]

    @return the signature of agent, the contact map, the signature of
    tokens, algebraic variable on which constant propagation is
    forbidden, and an Ast.compil where identifiers are integers and
    not string, syntactic sugar on rules are expansed (syntactic sugar
    on mixture are not)

    This function sorts out longest prefix convention as well as ensure a
    lot of sanity on mixtures:
    - agent exists
    - sites exist
    - unique site occurence / agent
    - internal_states exist
    - unique internal_state / site
    - links appear exactly twice

    The sanity checks on rates consists in ensuring that
    - either absolute or unary rates are provided;
    - if the algebraic expression of the rate contains a mixture then
    a new variable is declared called rulelabel_un_rate; it is
    necessary in the update phase.*)
