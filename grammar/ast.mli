(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Kappa AST just after parsing *)
type syntax_version = V3 | V4

val merge_version : syntax_version -> syntax_version -> syntax_version

type ('a,'annot) link =
  | ANY_FREE
  | LNK_VALUE of int * 'annot
  | LNK_FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a * 'a (** port * agent_type *)

type internal = string option Locality.annot list

type port = {
  port_nme:string Locality.annot;
  port_int:internal;
  port_int_mod: string Locality.annot option;
  port_lnk:(string Locality.annot,unit) link Locality.annot list;
  port_lnk_mod: int Locality.annot option option;
}

type counter_test = CEQ of int | CGTE of int | CVAR of string

type counter = {
  count_nme: string Locality.annot;
  count_test: counter_test Locality.annot option;
  count_delta: int Locality.annot;
}

type site =
  | Port of port
  | Counter of counter

type agent_mod = Erase | Create

type agent =
  | Present of string Locality.annot * site list * agent_mod option
  | Absent of Locality.t

type mixture = agent list

type edit_notation = {
  mix: mixture;
  delta_token: ((mixture,string) Alg_expr.e Locality.annot
                * string Locality.annot) list;
}

type arrow_notation = {
  lhs: mixture ;
  rm_token: ((mixture,string) Alg_expr.e Locality.annot
             * string Locality.annot) list ;
  rhs: mixture ;
  add_token: ((mixture,string) Alg_expr.e Locality.annot
              * string Locality.annot) list;
}

type rule_content = Edit of edit_notation | Arrow of arrow_notation

type rule = {
  rewrite: rule_content;
  bidirectional:bool ;
  k_def: (mixture,string) Alg_expr.e Locality.annot ;
  k_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*k_1:radius_opt*)
  k_op: (mixture,string) Alg_expr.e Locality.annot option ;
  k_op_un:
    ((mixture,string) Alg_expr.e Locality.annot *
     (mixture,string) Alg_expr.e Locality.annot option) option;
  (*rate for backward rule*)
}

val flip_label : string -> string

type ('pattern,'mixture,'id) modif_expr =
  | INTRO of
      (('pattern,'id) Alg_expr.e Locality.annot * 'mixture Locality.annot)
  | DELETE of
      (('pattern,'id) Alg_expr.e Locality.annot * 'pattern Locality.annot)
  | UPDATE of
      ('id Locality.annot * ('pattern,'id) Alg_expr.e Locality.annot)
  (*TODO: pause*)
  | UPDATE_TOK of
      ('id Locality.annot * ('pattern,'id) Alg_expr.e Locality.annot)
  (*TODO: pause*)
  | STOP of ('pattern,'id) Alg_expr.e Primitives.print_expr list
  | SNAPSHOT of ('pattern,'id) Alg_expr.e Primitives.print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      (('pattern,'id) Alg_expr.e Primitives.print_expr list) *
       (('pattern,'id) Alg_expr.e Primitives.print_expr list)
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Locality.annot)
  | CFLOWMIX of (bool * 'pattern Locality.annot)
  | FLUX of
      Primitives.flux_kind * ('pattern,'id) Alg_expr.e Primitives.print_expr list
  | FLUXOFF of ('pattern,'id) Alg_expr.e Primitives.print_expr list
  | SPECIES_OF of
      (bool * ('pattern,'id) Alg_expr.e Primitives.print_expr list
       * 'pattern Locality.annot)

type ('pattern,'mixture,'id) perturbation =
  (Nbr.t option *
   ('pattern,'id) Alg_expr.bool Locality.annot option *
   (('pattern,'mixture,'id) modif_expr list) *
   ('pattern,'id) Alg_expr.bool Locality.annot option)
    Locality.annot

type configuration = string Locality.annot * (string Locality.annot list)

type ('pattern,'id) variable_def =
  string Locality.annot * ('pattern,'id) Alg_expr.e Locality.annot

type ('mixture,'id) init_t =
  | INIT_MIX of 'mixture Locality.annot
  | INIT_TOK of 'id Locality.annot list

type ('pattern,'mixture,'id) init_statment =
  ('pattern,'id) Alg_expr.e Locality.annot * ('mixture,'id) init_t

type ('agent,'pattern,'mixture,'id,'rule) instruction =
  | SIG      of 'agent
  | TOKENSIG of string Locality.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of ('pattern,'mixture,'id) init_statment
  | DECLARE  of ('pattern,'id) variable_def
  | OBS      of ('pattern,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('pattern,'id) Alg_expr.e Locality.annot
  | PERT     of ('pattern,'mixture,'id) perturbation
  | CONFIG   of configuration
  | RULE     of (string Locality.annot option * 'rule Locality.annot)

type ('pattern,'mixture,'id) command =
  | RUN of ('pattern,'id) Alg_expr.bool Locality.annot
  | MODIFY of ('pattern,'mixture,'id) modif_expr list
  | QUIT

type ('agent,'pattern,'mixture,'id,'rule) compil =
  {
    filenames : string list;
    variables :
      ('pattern,'id) variable_def list;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures :
      'agent list; (**agent signature declaration*)
    rules :
      (string Locality.annot option * 'rule Locality.annot) list;
    (**rules (possibly named)*)
    observables : ('pattern,'id) Alg_expr.e Locality.annot list;
    (*list of patterns to plot*)
    init : ('pattern,'mixture,'id) init_statment list;
    (*initial graph declaration*)
    perturbations : ('pattern,'mixture,'id) perturbation list;
    configurations : configuration list;
    tokens : string Locality.annot list;
    volumes : (string * float * string) list
  }

type parsing_compil = (agent,mixture,mixture,string,rule) compil
type parsing_instruction = (agent,mixture,mixture,string,rule) instruction

val empty_compil : parsing_compil

val no_more_site_on_right : bool -> site list -> site list -> bool

val split_mixture : mixture -> (mixture * mixture)
(** @return (lhs,rhs) *)

val implicit_signature : parsing_compil -> parsing_compil
(** Infer agent signatures and tokens from init, rules and perturbations *)

(** {6 Printers} *)

val print_link :
  ('a -> Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) link -> unit
val print_counter : Format.formatter -> counter -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_rule : Format.formatter -> rule -> unit
val print_rule_content :
  bidirectional:bool -> Format.formatter -> rule_content -> unit

val link_to_json :
  ('a -> 'a -> Yojson.Basic.json) -> ('a -> Yojson.Basic.json) ->
  ('b -> Yojson.Basic.json list) -> ('a, 'b) link -> Yojson.Basic.json
(** Fragile: the list MUST NOT be a singleton *)

val link_of_json :
  ('a -> Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'a) ->
  (Yojson.Basic.json list -> 'b) -> Yojson.Basic.json -> ('a, 'b) link

val compil_of_json : Yojson.Basic.json -> parsing_compil
val compil_to_json : parsing_compil -> Yojson.Basic.json
