(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Kappa AST just after parsing *)
type syntax_version = V3 | V4

val merge_version : syntax_version -> syntax_version -> syntax_version

type internal = string option Loc.annoted list

type port = {
  port_name: string Loc.annoted;
  port_int: internal;
  port_int_mod: string Loc.annoted option;
  port_link: (string Loc.annoted, unit) LKappa.link Loc.annoted list;
  port_link_mod: int Loc.annoted option option;
}
(** Describe a port from an agent. [_int] references the internal state of the port, [_link], the possible links that can be made to this port, [_mod] to the changes in a rule that would be made to the state, used only in edit_notation  *)

(** What test is done by the counter expression
 * - CEQ: If counter value is equal to the specified value
 * - CGTE: If counter value is greater or equal to the specified value
 * - CLTE: If counter value is less or equal to the specified value
 * - CVAR: Not a test, but defines a variable to be used in the rule rates *)
type counter_test = CEQ of int | CGTE of int | CLTE of int | CVAR of string

type counter = {
  counter_name: string Loc.annoted;
  counter_test: counter_test Loc.annoted option;
  counter_delta: int Loc.annoted;
}

type site = Port of port | Counter of counter
type agent_mod = NoMod | Erase | Create

type agent =
  | Present of string Loc.annoted * site list * agent_mod
  | Absent of Loc.t

type mixture = agent list list

val mixture_to_user_graph : mixture -> User_graph.connected_component

type edit_notation = {
  mix: mixture;
  delta_token:
    ((mixture, string) Alg_expr.e Loc.annoted * string Loc.annoted) list;
}

type arrow_notation = {
  lhs: mixture;
  rm_token:
    ((mixture, string) Alg_expr.e Loc.annoted * string Loc.annoted) list;
  rhs: mixture;
  add_token:
    ((mixture, string) Alg_expr.e Loc.annoted * string Loc.annoted) list;
}

type rule_content = Edit of edit_notation | Arrow of arrow_notation

type rule = {
  (* TODO: is rewrite good naming ? *)
  rewrite: rule_content;
  bidirectional: bool;
  (* rates *)
  k_def: (mixture, string) Alg_expr.e Loc.annoted;
  k_un:
    ((mixture, string) Alg_expr.e Loc.annoted
    * (mixture, string) Alg_expr.e Loc.annoted option)
    option;
  (*k_1:radius_opt*)
  k_op: (mixture, string) Alg_expr.e Loc.annoted option;
  k_op_un:
    ((mixture, string) Alg_expr.e Loc.annoted
    * (mixture, string) Alg_expr.e Loc.annoted option)
    option;
      (*rate for backward rule*)
}

val flip_label : string -> string

type ('pattern, 'mixture, 'id, 'rule) modif_expr =
  | APPLY of (('pattern, 'id) Alg_expr.e Loc.annoted * 'rule Loc.annoted)
  | UPDATE of ('id Loc.annoted * ('pattern, 'id) Alg_expr.e Loc.annoted)
  (*TODO: pause*)
  | STOP of ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | SNAPSHOT of bool * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      ('pattern, 'id) Alg_expr.e Primitives.print_expr list
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Loc.annoted)
  | CFLOWMIX of (bool * 'pattern Loc.annoted)
  | DIN of
      Primitives.din_kind
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | DINOFF of ('pattern, 'id) Alg_expr.e Primitives.print_expr list
  | SPECIES_OF of
      bool
      * ('pattern, 'id) Alg_expr.e Primitives.print_expr list
      * 'pattern Loc.annoted

type ('pattern, 'mixture, 'id, 'rule) perturbation =
  (Nbr.t option
  * ('pattern, 'id) Alg_expr.bool Loc.annoted option
  * ('pattern, 'mixture, 'id, 'rule) modif_expr list
  * ('pattern, 'id) Alg_expr.bool Loc.annoted option)
  Loc.annoted

type configuration = string Loc.annoted * string Loc.annoted list

type ('pattern, 'id) variable_def =
  string Loc.annoted * ('pattern, 'id) Alg_expr.e Loc.annoted

type ('mixture, 'id) init_t =
  | INIT_MIX of 'mixture Loc.annoted
  | INIT_TOK of 'id Loc.annoted list

type ('pattern, 'mixture, 'id) init_statement =
  ('pattern, 'id) Alg_expr.e Loc.annoted * ('mixture, 'id) init_t

type ('agent, 'pattern, 'mixture, 'id, 'rule) instruction =
  | SIG of 'agent
  | TOKENSIG of string Loc.annoted
  | VOLSIG of string * float * string  (** type, volume, parameter *)
  | INIT of ('pattern, 'mixture, 'id) init_statement
  | DECLARE of ('pattern, 'id) variable_def
  | OBS of ('pattern, 'id) variable_def (*for backward compatibility*)
  | PLOT of ('pattern, 'id) Alg_expr.e Loc.annoted
  | PERT of ('pattern, 'mixture, 'id, 'rule) perturbation
  | CONFIG of configuration
  | RULE of (string Loc.annoted option * 'rule Loc.annoted)

type ('pattern, 'mixture, 'id, 'rule) command =
  | RUN of ('pattern, 'id) Alg_expr.bool Loc.annoted
  | MODIFY of ('pattern, 'mixture, 'id, 'rule) modif_expr list
  | QUIT

type ('agent, 'pattern, 'mixture, 'id, 'rule) compil = {
  filenames: string list;
  variables: ('pattern, 'id) variable_def list;
      (** pattern declaration for reusing as variable in perturbations or kinetic rate *)
  signatures: 'agent list;  (** agent signature declarations *)
  rules: (string Loc.annoted option * 'rule Loc.annoted) list;
      (**rules (possibly named)*)
  observables: ('pattern, 'id) Alg_expr.e Loc.annoted list;
      (** list of patterns to plot *)
  init: ('pattern, 'mixture, 'id) init_statement list;
      (** initial graph declaration *)
  perturbations: ('pattern, 'mixture, 'id, 'rule) perturbation list;
  configurations: configuration list;
  tokens: string Loc.annoted list;
  volumes: (string * float * string) list;
}

type parsing_compil = (agent, mixture, mixture, string, rule) compil
type parsing_instruction = (agent, mixture, mixture, string, rule) instruction

val empty_compil : parsing_compil
val no_more_site_on_right : bool -> site list -> site list -> bool

val split_mixture : mixture -> mixture * mixture
(** @return (lhs,rhs) *)

val infer_agent_signatures : parsing_compil -> parsing_compil
(** Used when agent signatures is implicit: infer agent signatures and tokens from init, rules and perturbations *)

(** {6 Printers} *)

val print_counter : Format.formatter -> counter -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_rule : Format.formatter -> rule -> unit

val print_rule_content :
  bidirectional:bool -> Format.formatter -> rule_content -> unit

val print_parsing_compil_kappa : Format.formatter -> parsing_compil -> unit
val to_erased_mixture : mixture -> mixture
val to_created_mixture : mixture -> mixture
val compil_of_json : Yojson.Basic.t -> parsing_compil
val compil_to_json : parsing_compil -> Yojson.Basic.t
val write_parsing_compil : Buffer.t -> parsing_compil -> unit
val read_parsing_compil : Yojson.lexer_state -> Lexing.lexbuf -> parsing_compil
