(** Kappa AST just after parsing *)

type formatCflow =
  | Dot
  | Html
  | Json

type ('a,'annot) link =
  | LNK_VALUE of int * 'annot
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a * 'a (** port * agent_type *)

type internal = string Location.annot list

type port = {port_nme:string Location.annot;
             port_int:internal;
             port_lnk:(string Location.annot,unit) link Location.annot;}

type agent = (string Location.annot * port list)

type mixture = agent list

type arrow = RAR | LRAR

type rule = {
  lhs: mixture ;
  rm_token: ((mixture,string) Alg_expr.e Location.annot *
             string Location.annot) list;
  arrow:arrow ;
  rhs: mixture ;
  add_token: ((mixture,string) Alg_expr.e Location.annot *
              string Location.annot) list;
  k_def: (mixture,string) Alg_expr.e Location.annot ;
  k_un:
    ((mixture,string) Alg_expr.e Location.annot *
     int Location.annot option) option;
  (*k_1:radius_opt*)
  k_op: (mixture,string) Alg_expr.e Location.annot option ;
  k_op_un:
    ((mixture,string) Alg_expr.e Location.annot *
     int Location.annot option) option;
  (*rate for backward rule*)
}

val flip_label : string -> string

type 'alg_expr print_expr =
  | Str_pexpr of string Location.annot
  | Alg_pexpr of 'alg_expr Location.annot

type ('mixture,'id) modif_expr =
  | INTRO of
      (('mixture,'id) Alg_expr.e Location.annot * 'mixture Location.annot)
  | DELETE of
      (('mixture,'id) Alg_expr.e Location.annot * 'mixture Location.annot)
  | UPDATE of
      ('id Location.annot * ('mixture,'id) Alg_expr.e Location.annot)
  (*TODO: pause*)
  | UPDATE_TOK of
      ('id Location.annot * ('mixture,'id) Alg_expr.e Location.annot)
  (*TODO: pause*)
  | STOP of ('mixture,'id) Alg_expr.e print_expr list
  | SNAPSHOT of ('mixture,'id) Alg_expr.e print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      ((('mixture,'id) Alg_expr.e print_expr list) *
       (('mixture,'id) Alg_expr.e print_expr list))
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Location.annot)
  | CFLOWMIX of (bool * 'mixture Location.annot)
  | FLUX of bool * ('mixture,'id) Alg_expr.e print_expr list
  | FLUXOFF of ('mixture,'id) Alg_expr.e print_expr list

type ('mixture,'id) perturbation =
  (('mixture,'id) Alg_expr.bool_expr Location.annot *
   (('mixture,'id) modif_expr list) *
   ('mixture,'id) Alg_expr.bool_expr Location.annot option)
    Location.annot

type configuration = string Location.annot * (string Location.annot list)

type ('mixture,'id) variable_def =
  string Location.annot * ('mixture,'id) Alg_expr.e Location.annot

type ('mixture,'id) init_t =
  | INIT_MIX of 'mixture
  | INIT_TOK of 'id

type ('mixture,'id) instruction =
  | SIG      of agent
  | TOKENSIG of string Location.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of
      string Location.annot option *
      ('mixture,'id) Alg_expr.e Location.annot *
      ('mixture,'id) init_t Location.annot
  (*volume, init, position *)
  | DECLARE  of ('mixture,'id) variable_def
  | OBS      of ('mixture,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('mixture,'id) Alg_expr.e Location.annot
  | PERT     of ('mixture,'id) perturbation
  | CONFIG   of configuration

type ('mixture,'id) command =
  | RUN of ('mixture,'id) Alg_expr.bool_expr
  | MODIFY of ('mixture,'id) modif_expr
  | QUIT

type ('agent,'mixture,'id,'rule) compil =
  {
    variables :
      ('mixture,'id) variable_def list;
    (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures :
      'agent list; (*agent signature declaration*)
    rules :
      (string Location.annot option * 'rule Location.annot) list;
    (*rules (possibly named)*)
    observables :
      ('mixture,'id) Alg_expr.e Location.annot list;
    (*list of patterns to plot*)
    init :
      (string Location.annot option *
       ('mixture,'id) Alg_expr.e Location.annot *
       ('mixture,'id) init_t Location.annot) list;
    (*initial graph declaration*)
    perturbations :
      ('mixture,'id) perturbation list;
    configurations :
      configuration list;
    tokens :
      string Location.annot list;
    volumes :
      (string * float * string) list
  }

val empty_compil : (agent,mixture,string,rule) compil

val no_more_site_on_right : bool -> port list -> port list -> bool

val implicit_signature :
  (agent,mixture,string,rule) compil -> (agent,mixture,string,rule) compil
(** Infer agent signatures and tokens from init, rules and perturbations *)

(** {6 Printers} *)

val print_link :
  ('a -> Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) link -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_rule : Format.formatter -> rule -> unit
val print_ast_rule_no_rate :
  reverse:bool -> Format.formatter -> rule -> unit

val link_to_json :
  ('a -> 'a -> Yojson.Basic.json) -> ('a -> Yojson.Basic.json) ->
  ('b -> Yojson.Basic.json list) -> ('a, 'b) link -> Yojson.Basic.json
(** Fragile: the list MUST NOT be a singleton *)

val link_of_json :
  ('a -> Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'a) ->
  (Yojson.Basic.json list -> 'b) -> Yojson.Basic.json -> ('a, 'b) link

val compil_of_json :
  Yojson.Basic.json -> (agent,mixture,string,rule) compil
val compil_to_json :
  (agent,mixture,string,rule) compil -> Yojson.Basic.json
