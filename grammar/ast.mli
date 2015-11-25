type str_pos = string * Tools.pos

type ('a,'annot) link =
  | LNK_VALUE of int * 'annot
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a (* port *)
    * 'a (*agent_type*)

type internal = string Location.annot list

type port = {port_nme:string Location.annot;
	     port_int:internal;
	     port_lnk:(string Location.annot,unit) link Location.annot;}

type agent = (string Location.annot * port list)

type mixture = agent list

type 'mixt ast_alg_expr =
    BIN_ALG_OP of
      Operator.bin_alg_op * 'mixt ast_alg_expr Location.annot
      * 'mixt ast_alg_expr Location.annot
  | UN_ALG_OP of Operator.un_alg_op * 'mixt ast_alg_expr Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | OBS_VAR of string
  | TOKEN_ID of string
  | KAPPA_INSTANCE of 'mixt
  | CONST of Nbr.t
  | TMAX
  | EMAX
  | PLOTNUM

type 'a bool_expr =
  | TRUE
  | FALSE
  | BOOL_OP of
      Operator.bool_op * 'a bool_expr Location.annot * 'a bool_expr Location.annot
  | COMPARE_OP of Operator.compare_op * 'a Location.annot * 'a Location.annot

type arrow = RAR | LRAR

type rule = {
  lhs: mixture ;
  rm_token: (mixture ast_alg_expr Location.annot * string Location.annot) list ;
  arrow:arrow ;
  rhs: mixture ;
  add_token: (mixture ast_alg_expr Location.annot * string Location.annot) list;
  k_def: mixture ast_alg_expr Location.annot ;
  k_un:
    (mixture ast_alg_expr Location.annot *
       mixture ast_alg_expr Location.annot option) option;
  (*k_1:radius_opt*)
  k_op: mixture ast_alg_expr Location.annot option ; (*rate for backward rule*)
}

val flip_label : string -> string

type 'alg_expr print_expr =
    Str_pexpr of string
  | Alg_pexpr of 'alg_expr

type 'mixture modif_expr =
  | INTRO of ('mixture ast_alg_expr Location.annot * 'mixture Location.annot)
  | DELETE of ('mixture ast_alg_expr Location.annot * 'mixture Location.annot)
  | UPDATE of
      (string Location.annot * 'mixture ast_alg_expr Location.annot) (*TODO: pause*)
  | UPDATE_TOK of
      (string Location.annot * 'mixture ast_alg_expr Location.annot) (*TODO: pause*)
  | STOP of ('mixture ast_alg_expr print_expr Location.annot list * Tools.pos)
  | SNAPSHOT of
      ('mixture ast_alg_expr print_expr Location.annot list * Tools.pos)
  (*maybe later of mixture too*)
  | PRINT of
      (('mixture ast_alg_expr print_expr Location.annot list) *
	  ('mixture  ast_alg_expr print_expr Location.annot list) * Tools.pos)
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Location.annot)
  | CFLOWMIX of (bool * 'mixture Location.annot)
  | FLUX of 'mixture ast_alg_expr print_expr Location.annot list * Tools.pos
  | FLUXOFF of 'mixture ast_alg_expr print_expr Location.annot list * Tools.pos

type 'mixture perturbation =
    ('mixture ast_alg_expr bool_expr Location.annot * ('mixture modif_expr list) *
       'mixture ast_alg_expr bool_expr Location.annot option) Location.annot

type configuration = string Location.annot * (str_pos list)

type 'mixture variable_def =
    string Location.annot * 'mixture ast_alg_expr Location.annot

type 'mixture init_t =
  | INIT_MIX of 'mixture ast_alg_expr Location.annot * 'mixture Location.annot
  | INIT_TOK of 'mixture ast_alg_expr Location.annot * string Location.annot

type 'mixture instruction =
  | SIG      of agent
  | TOKENSIG of string Location.annot
  | VOLSIG   of str_pos * float * str_pos (* type, volume, parameter*)
  | INIT     of string Location.annot option * 'mixture init_t * Tools.pos
  (*volume, init, position *)
  | DECLARE  of 'mixture variable_def
  | OBS      of 'mixture variable_def (*for backward compatibility*)
  | PLOT     of 'mixture ast_alg_expr Location.annot
  | PERT     of 'mixture perturbation
  | CONFIG   of configuration

type ('agent,'mixture,'rule) compil =
    {
      variables :
	'mixture variable_def list;
      (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
      signatures :
	'agent list; (*agent signature declaration*)
      rules :
	(string Location.annot option * 'rule Location.annot) list;
      (*rules (possibly named)*)
      observables :
	'mixture ast_alg_expr Location.annot list;
      (*list of patterns to plot*)
      init :
	(string Location.annot option * 'mixture init_t * Tools.pos) list;
      (*initial graph declaration*)
      perturbations :
	'mixture perturbation list;
      configurations :
	configuration list;
      tokens :
	string Location.annot list;
      volumes :
	(str_pos * float * str_pos) list
    }

val result : (agent,mixture,rule) compil ref
val init_compil : unit -> unit

val no_more_site_on_right : bool -> port list -> port list -> bool

(** {6 Printers} *)

val print_link :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) link -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_alg : Format.formatter -> mixture ast_alg_expr -> unit
val print_ast_rule : Format.formatter -> rule -> unit
val print_ast_rule_no_rate :
  reverse:bool -> Format.formatter -> rule -> unit

val print_bool :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a bool_expr -> unit
val print_ast_bool :
  Format.formatter -> mixture ast_alg_expr bool_expr -> unit

