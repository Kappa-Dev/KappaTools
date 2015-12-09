type ('a,'annot) link =
  | LNK_VALUE of int * 'annot
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a (* port *) * 'a (*agent_type*)

type internal = string Location.annot list

type port = {port_nme:string Location.annot;
	     port_int:internal;
	     port_lnk:(string Location.annot,unit) link Location.annot;}

type agent = (string Location.annot * port list)

type mixture = agent list

type ('mixt,'id) ast_alg_expr =
  | BIN_ALG_OP of
      Operator.bin_alg_op * ('mixt,'id) ast_alg_expr Location.annot
      * ('mixt,'id) ast_alg_expr Location.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mixt,'id) ast_alg_expr Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | OBS_VAR of 'id
  | TOKEN_ID of 'id
  | KAPPA_INSTANCE of 'mixt
  | CONST of Nbr.t
  | TMAX
  | EMAX
  | PLOTNUM

type 'a bool_expr =
  | TRUE
  | FALSE
  | BOOL_OP of Operator.bool_op *
		 'a bool_expr Location.annot * 'a bool_expr Location.annot
  | COMPARE_OP of Operator.compare_op * 'a Location.annot * 'a Location.annot

type arrow = RAR | LRAR

type rule = {
    lhs: mixture ;
    rm_token: ((mixture,string) ast_alg_expr Location.annot *
		 string Location.annot) list;
    arrow:arrow ;
    rhs: mixture ;
    add_token: ((mixture,string) ast_alg_expr Location.annot *
		  string Location.annot) list;
    k_def: (mixture,string) ast_alg_expr Location.annot ;
    k_absolute: bool;
    k_un:
      ((mixture,string) ast_alg_expr Location.annot *
	 (mixture,string) ast_alg_expr Location.annot option) option;
    (*k_1:radius_opt*)
    k_op: (mixture,string) ast_alg_expr Location.annot option ;
    (*rate for backward rule*)
  }

val flip_label : string -> string

type 'alg_expr print_expr =
  | Str_pexpr of string Location.annot
  | Alg_pexpr of 'alg_expr Location.annot

type ('mixture,'id) modif_expr =
  | INTRO of
      (('mixture,'id) ast_alg_expr Location.annot * 'mixture Location.annot)
  | DELETE of
      (('mixture,'id) ast_alg_expr Location.annot * 'mixture Location.annot)
  | UPDATE of
      (string Location.annot * ('mixture,'id) ast_alg_expr Location.annot)
  (*TODO: pause*)
  | UPDATE_TOK of
      ('id Location.annot * ('mixture,'id) ast_alg_expr Location.annot)
  (*TODO: pause*)
  | STOP of ('mixture,'id) ast_alg_expr print_expr list
  | SNAPSHOT of ('mixture,'id) ast_alg_expr print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      ((('mixture,'id) ast_alg_expr print_expr list) *
	 (('mixture,'id) ast_alg_expr print_expr list))
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Location.annot)
  | CFLOWMIX of (bool * 'mixture Location.annot)
  | FLUX of ('mixture,'id) ast_alg_expr print_expr list
  | FLUXOFF of ('mixture,'id) ast_alg_expr print_expr list

type ('mixture,'id) perturbation =
  (('mixture,'id) ast_alg_expr bool_expr Location.annot *
     (('mixture,'id) modif_expr list) *
       ('mixture,'id) ast_alg_expr bool_expr Location.annot option)
    Location.annot

type configuration = string Location.annot * (string Location.annot list)

type ('mixture,'id) variable_def =
    string Location.annot * ('mixture,'id) ast_alg_expr Location.annot

type ('mixture,'id) init_t =
  | INIT_MIX of
      ('mixture,'id) ast_alg_expr Location.annot * 'mixture Location.annot
  | INIT_TOK of
      ('mixture,'id) ast_alg_expr Location.annot * 'id Location.annot

type ('mixture,'id) instruction =
  | SIG      of agent
  | TOKENSIG of string Location.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of string Location.annot option * ('mixture,'id) init_t
  (*volume, init, position *)
  | DECLARE  of ('mixture,'id) variable_def
  | OBS      of ('mixture,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('mixture,'id) ast_alg_expr Location.annot
  | PERT     of ('mixture,'id) perturbation
  | CONFIG   of configuration

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
	('mixture,'id) ast_alg_expr Location.annot list;
      (*list of patterns to plot*)
      init :
	(string Location.annot option * ('mixture,'id) init_t) list;
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

val result : (agent,mixture,string,rule) compil ref
val init_compil : unit -> unit

val no_more_site_on_right : bool -> port list -> port list -> bool

(** {6 Printers} *)

val print_link :
  ('a -> Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) link -> unit
val print_ast_mix : Format.formatter -> mixture -> unit
val print_ast_alg :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a,'b) ast_alg_expr -> unit
val print_ast_rule : Format.formatter -> rule -> unit
val print_ast_rule_no_rate :
  reverse:bool -> Format.formatter -> rule -> unit

val print_bool :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a bool_expr -> unit
val print_ast_bool :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a,'b) ast_alg_expr bool_expr -> unit
