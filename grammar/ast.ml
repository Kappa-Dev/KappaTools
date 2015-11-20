open Operator

type str_pos = string * Tools.pos

type link =
  | LNK_VALUE of int
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of string Location.annot (* port *)
    * string Location.annot (*agent_type*)

type internal = string Location.annot list

type port = {port_nme:string Location.annot;
	     port_int:internal;
	     port_lnk:link Location.annot;}

type agent = (string Location.annot * port list)

type mixture = agent list

type 'mixt ast_alg_expr =
    BIN_ALG_OP of
      bin_alg_op * 'mixt ast_alg_expr Location.annot
      * 'mixt ast_alg_expr Location.annot
  | UN_ALG_OP of un_alg_op * 'mixt ast_alg_expr Location.annot
  | STATE_ALG_OP of state_alg_op
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
      bool_op * 'a bool_expr Location.annot * 'a bool_expr Location.annot
  | COMPARE_OP of compare_op * 'a Location.annot * 'a Location.annot

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

let flip_label str = str^"_op"

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

let no_more_site_on_right warning left right =
  List.for_all
    (fun p ->
     List.exists (fun p' -> fst p.port_nme = fst p'.port_nme) left
     || let () =
	  if warning then
	    ExceptionDefn.warning
	      ~pos:(snd p.port_nme)
	      (fun f ->
		Format.fprintf
		  f "@[Site@ '%s'@ was@ not@ mentionned in@ the@ left-hand@ side."
		  (fst p.port_nme);
		Format.fprintf
		  f "This@ agent@ and@ the@ following@ will@ be@ removed@ and@ ";
		Format.fprintf
		  f "recreated@ (probably@ causing@ side@ effects).@]")
	in false)
    right

let result:(agent,mixture,rule) compil ref =
  ref {
    variables      = [];
    signatures     = [];
    rules          = [];
    init           = [];
    observables    = [];
    perturbations  = [];
    configurations = [];
    tokens         = [];
    volumes        = []
  }

let init_compil () =
  result :=
    {
      variables      = [];
      signatures     = [];
      rules          = [];
      init           = [];
      observables    = [];
      perturbations  = [];
      configurations = [];
      tokens         = [];
      volumes        = []
    }

(*
  let reverse res = 
  let l_pat = List.rev !res.patterns
  and l_sig = List.rev !res.signatures
  and l_rul = List.rev !res.rules
  and l_ini = List.rev !res.init
  and l_obs = List.rev !res.observables
  in
  res:={patterns=l_pat ; signatures=l_sig ; rules=l_rul ; init = l_ini ; observables = l_obs}
*)
