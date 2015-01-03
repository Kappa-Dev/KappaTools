open Term
open Mods

type str_pos = string * Tools.pos

type link =
  | LNK_VALUE of int
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of str_pos * str_pos
type internal = string Term.with_pos list
type port = {port_nme:string Term.with_pos;
	     port_int:internal;
	     port_lnk:link with_pos;}
type agent = (string with_pos * port list)
type mixture =
	| COMMA of agent * mixture
	| EMPTY_MIX

type 'mixt ast_alg_expr =
    BIN_ALG_OP of
      bin_alg_op * 'mixt ast_alg_expr with_pos * 'mixt ast_alg_expr with_pos
  | UN_ALG_OP of un_alg_op * 'mixt ast_alg_expr with_pos
  | STATE_ALG_OP of state_alg_op
  | OBS_VAR of string
  | TOKEN_ID of string
  | KAPPA_INSTANCE of 'mixt
  | CONST of Nbr.t
  | TMAX | EMAX | PLOTNUM

type 'a bool_expr =
  | TRUE
  | FALSE
  | BOOL_OP of
      bool_op * 'a bool_expr with_pos * 'a bool_expr with_pos
  | COMPARE_OP of
      compare_op * 'a with_pos * 'a with_pos

type arrow = RAR | LRAR
type rule = {
  lhs: mixture ;
  rm_token: (mixture ast_alg_expr with_pos * str_pos) list ;
  arrow:arrow ;
  rhs: mixture ;
  add_token: (mixture ast_alg_expr with_pos * str_pos) list ;
  k_def: mixture ast_alg_expr with_pos ;
  k_un:
    (mixture ast_alg_expr with_pos*mixture ast_alg_expr with_pos option) option;
  (*k_1:radius_opt*)
  k_op: mixture ast_alg_expr with_pos option ; (*rate for backward rule*)
}

let flip_label str = str^"_op"

let flip (rule_label,rule) =
  let lbl = match rule_label with
      None -> None
    | Some (str,pos) -> Some (flip_label str,pos) in
  let rule =
    {rule with
      lhs = rule.rhs ;
      rhs = rule.lhs ;
      add_token = rule.rm_token ;
      rm_token = rule.add_token ;
      k_def = (match rule.k_op with
		 None -> Term.with_dummy_pos (CONST (Nbr.F 0.))
	       | Some k -> k);
      k_op = None
    }
  in
  (lbl,rule)

type 'alg_expr print_expr =
    Str_pexpr of string | Alg_pexpr of 'alg_expr
type 'mixture modif_expr =
  | INTRO of ('mixture ast_alg_expr with_pos * 'mixture  * Tools.pos)
  | DELETE of ('mixture ast_alg_expr with_pos * 'mixture * Tools.pos)
  | UPDATE of
      (string Term.with_pos * 'mixture ast_alg_expr with_pos) (*TODO: pause*)
  | UPDATE_TOK of (str_pos * 'mixture ast_alg_expr with_pos) (*TODO: pause*)
  | STOP of ('mixture ast_alg_expr print_expr with_pos list * Tools.pos)
  | SNAPSHOT of ('mixture ast_alg_expr print_expr with_pos list * Tools.pos)
  (*maybe later of mixture too*)
  | PRINT of
      (('mixture ast_alg_expr print_expr with_pos list) *
	 ('mixture  ast_alg_expr print_expr with_pos list) * Tools.pos)
  | CFLOW of (string Term.with_pos * Tools.pos)
  | CFLOWOFF of (string Term.with_pos * Tools.pos)
  | FLUX of 'mixture ast_alg_expr print_expr with_pos list * Tools.pos
  | FLUXOFF of 'mixture ast_alg_expr print_expr with_pos list * Tools.pos
type 'mixture perturbation =
    ('mixture ast_alg_expr bool_expr with_pos * ('mixture modif_expr list) *
       'mixture ast_alg_expr bool_expr with_pos option) Term.with_pos



type configuration = string Term.with_pos * (str_pos list)

type 'mixture variable_def = string with_pos * 'mixture ast_alg_expr with_pos
type 'mixture init_t =
  | INIT_MIX of 'mixture ast_alg_expr with_pos * 'mixture
  | INIT_TOK of 'mixture ast_alg_expr with_pos * str_pos

type 'mixture instruction =
  | SIG of agent
  | TOKENSIG of string Term.with_pos
  | VOLSIG of str_pos * float * str_pos (* type, volume, parameter*)
  | INIT of str_pos option * 'mixture init_t * Tools.pos (*volume, init, position *)
  | DECLARE of 'mixture variable_def
  | OBS of 'mixture variable_def (*for backward compatibility*)
  | PLOT of 'mixture ast_alg_expr with_pos
  | PERT of 'mixture perturbation
  | CONFIG of configuration

type ('agent,'mixture,'rule) compil =
  {
    variables : 'mixture variable_def list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures : 'agent list ; (*agent signature declaration*)
    rules : (string with_pos option * 'rule with_pos) list ; (*rules (possibly named)*)
    observables : 'mixture ast_alg_expr with_pos list ; (*list of patterns to plot*)
    init : (str_pos option * 'mixture init_t * Tools.pos) list ; (*initial graph declaration*)
    perturbations : 'mixture perturbation list ;
    configurations : configuration list ;
    tokens :  string Term.with_pos list ;
    volumes : (str_pos * float * str_pos) list
  }
let result:(agent,mixture,rule) compil ref =
  ref {variables=[]; signatures=[]; rules=[]; init=[]; observables=[];
       perturbations=[]; configurations=[]; tokens=[]; volumes=[]}
let init_compil _ =
  result :=
    {variables=[]; signatures=[]; rules=[]; init = []; observables=[];
     perturbations=[]; configurations=[]; tokens=[]; volumes=[]}

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
