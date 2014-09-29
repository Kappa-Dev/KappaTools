open Term
open Mods

type str_pos = string * Tools.pos

type ast_alg_expr =
    BIN_ALG_OP of
      bin_alg_op * ast_alg_expr with_pos * ast_alg_expr with_pos
  | UN_ALG_OP of un_alg_op * ast_alg_expr with_pos
  | STATE_ALG_OP of state_alg_op
  | OBS_VAR of string
  | TOKEN_ID of string
  | CONST of Nbr.t
  | TMAX
  | EMAX

type ast_bool_expr =
  | TRUE
  | FALSE
  | AND of ast_bool_expr with_pos * ast_bool_expr with_pos
  | OR of ast_bool_expr with_pos * ast_bool_expr with_pos
  | GREATER of ast_alg_expr with_pos * ast_alg_expr with_pos
  | SMALLER of ast_alg_expr with_pos * ast_alg_expr with_pos
  | EQUAL of ast_alg_expr with_pos * ast_alg_expr with_pos
  | DIFF of ast_alg_expr with_pos * ast_alg_expr with_pos

type link =
  | LNK_VALUE of int * Tools.pos
  | FREE
  | LNK_ANY of Tools.pos
  | LNK_SOME of Tools.pos
  | LNK_TYPE of str_pos * str_pos
type mixture = 
	| COMMA of agent * mixture 
	| EMPTY_MIX
and agent = {ag_nme:string ; ag_intf:interface ; ag_pos:Tools.pos}
and interface = PORT_SEP of port * interface | EMPTY_INTF
and port = {port_nme:string ; port_int: internal ; port_lnk : link ; port_pos : Tools.pos}
and internal = string list

type rule = {
  rule_pos: Tools.pos ;
  lhs: mixture ;
  rm_token: (ast_alg_expr with_pos * str_pos) list ;
  arrow:arrow ;
  rhs:mixture;
  add_token: (ast_alg_expr with_pos * str_pos) list ;
  k_def:ast_alg_expr with_pos ;
  k_un:(ast_alg_expr with_pos * ast_alg_expr with_pos option) option ;
  (*k_1:radius_opt*)
  k_op: ast_alg_expr with_pos option ; (*rate for backward rule*)
}

and arrow = RAR of Tools.pos | LRAR of Tools.pos
type rule_label = {lbl_nme:str_pos option ; lbl_ref:str_pos option}

let flip (rule_label,rule) = 
	let lbl = match rule_label.lbl_nme with None -> None | Some (str,pos) -> Some (str^"_op",pos)
	and rule = 
		{rule with 
			lhs = rule.rhs ; 
			rhs = rule.lhs ; 
			add_token = rule.rm_token ; 
			rm_token = rule.add_token ; 
			k_def = (match rule.k_op with
				   None -> CONST (Nbr.F 0.),
					   (Lexing.dummy_pos, Lexing.dummy_pos)
				 | Some k -> k);
			k_op = None
			}
	in 
	({rule_label with lbl_nme=lbl},rule)
		

type print_expr = Str_pexpr of string | Alg_pexpr of ast_alg_expr
type modif_expr =
	| INTRO of (ast_alg_expr with_pos * mixture * Tools.pos)
	| DELETE of (ast_alg_expr with_pos * mixture * Tools.pos)
	| UPDATE of (str_pos * ast_alg_expr with_pos) (*TODO: pause*)
	| UPDATE_TOK of (str_pos * ast_alg_expr with_pos) (*TODO: pause*)
	| STOP of (print_expr with_pos list * Tools.pos)
	| SNAPSHOT of (print_expr with_pos list * Tools.pos)
	(*maybe later of mixture too*)
	| PRINT of ((print_expr with_pos list) * (print_expr with_pos list) * Tools.pos)
	| CFLOW of (str_pos * Tools.pos)
	| CFLOWOFF of (str_pos * Tools.pos)
	| FLUX of print_expr with_pos list * Tools.pos
	| FLUXOFF of print_expr with_pos list * Tools.pos
type perturbation =
    ast_bool_expr with_pos * (modif_expr list) *
      Tools.pos * ast_bool_expr with_pos option



type configuration = str_pos * (str_pos list)

type instruction = 
	| SIG of agent * Tools.pos
	| TOKENSIG of str_pos
	| VOLSIG of str_pos * float * str_pos (* type, volume, parameter*)
	| INIT of str_pos option * init_t * Tools.pos (*volume, init, position *)
	| DECLARE of variable
	| OBS of variable  (*for backward compatibility*)
	| PLOT of ast_alg_expr with_pos
	| PERT of perturbation
	| CONFIG of configuration
and init_t = 
	| INIT_MIX of  ast_alg_expr with_pos * mixture 
	| INIT_TOK of  ast_alg_expr with_pos * str_pos 
and variable = 
	| VAR_KAPPA of mixture * str_pos 
	| VAR_ALG of ast_alg_expr with_pos * str_pos 
	
type compil = {variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
	       signatures : (agent * Tools.pos) list ; (*agent signature declaration*)
	       rules : (rule_label * rule) list ; (*rules (possibly named)*)
	       observables : ast_alg_expr with_pos list ; (*list of patterns to plot*)
	       init : (str_pos option * init_t * Tools.pos) list ; (*initial graph declaration*)
	       perturbations : perturbation list ;
	       configurations : configuration list ;
	       tokens :  str_pos list ;
	       volumes : (str_pos * float * str_pos) list
	      }
let result:compil ref = ref {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = [] ; configurations = [] ; tokens = []; volumes=[]} 
let init_compil = fun _ -> result := {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = [] ; configurations = [] ; tokens = [] ; volumes=[]}

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
