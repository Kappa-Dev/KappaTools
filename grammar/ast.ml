open Misc

type alg_expr = 
		MULT of alg_expr * alg_expr * position
	| SUM of alg_expr * alg_expr * position
	| DIV of alg_expr * alg_expr * position
	| MINUS of alg_expr * alg_expr * position
	| POW of alg_expr * alg_expr * position
	| MODULO of alg_expr * alg_expr * position
	| LOG of alg_expr * position
	| SQRT of alg_expr * position
	| EXP of alg_expr * position
	| SINUS of alg_expr * position
	| COSINUS of alg_expr * position
	| TAN of alg_expr * position
	| ABS of alg_expr * position
	| TIME_VAR of position
	| EVENT_VAR of position
	| OBS_VAR of (string * position)
	| FLOAT of float * position
	| TMAX of position
	| EMAX of position
	| INFINITY of position

type bool_expr =
	| TRUE of position
	| FALSE of position
	| AND of bool_expr * bool_expr * position
	| OR of bool_expr * bool_expr * position
	| GREATER of alg_expr * alg_expr * position
	| SMALLER of alg_expr * alg_expr * position
	| EQUAL of alg_expr * alg_expr * position
	| NOT of bool_expr * position

type mixture = 
	| COMMA of agent * mixture 
	| DOT of int * agent * mixture 
	| PLUS of int * agent * mixture 
	| EMPTY_MIX
and agent = {ag_nme:string ; ag_intf:interface ; ag_pos:position}
and interface = PORT_SEP of port * interface | EMPTY_INTF
and port = {port_nme:string ; port_int: internal ; port_lnk : link ; port_pos : position}
and internal = string list
and link = 
	| LNK_VALUE of (int * position)
	| FREE 
	| LNK_ANY of position 
	| LNK_SOME of position
	| LNK_TYPE of ((string * position) * (string * position))

type rule = {lhs: mixture ; arrow:arrow ; rhs:mixture; k_def:alg_expr ; k_un:alg_expr option}
and arrow = RAR | RAR_NOPOLY of position
type rule_label = {lbl_nme:(string * position) option ; lbl_ref:(string * position) option}

type perturbation = bool_expr * modif_expr * position * bool_expr option
and modif_expr = 
	| INTRO of (alg_expr * mixture * position) 
	| DELETE of (alg_expr * mixture * position) 
	| UPDATE of (string * position * alg_expr * position) (*TODO: pause*)
	| STOP of position
	| SNAPSHOT of position (*maybe later of mixture too*)

type instruction = 
	| SIG of agent * position 
	| INIT of int * mixture * position
	| DECLARE of variable
	| OBS of variable  (*for backward compatibility*)
	| PLOT of alg_expr
	| PERT of perturbation
and variable =
	| VAR_KAPPA of (mixture * (string * position))
	| VAR_ALG of (alg_expr * (string * position))

type compil = {variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
							 signatures : (agent * position) list ; (*agent signature declaration*)
							 rules : (rule_label * rule) list ; (*rules (possibly named)*)
							 observables : alg_expr list ; (*list of patterns to plot*) 
							 init : (int * mixture * position) list ; (*initial graph declaration*)
							 perturbations : perturbation list
							}

let result:compil ref = ref {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = []} 
let init_compil = fun _ -> result := {variables=[] ; signatures=[] ; rules=[] ; init = [] ; observables = [] ; perturbations = []}

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