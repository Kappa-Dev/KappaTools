type alg_expr = 
		MULT of alg_expr * alg_expr * Tools.pos
	| SUM of alg_expr * alg_expr * Tools.pos
	| DIV of alg_expr * alg_expr * Tools.pos
	| MINUS of alg_expr * alg_expr * Tools.pos
	| POW of alg_expr * alg_expr * Tools.pos
	| MODULO of alg_expr * alg_expr * Tools.pos
	| LOG of alg_expr * Tools.pos
	| SQRT of alg_expr * Tools.pos
	| EXP of alg_expr * Tools.pos
	| SINUS of alg_expr * Tools.pos
	| COSINUS of alg_expr * Tools.pos
	| TAN of alg_expr * Tools.pos
	| ABS of alg_expr * Tools.pos
	| TIME_VAR of Tools.pos
	| EVENT_VAR of Tools.pos
	| OBS_VAR of (string * Tools.pos)
	| FLOAT of float * Tools.pos
	| TMAX of Tools.pos
	| EMAX of Tools.pos
	| INFINITY of Tools.pos

type bool_expr =
	| TRUE of Tools.pos
	| FALSE of Tools.pos
	| AND of bool_expr * bool_expr * Tools.pos
	| OR of bool_expr * bool_expr * Tools.pos
	| GREATER of alg_expr * alg_expr * Tools.pos
	| SMALLER of alg_expr * alg_expr * Tools.pos
	| EQUAL of alg_expr * alg_expr * Tools.pos
	| NOT of bool_expr * Tools.pos

type mixture = 
	| COMMA of agent * mixture 
	| DOT of int * agent * mixture 
	| PLUS of int * agent * mixture 
	| EMPTY_MIX
and agent = {ag_nme:string ; ag_intf:interface ; ag_pos:Tools.pos}
and interface = PORT_SEP of port * interface | EMPTY_INTF
and port = {port_nme:string ; port_int: internal ; port_lnk : link ; port_pos : Tools.pos}
and internal = string list
and link = 
	| LNK_VALUE of (int * Tools.pos)
	| FREE 
	| LNK_ANY of Tools.pos 
	| LNK_SOME of Tools.pos
	| LNK_TYPE of ((string * Tools.pos) * (string * Tools.pos))

type rule = {lhs: mixture ; arrow:arrow ; rhs:mixture; k_def:alg_expr ; k_un:alg_expr option}
and arrow = RAR | RAR_NOPOLY of Tools.pos
type rule_label = {lbl_nme:(string * Tools.pos) option ; lbl_ref:(string * Tools.pos) option}

type perturbation = bool_expr * modif_expr * Tools.pos * bool_expr option
and modif_expr = 
	| INTRO of (alg_expr * mixture * Tools.pos) 
	| DELETE of (alg_expr * mixture * Tools.pos) 
	| UPDATE of (string * Tools.pos * alg_expr * Tools.pos) (*TODO: pause*)
	| STOP of Tools.pos
	| SNAPSHOT of Tools.pos (*maybe later of mixture too*)

type instruction = 
	| SIG of agent * Tools.pos 
	| INIT of int * mixture * Tools.pos
	| DECLARE of variable
	| OBS of variable  (*for backward compatibility*)
	| PLOT of alg_expr
	| PERT of perturbation
and variable =
	| VAR_KAPPA of (mixture * (string * Tools.pos))
	| VAR_ALG of (alg_expr * (string * Tools.pos))

type compil = {variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
							 signatures : (agent * Tools.pos) list ; (*agent signature declaration*)
							 rules : (rule_label * rule) list ; (*rules (possibly named)*)
							 observables : alg_expr list ; (*list of patterns to plot*) 
							 init : (int * mixture * Tools.pos) list ; (*initial graph declaration*)
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