type str_pos = string * Tools.pos

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
	| MAX of alg_expr * alg_expr * Tools.pos
	| MIN of alg_expr * alg_expr * Tools.pos
	| TIME_VAR of Tools.pos
	| EVENT_VAR of Tools.pos
	| NULL_EVENT_VAR of Tools.pos
	| PROD_EVENT_VAR of Tools.pos
	| OBS_VAR of str_pos 
	| TOKEN_ID of str_pos 
	| FLOAT of float * Tools.pos
	| INT of int * Tools.pos
	| TMAX of Tools.pos
	| EMAX of Tools.pos
	| CPUTIME of Tools.pos
	| INFINITY of Tools.pos

type bool_expr =
	| TRUE of Tools.pos
	| FALSE of Tools.pos
	| AND of bool_expr * bool_expr * Tools.pos
	| OR of bool_expr * bool_expr * Tools.pos
	| GREATER of alg_expr * alg_expr * Tools.pos
	| SMALLER of alg_expr * alg_expr * Tools.pos
	| EQUAL of alg_expr * alg_expr * Tools.pos
	| DIFF of alg_expr * alg_expr * Tools.pos

type mixture = 
	| COMMA of agent * mixture 
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
	| LNK_TYPE of str_pos * str_pos

type rule = {
	rule_pos: Tools.pos ; 
	lhs: mixture ; 
	rm_token: (alg_expr * str_pos) list ; 
	arrow:arrow ; 
	rhs:mixture; 
	add_token: (alg_expr * str_pos) list ; 
	k_def:alg_expr ; 
	k_un:alg_expr option ;
	k_op: alg_expr option ; (*rate for backward rule*)
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
			k_def = (match rule.k_op with None -> (FLOAT (0.,Tools.no_pos)) | Some k -> k) ;
			k_op = None
			}
	in 
	({rule_label with lbl_nme=lbl},rule)
		

type perturbation = bool_expr * (modif_expr list) * Tools.pos * bool_expr option
and modif_expr = 
	| INTRO of (alg_expr * mixture * Tools.pos) 
	| DELETE of (alg_expr * mixture * Tools.pos) 
 	| UPDATE of (str_pos * alg_expr) (*TODO: pause*)
	| UPDATE_TOK of (str_pos * alg_expr) (*TODO: pause*)
	| STOP of (print_expr list * Tools.pos)
	| SNAPSHOT of (print_expr list * Tools.pos) (*maybe later of mixture too*)
	| PRINT of ((print_expr list) * (print_expr list) * Tools.pos)
	| CFLOW of (str_pos * Tools.pos) 
	| CFLOWOFF of (str_pos * Tools.pos)
	| FLUX of print_expr list * Tools.pos
	| FLUXOFF of print_expr list * Tools.pos

and print_expr = Str_pexpr of str_pos | Alg_pexpr of alg_expr


type configuration = str_pos * (str_pos list)

type instruction = 
	| SIG of agent * Tools.pos
	| TOKENSIG of str_pos
	| VOLSIG of str_pos * float * str_pos (* type, volume, parameter*)
	| INIT of str_pos option * init_t * Tools.pos (*volume, init, position *)
	| DECLARE of variable
	| OBS of variable  (*for backward compatibility*)
	| PLOT of alg_expr
	| PERT of perturbation
	| CONFIG of configuration
and init_t = 
	| INIT_MIX of  alg_expr * mixture 
	| INIT_TOK of  alg_expr * str_pos 
and variable = 
	| VAR_KAPPA of mixture * str_pos 
	| VAR_ALG of alg_expr * str_pos 
	
type compil = {variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
							 signatures : (agent * Tools.pos) list ; (*agent signature declaration*)
							 rules : (rule_label * rule) list ; (*rules (possibly named)*)
							 observables : alg_expr list ; (*list of patterns to plot*) 
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
