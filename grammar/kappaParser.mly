%{
%}

%token EOF NEWLINE SEMICOLON
%token AT OP_PAR CL_PAR COMMA DOT TYPE_TOK LAR
%token <Tools.pos> LOG PLUS MULT MINUS AND OR GREATER SMALLER EQUAL PERT INTRO DELETE DO SET UNTIL TRUE FALSE OBS KAPPA_RAR TRACK CPUTIME CONFIG REPEAT DIFF
%token <Tools.pos> KAPPA_WLD KAPPA_SEMI SIGNATURE INFINITY TIME EVENT NULL_EVENT PROD_EVENT INIT LET DIV PLOT SINUS COSINUS TAN SQRT EXPONENT POW ABS MODULO 
%token <Tools.pos> EMAX TMAX FLUX ASSIGN ASSIGN2 TOKEN KAPPA_LNK PIPE KAPPA_LRAR PRINT
%token <int*Tools.pos> INT 
%token <string*Tools.pos> ID LABEL KAPPA_MRK  
%token <float*Tools.pos> FLOAT 
%token <string*Tools.pos> FILENAME
%token <Tools.pos> STOP SNAPSHOT

%left MINUS PLUS 
%left MULT DIV 
%left MODULO
%right POW 
%nonassoc LOG SQRT EXPONENT SINUS COSINUS ABS TAN

%left OR
%left AND

%start start_rule
%type <unit> start_rule 

%% /*Grammar rules*/

newline:
| NEWLINE start_rule
	{$2}
| EOF
	{()}
;

start_rule:
| newline
  {$1}
| rule_expression newline
	{let rule_label,r = $1 in Ast.result := {!Ast.result with Ast.rules = (rule_label,r)::!Ast.result.Ast.rules} ; $2}
| instruction newline 
	{
		let inst = $1 in
		begin 
			match inst with
				| Ast.SIG (ag,pos) -> 
						(Ast.result:={!Ast.result with 
						Ast.signatures=(ag,pos)::!Ast.result.Ast.signatures}
						)
				| Ast.TOKENSIG (str,pos) -> 
						(Ast.result:={!Ast.result with 
						Ast.tokens=(str,pos)::!Ast.result.Ast.tokens}
						)
				| Ast.INIT init_t -> (Ast.result := {!Ast.result with Ast.init=init_t::!Ast.result.Ast.init})
				| Ast.DECLARE var ->
					(Ast.result := {!Ast.result with Ast.variables = var::!Ast.result.Ast.variables})
				| Ast.OBS var -> (*for backward compatibility, shortcut for %var + %plot*)
					let expr =
						match var with
							| Ast.VAR_KAPPA (_,(label, pos)) -> Ast.OBS_VAR (label,pos)  
							| Ast.VAR_ALG (_,(label, pos)) -> Ast.OBS_VAR (label, pos)
					in					 
					(Ast.result := {!Ast.result with Ast.variables = var::!Ast.result.Ast.variables ; Ast.observables = expr::!Ast.result.Ast.observables})
				| Ast.PLOT expr ->
					(Ast.result := {!Ast.result with Ast.observables = expr::!Ast.result.Ast.observables})
				| Ast.PERT (pre,effect,pos,opt) ->
					(Ast.result := {!Ast.result with Ast.perturbations = (pre,effect,pos,opt)::!Ast.result.Ast.perturbations})
				| Ast.CONFIG (param_name,pos_p,value_list) ->
					(Ast.result := {!Ast.result with Ast.configurations = (param_name,pos_p,value_list)::!Ast.result.Ast.configurations})
		end ; $2 
	}
| error 
	{raise (ExceptionDefn.Syntax_Error (None, "Syntax error"))}
;

instruction:
| SIGNATURE agent_expression  
	{Ast.SIG ($2,$1)}
| TOKEN ID
	{let str,pos = $2 in Ast.TOKENSIG (str,pos)}
| SIGNATURE error
	{raise (ExceptionDefn.Syntax_Error (Some $1,"Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))}
| INIT multiple non_empty_mixture 
	{Ast.INIT (Ast.INIT_MIX ($2,$3,$1))}
| INIT ID LAR multiple {let str,_ = $2 in Ast.INIT (Ast.INIT_TOK ($4,str,$1))}
| INIT error
 {let pos = $1 in raise (ExceptionDefn.Syntax_Error (Some pos,"Malformed initial condition"))}
| LET variable_declaration 
	{Ast.DECLARE $2}
| OBS variable_declaration
	{Ast.OBS $2}
| PLOT alg_expr 
	{Ast.PLOT $2}
| PLOT error 
	{raise (ExceptionDefn.Syntax_Error (Some $1,"Malformed plot instruction, I was expecting an algebraic expression of variables"))}
| PERT perturbation_declaration {let (bool_expr,mod_expr_list,pos) = $2 in Ast.PERT (bool_expr,mod_expr_list,pos,None)}
| PERT REPEAT perturbation_declaration UNTIL bool_expr 
	{let (bool_expr,mod_expr_list,pos) = $3 in 
	 if List.exists 
		(fun effect -> 
			match effect with 
				| (Ast.CFLOW _ | Ast.CFLOWOFF _ | Ast.FLUX _ | Ast.FLUXOFF _) -> true 
				| _ -> false
		) mod_expr_list
	 then (ExceptionDefn.warning ~with_pos:$1 "Perturbation need not be applied repeatedly") ;
	Ast.PERT (bool_expr,mod_expr_list,pos,Some $5)}
| CONFIG FILENAME value_list 
	{let param_name,pos_p = $2 and value_list = $3 in Ast.CONFIG (param_name,pos_p,value_list)} 
| PERT bool_expr DO effect_list UNTIL bool_expr /*for backward compatibility*/
	{ExceptionDefn.warning ~with_pos:$1 "Deprecated perturbation syntax: use the 'repeat ... until' construction" ; 
	Ast.PERT ($2,$4,$1,Some $6)}
;

value_list: 
| FILENAME 
	{[$1]}
| FILENAME value_list 
	{$1::$2}
;

perturbation_declaration:
| OP_PAR perturbation_declaration CL_PAR {$2}
| bool_expr DO effect_list {($1,$3,$2)}
| bool_expr SET effect_list {ExceptionDefn.warning ~with_pos:$2 "Deprecated perturbation syntax: 'set' keyword is replaced by 'do'" ; ($1,$3,$2)} /*For backward compatibility*/
;

effect_list:
| OP_PAR effect_list CL_PAR {$2}
| effect {[$1]}
| effect SEMICOLON effect_list {$1::$3}
;

effect:
| LABEL ASSIGN alg_expr /*updating the rate of a rule -backward compatibility*/
	{let _ = ExceptionDefn.warning ~with_pos:$2 "Deprecated syntax, use $UPDATE perturbation instead of the ':=' assignment (see Manual)" in 
	let lab,pos_lab = $1 in Ast.UPDATE (lab,pos_lab,$3,$2)}
| ASSIGN2 LABEL alg_expr /*updating the rate of a rule*/
	{let lab,pos_lab = $2 in Ast.UPDATE (lab,pos_lab,$3,$1)}
| TRACK LABEL boolean 
	{let ast = if $3 then (fun x -> Ast.CFLOW x) else (fun x -> Ast.CFLOWOFF x) in let lab,pos_lab = $2 in ast (lab,pos_lab,$1)}
| FLUX fic_label boolean 
	{let ast = if $3 then (fun (x,y) -> Ast.FLUX (x,y)) else (fun (x,y) -> Ast.FLUXOFF (x,y)) in ast ($2,$1)}
| INTRO multiple_mixture 
	{let (alg,mix) = $2 in Ast.INTRO (alg,mix,$1)}
| INTRO error
	{raise (ExceptionDefn.Syntax_Error (Some $1, "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'"))}
| DELETE multiple_mixture 
	{let (alg,mix) = $2 in Ast.DELETE (alg,mix,$1)}
| DELETE error
	{raise (ExceptionDefn.Syntax_Error (Some $1,"Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'"))}
| ID LAR alg_expr /*updating the value of a token*/
	{let lab,pos_lab = $1 in Ast.UPDATE_TOK (lab,pos_lab,$3,pos_lab)}
| SNAPSHOT fic_label
	{Ast.SNAPSHOT ($2,$1)}
| STOP fic_label
	{Ast.STOP ($2,$1)}
| PRINT fic_label alg_expr
	{Ast.PRINT ($2,$3,$1)}
;


boolean:
| TRUE {true}
| FALSE {false}
;

variable_declaration:
| LABEL non_empty_mixture {Ast.VAR_KAPPA ($2,$1)}
| LABEL alg_expr {Ast.VAR_ALG ($2,$1)}
| LABEL error 
	{let str,pos = $1 in
		raise 
		(ExceptionDefn.Syntax_Error (Some pos,
		(Printf.sprintf "Variable '%s' should be either a pure kappa expression or an algebraic expression on variables" str))
		) 
	}
;

bool_expr:
| OP_PAR bool_expr CL_PAR 
	{$2}
| bool_expr AND bool_expr 
	{Ast.AND ($1,$3,$2)}
| bool_expr OR bool_expr 
	{Ast.OR ($1,$3,$2)}
| alg_expr GREATER alg_expr 
	{Ast.GREATER ($1,$3,$2)}
| alg_expr SMALLER alg_expr 
	{Ast.SMALLER ($1,$3,$2)}
| alg_expr EQUAL alg_expr 
	{Ast.EQUAL ($1,$3,$2)}
| alg_expr DIFF alg_expr  
	{Ast.DIFF ($1,$3,$2)}
| TRUE
	{Ast.TRUE $1}
| FALSE
	{Ast.FALSE $1}
;

fic_label:
/*empty*/ {None}
| FILENAME {Some $1}

multiple:
| INT {let int,pos=$1 in Ast.FLOAT (float_of_int int,pos) }
| FLOAT {let x,pos=$1 in Ast.FLOAT (x,pos) }
| LABEL {let str,pos = $1 in Ast.OBS_VAR (str,pos)}
;

rule_label: 
/*empty */
	{{Ast.lbl_nme = None ; Ast.lbl_ref = None}}
| LABEL 
	{let lab,pos = $1 in {Ast.lbl_nme=Some (lab,pos) ; Ast.lbl_ref = None}}
;

lhs_rhs:
mixture token_expr {($1,$2)}
;

token_expr:
/*empty*/ {[]}
| PIPE sum_token {$2} 
| PIPE error 
	{let pos = $1 in 
	raise (ExceptionDefn.Syntax_Error (Some pos, "Malformed token expression, I was expecting a_0 t_0 + ... + a_n t_n, where t_i are tokens and a_i any algebraic formula"))}
;

sum_token:
| OP_PAR sum_token CL_PAR 
	{$2} 
| alg_expr TYPE_TOK ID 
	{[($1,$3)]}
| alg_expr TYPE_TOK ID PLUS sum_token 
	{let l = $5 in ($1,$3)::l}

mixture:
/*empty*/ 
	{Ast.EMPTY_MIX}
| non_empty_mixture 
	{$1}
;

rule_expression:
| rule_label lhs_rhs arrow lhs_rhs AT rate 
	{ let pos = match $3 with Ast.RAR pos | Ast.LRAR pos -> pos in
		let (k2,k1,kback) = $6 in
		let _ =
			match (kback,$3) with
				| (None,Ast.LRAR pos) | (Some _,Ast.RAR pos) -> raise (ExceptionDefn.Syntax_Error (Some pos,"Malformed bi-directional rule expression"))
				| _ -> ()
		in
		let lhs,token_l = $2 and rhs,token_r = $4 in 
		($1,{Ast.rule_pos = pos ; Ast.lhs=lhs; Ast.rm_token = token_l ; Ast.arrow=$3; Ast.rhs=rhs; Ast.add_token = token_r; Ast.k_def=k2; Ast.k_un=k1; Ast.k_op=kback})
	}
| rule_label lhs_rhs arrow lhs_rhs 
	{let pos = match $3 with Ast.RAR pos | Ast.LRAR pos -> pos in
	let lhs,token_l = $2 and rhs,token_r = $4 in 
		ExceptionDefn.warning ~with_pos:pos "Rule has no kinetics. Default rate of 0.0 is assumed." ; 
		($1,{Ast.rule_pos = pos ; Ast.lhs=lhs; Ast.rm_token = token_l; Ast.arrow=$3; Ast.rhs=rhs; Ast.add_token = token_r; Ast.k_def=(Ast.FLOAT (0.0,Tools.no_pos)); Ast.k_un=None ;Ast.k_op=None})}
;

arrow:
| KAPPA_RAR 
	{Ast.RAR $1}
| KAPPA_LRAR
	{Ast.LRAR $1}
;

constant:
| INFINITY
	{Ast.INFINITY $1}
| FLOAT
	{let f,pos = $1 in Ast.FLOAT (f,pos)}
| INT 
	{let i,pos = $1 in Ast.FLOAT (float_of_int i,pos)}
| EMAX
	{let pos = $1 in Ast.EMAX pos}
| TMAX
	{let pos = $1 in Ast.TMAX pos}
| CPUTIME
	{let pos = $1 in Ast.CPUTIME pos}
;

variable:
| PIPE ID PIPE 
	{let str,pos = $2 in Ast.TOKEN_ID (str,pos)}
| LABEL 
	{let str,pos = $1 in Ast.OBS_VAR (str,pos)}
| TIME
	{Ast.TIME_VAR $1}
| EVENT
	{Ast.EVENT_VAR $1}
| NULL_EVENT
	{Ast.NULL_EVENT_VAR $1}
| PROD_EVENT
	{Ast.PROD_EVENT_VAR $1}
;

alg_expr:
| OP_PAR alg_expr CL_PAR 
	{$2}
| constant 
	{$1}
| variable
	{$1}
| alg_expr MULT alg_expr
	{Ast.MULT ($1,$3,$2)}
| alg_expr PLUS alg_expr
	{Ast.SUM ($1,$3,$2)}
| alg_expr DIV alg_expr
	{Ast.DIV ($1,$3,$2)}
| alg_expr MINUS alg_expr
	{Ast.MINUS ($1,$3,$2)}
| alg_expr POW alg_expr
	{Ast.POW ($1,$3,$2)}
| alg_expr MODULO alg_expr
	{Ast.MODULO ($1,$3,$2)}	
| EXPONENT alg_expr 
	{Ast.EXP ($2,$1)}
| SINUS alg_expr 
	{Ast.SINUS ($2,$1)}
| COSINUS alg_expr 
	{Ast.COSINUS ($2,$1)}
| TAN alg_expr 
	{Ast.TAN ($2,$1)}
| ABS alg_expr 
	{Ast.ABS ($2,$1)}
| SQRT alg_expr 
	{Ast.SQRT ($2,$1)}
| LOG alg_expr
	{Ast.LOG ($2,$1)}
;

rate:
| alg_expr OP_PAR alg_expr CL_PAR 
	{($1,Some $3,None)}
| alg_expr 
	{($1,None,None)}
| alg_expr COMMA alg_expr 
	{($1,None,Some $3)}
;

multiple_mixture:
| alg_expr non_empty_mixture /*conflict here because ID (blah) could be token non_empty mixture or mixture...*/
	{($1,$2)}
| non_empty_mixture 
	{(Ast.FLOAT (1.,Tools.no_pos),$1)}
;

non_empty_mixture:
| OP_PAR non_empty_mixture CL_PAR
	{$2}
| agent_expression COMMA non_empty_mixture  
	{Ast.COMMA ($1,$3)}
| agent_expression 
	{Ast.COMMA($1,Ast.EMPTY_MIX)}
;

agent_expression:
| ID OP_PAR interface_expression CL_PAR 
	{let (id,pos) = $1 in {Ast.ag_nme=id; Ast.ag_intf=$3; Ast.ag_pos=pos}}
| ID error 
	{let str,pos = $1 in raise (ExceptionDefn.Syntax_Error (Some pos,Printf.sprintf "Malformed agent '%s'" str))}
;

interface_expression:
/*empty*/ 
	{Ast.EMPTY_INTF}
| ne_interface_expression 
	{$1}
;

ne_interface_expression:
| port_expression COMMA ne_interface_expression 
	{Ast.PORT_SEP($1,$3)}
| port_expression  
	{Ast.PORT_SEP($1,Ast.EMPTY_INTF)}
;


port_expression:
| ID internal_state link_state 
	{let (id,pos) = $1 in {Ast.port_nme=id; Ast.port_int=$2; Ast.port_lnk=$3; Ast.port_pos=pos}}
;

internal_state:
/*empty*/ {[]}
| KAPPA_MRK internal_state 
	{let m,pos = $1 in m::$2}
| error 
	{raise (ExceptionDefn.Syntax_Error (None,"Invalid internal state"))}
;

link_state:
/*empty*/ 
	{Ast.FREE}
| KAPPA_LNK INT 
	{Ast.LNK_VALUE $2}
| KAPPA_LNK KAPPA_SEMI 
	{Ast.LNK_SOME $2}
| KAPPA_LNK ID DOT ID
	{Ast.LNK_TYPE ($2,$4)}
| KAPPA_WLD 
	{Ast.LNK_ANY $1}
| KAPPA_LNK error 
	{let pos = $1 in raise (ExceptionDefn.Syntax_Error (Some pos,"Invalid link state"))}
;

%%
