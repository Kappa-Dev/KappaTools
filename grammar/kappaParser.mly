%{
%}

%token EOF NEWLINE 
%token AT OP_PAR CL_PAR OP_BRA CL_BRA COMMA DOT KAPPA_LNK PIPE
%token <Tools.pos> LOG PLUS MULT MINUS AND OR GREATER SMALLER EQUAL NOT PERT INTRO DELETE SET DO UNTIL TRUE FALSE OBS KAPPA_RAR TRACK CPUTIME CONFIG
%token <Tools.pos> KAPPA_WLD KAPPA_SEMI SIGNATURE INFINITY TIME EVENT NULL_EVENT PROD_EVENT INIT LET DIV PLOT SINUS COSINUS TAN SQRT EXPONENT POW ABS MODULO 
%token <Tools.pos> EMAX TMAX FLUX ENABLE DISABLE ASSIGN TOKEN INITIALIZE
%token <int*Tools.pos> INT 
%token <string*Tools.pos> ID LABEL KAPPA_MRK 
%token <int> DOT_RADIUS PLUS_RADIUS 
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
%nonassoc NOT

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
				| Ast.TOKEN (str,pos) -> 
						(Ast.result:={!Ast.result with 
						Ast.tokens=(str,pos)::!Ast.result.Ast.tokens}
						)
				| Ast.INIT init_t -> (Ast.result := {!Ast.result with Ast.init=init_t::!Ast.result.Ast.init})
				| Ast.DECLARE var ->
					(Ast.result := {!Ast.result with Ast.variables = var::!Ast.result.Ast.variables})
				| Ast.OBS var -> (*for backward compatibility, shortcut for %var + %plot*)
					let expr =
						match var with
							| Ast.VAR_KAPPA (_,(label, pos)) | Ast.VAR_ALG (_,(label, pos)) -> Ast.OBS_VAR (label, pos)
					in					 
					(Ast.result := {!Ast.result with Ast.variables = var::!Ast.result.Ast.variables ; Ast.observables = expr::!Ast.result.Ast.observables})
				| Ast.PLOT expr ->
					(Ast.result := {!Ast.result with Ast.observables = expr::!Ast.result.Ast.observables})
				| Ast.PERT (pre,effect,pos,opt) ->
					(Ast.result := {!Ast.result with Ast.perturbations = (pre,effect,pos,opt)::!Ast.result.Ast.perturbations})
				| Ast.CONFIG (param_name,pos_p,value,pos_v) ->
					(Ast.result := {!Ast.result with Ast.configurations = (param_name,pos_p,value,pos_v)::!Ast.result.Ast.configurations})
		end ; $2 
	}
| error 
	{raise (ExceptionDefn.Syntax_Error "Syntax error")}
;

instruction:
| SIGNATURE agent_expression  
	{Ast.SIG ($2,$1)}
| TOKEN ID
	{let str,pos = $2 in Ast.TOKEN (str,pos)}
| SIGNATURE error
	{raise (ExceptionDefn.Syntax_Error "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'")}
| INIT multiple non_empty_mixture 
	{Ast.INIT (Ast.INIT_MIX ($2,$3,$1))}
| INIT ID INITIALIZE multiple {let str,_ = $2 in Ast.INIT (Ast.INIT_TOK ($4,str,$1))}
| INIT error
 {raise (ExceptionDefn.Syntax_Error "Malformed initial condition, I was expecting something of the form '%init: n or 'v' kappa_expression' where n is a value or 'v' is a constant")}
| LET variable_declaration 
	{Ast.DECLARE $2}
| OBS variable_declaration
	{Ast.OBS $2}
| PLOT alg_expr 
	{Ast.PLOT $2}
| PLOT error 
	{raise (ExceptionDefn.Syntax_Error "Malformed plot instruction, I was expecting an algebraic expression of variables")}
| PERT bool_expr DO one_shot_effect 
	{Ast.PERT ($2,$4,$1,None)}
| PERT bool_expr DO one_shot_effect UNTIL bool_expr
	{Ast.PERT ($2,$4,$1,Some $6)}
| CONFIG FILENAME FILENAME 
	{let param_name,pos_p = $2 and value,pos_v = $3 in Ast.CONFIG (param_name,pos_p,value,pos_v)} 
| PERT bool_expr SET perm_effect
	{Ast.PERT ($2,$4,$1,None)}
;

perm_effect:
| OP_PAR perm_effect CL_PAR {$2}
| ASSIGN LABEL alg_expr 
	{let lab,pos_lab = $2 in Ast.UPDATE (lab,pos_lab,$3,$1)}
| TRACK LABEL boolean 
	{let ast = if $3 then (fun x -> Ast.CFLOW x) else (fun x -> Ast.CFLOWOFF x) in let lab,pos_lab = $2 in ast (lab,pos_lab,$1)}
| FLUX fic_label boolean 
	{let ast = if $3 then (fun (x,y) -> Ast.FLUX (x,y)) else (fun (x,y) -> Ast.FLUXOFF (x,y)) in ast ($2,$1)}
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
		(ExceptionDefn.Syntax_Error 
		(Printf.sprintf "Variable '%s' should be either a pure kappa expression or an algebraic expression on variables" str)
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
| NOT bool_expr 
	{Ast.NOT ($2,$1)}
| TRUE
	{Ast.TRUE $1}
| FALSE
	{Ast.FALSE $1}
;


one_shot_effect:
| OP_PAR one_shot_effect CL_PAR  
	{$2}
| INTRO multiple_mixture 
	{let (alg,mix) = $2 in Ast.INTRO (alg,mix,$1)}
| INTRO error
	{raise (ExceptionDefn.Syntax_Error "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'")}
| DELETE multiple_mixture 
	{let (alg,mix) = $2 in Ast.DELETE (alg,mix,$1)}
| DELETE error
	{raise (ExceptionDefn.Syntax_Error "Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'")}
| SNAPSHOT fic_label
	{Ast.SNAPSHOT ($2,$1)}
| STOP fic_label
	{Ast.STOP ($2,$1)}

;

fic_label:
/*empty*/ {None}
| FILENAME {Some $1}

multiple:
/*empty*/ {Ast.FLOAT (1.0,Tools.no_pos)}
| INT {let int,pos=$1 in Ast.FLOAT (float_of_int int,pos) }
| LABEL {Ast.OBS_VAR $1}
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
;

sum_token:
| OP_PAR sum_token CL_PAR 
	{$2} 
| ID 
	{[$1]}
| ID PLUS sum_token 
	{let l = $3 in $1::l}
;

mixture:
/*empty*/ 
	{Ast.EMPTY_MIX}
| non_empty_mixture 
	{$1}
;

rule_expression:
| rule_label lhs_rhs arrow lhs_rhs AT rate 
	{ let pos = match $3 with Ast.RAR pos -> pos in
		let (k2,k1) = $6 in
		let lhs,token_l = $2 and rhs,token_r = $4 in 
		($1,{Ast.rule_pos = pos ; Ast.lhs=lhs; Ast.rm_token = token_l ; Ast.arrow=$3; Ast.rhs=rhs; Ast.add_token = token_r; Ast.k_def=k2; Ast.k_un=k1})
	}
| rule_label lhs_rhs arrow lhs_rhs 
	{let pos = match $3 with Ast.RAR pos -> pos in
	let lhs,token_l = $2 and rhs,token_r = $4 in 
		ExceptionDefn.warning ~with_pos:pos "Rule has no kinetics. Default rate of 0.0 is assumed." ; 
		($1,{Ast.rule_pos = pos ; Ast.lhs=lhs; Ast.rm_token = token_l; Ast.arrow=$3; Ast.rhs=rhs; Ast.add_token = token_r; Ast.k_def=(Ast.FLOAT (0.0,Tools.no_pos)); Ast.k_un=None})}
;

arrow:
| KAPPA_RAR 
	{Ast.RAR $1}
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
| LABEL 
	{Ast.OBS_VAR $1}
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
	{($1,Some $3)}
| alg_expr 
	{($1,None)}
;

multiple_mixture:
| alg_expr non_empty_mixture 
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
| ID 
	{let (id,pos) = $1 in {Ast.ag_nme=id;Ast.ag_intf=Ast.EMPTY_INTF;Ast.ag_pos=pos}}
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


/*ne_interface_expression:
| ne_interface_expression COMMA port_expression
	{Ast.PORT_SEP($3,$1)}
| port_expression  
	{Ast.PORT_SEP($1,Ast.EMPTY_INTF)}
;*/

port_expression:
| ID internal_state link_state 
	{let (id,pos) = $1 in {Ast.port_nme=id; Ast.port_int=$2; Ast.port_lnk=$3; Ast.port_pos=pos}}
;

internal_state:
/*empty*/ {[]}
| KAPPA_MRK internal_state 
	{let m,pos = $1 in m::$2}
| error 
	{raise (ExceptionDefn.Syntax_Error "Invalid internal state")}
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
	{raise (ExceptionDefn.Syntax_Error "Invalid link state")}
;

%%
