%{
  let add_pos x =
    (x,Location.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
  let rhs_pos i =
  Location.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
%}

%token EOF NEWLINE SEMICOLON COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR AT TYPE LAR
%token CPUTIME EMAX TMAX PLOTNUM PLOTENTRY DELETE INTRO TRACK DO SET REPEAT
%token UNTIL LOG PLUS MULT MINUS MAX MIN DIV SINUS COSINUS TAN POW ABS MODULO
%token SQRT EXPONENT INFINITY TIME EVENT NULL_EVENT PIPE EQUAL AND OR
%token GREATER SMALLER TRUE FALSE DIFF KAPPA_RAR KAPPA_LRAR KAPPA_LNK
%token SIGNATURE INIT LET PLOT PERT OBS TOKEN CONFIG KAPPA_WLD KAPPA_SEMI
%token FLUX ASSIGN ASSIGN2 PRINT PRINTF STOP SNAPSHOT
%token <int> INT
%token <string> ID
%token <string> KAPPA_MRK LABEL
%token <float> FLOAT
%token <string> STRING

%left MINUS PLUS
%left MULT DIV
%left MODULO
%right POW
%nonassoc LOG SQRT EXPONENT SINUS COSINUS ABS TAN

%left OR
%left AND

%start start_rule
%type <(Ast.agent,Ast.mixture,string,Ast.rule) Ast.compil -> (Ast.agent,Ast.mixture,string,Ast.rule) Ast.compil> start_rule

%% /*Grammar rules*/

newline:
    | NEWLINE start_rule {$2}
    | EOF {fun c -> c};

start_rule:
    | newline {$1}
    | rule_expression newline
        {fun c -> let r = $2 c in {r with Ast.rules = $1::r.Ast.rules}}
    | instruction newline
		  { fun c -> let r = $2 c in
		      match $1 with
		      | Ast.SIG ag ->
			 {r with Ast.signatures=ag::r.Ast.signatures}
		      | Ast.TOKENSIG (str_pos) ->
			 {r with Ast.tokens=str_pos::r.Ast.tokens}
		      | Ast.VOLSIG (vol_type,vol,vol_param) ->
			 {r with Ast.volumes=(vol_type,vol,vol_param)::r.Ast.volumes}
		      | Ast.INIT (opt_vol,init_t) ->
			 {r with Ast.init=(opt_vol,init_t)::r.Ast.init}
		      | Ast.DECLARE var ->
			 {r with Ast.variables = var::r.Ast.variables}
		      | Ast.OBS ((lbl,pos),_ as var) ->
			 (*for backward compatibility, shortcut for %var + %plot*)
			   {r with
			     Ast.variables = var::r.Ast.variables;
			     Ast.observables = (Ast.OBS_VAR lbl,pos)
						 ::r.Ast.observables}
		      | Ast.PLOT expr ->
			 {r with Ast.observables = expr::r.Ast.observables}
		      | Ast.PERT ((pre,effect,opt),pos) ->
			 {r with
			  Ast.perturbations =
			   ((pre,effect,opt),pos)::r.Ast.perturbations}
		      | Ast.CONFIG (param_name,value_list) ->
			 {r with
			  Ast.configurations = (param_name,value_list)::r.Ast.configurations}
		  }
    | error
	{raise (ExceptionDefn.Syntax_Error (add_pos "Syntax error"))}
    ;

instruction:
    | SIGNATURE agent_expression {Ast.SIG $2}
    | TOKEN ID {Ast.TOKENSIG ($2,rhs_pos 2)}
    | SIGNATURE error {raise (ExceptionDefn.Syntax_Error
				(add_pos "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))}

    | INIT init_declaration
	   {let (opt_vol,init) = $2 in Ast.INIT (opt_vol,init)}
    | INIT error
	{ raise (ExceptionDefn.Syntax_Error
		   (add_pos "Malformed initial condition"))}

    | LET variable_declaration {Ast.DECLARE $2}
    | OBS variable_declaration {Ast.OBS $2}
    | PLOT alg_expr {Ast.PLOT $2}
    | PLOT error {raise (ExceptionDefn.Syntax_Error
			   (add_pos "Malformed plot instruction, I was expecting an algebraic expression of variables"))}
    | PERT perturbation_declaration
	   {let (bool_expr,mod_expr_list) = $2 in
	    Ast.PERT (add_pos (bool_expr,mod_expr_list,None))}
    | PERT REPEAT perturbation_declaration UNTIL bool_expr
	   {let (bool_expr,mod_expr_list) = $3 in
	    let () = if List.exists
			  (fun effect ->
			   match effect with
			   | (Ast.CFLOWLABEL _ | Ast.CFLOWMIX _
			      | Ast.FLUX _ | Ast.FLUXOFF _) -> true
			   | (Ast.STOP _ | Ast.INTRO _ | Ast.DELETE _
			     | Ast.UPDATE _ | Ast.UPDATE_TOK _ | Ast.PRINT _
			     | Ast.SNAPSHOT _ | Ast.PLOTENTRY) -> false
			  ) mod_expr_list
		     then
		       ExceptionDefn.warning
			 ~pos:(Location.of_pos (Parsing.symbol_start_pos ())
					       (Parsing.symbol_end_pos ()))
			 (fun f ->
			  Format.pp_print_string
			    f "Perturbation need not be applied repeatedly") in
	    Ast.PERT (add_pos (bool_expr,mod_expr_list,Some $5))}
    | CONFIG STRING value_list
	     {Ast.CONFIG (($2,rhs_pos 2),$3)}
    | PERT bool_expr DO effect_list UNTIL bool_expr
      /* backward compatibility */
	   {ExceptionDefn.deprecated
	      ~pos:(Location.of_pos (Parsing.symbol_start_pos ())
				    (Parsing.symbol_end_pos ()))
	      "perturbation"
	      (fun f -> Format.pp_print_string
			  f "use the 'repeat ... until' construction");
	    Ast.PERT (add_pos ($2,$4,Some $6))}
    ;

init_declaration:
    | alg_expr non_empty_mixture
	       {(None,Ast.INIT_MIX ($1,($2,rhs_pos 2)))}
    | ID LAR alg_expr {(None,Ast.INIT_TOK ($3,($1,rhs_pos 1)))}
    | ID OP_CUR init_declaration CL_CUR
	 {let _,init = $3 in (Some ($1,rhs_pos 1),init)}
    ;

value_list:
    | STRING {[$1, rhs_pos 1]}
    | STRING value_list {($1,rhs_pos 1)::$2}
    ;

perturbation_declaration:
    | OP_PAR perturbation_declaration CL_PAR {$2}
    | bool_expr DO effect_list {($1,$3)}
    | bool_expr SET effect_list
		{ExceptionDefn.deprecated
		   ~pos:(Location.of_pos (Parsing.symbol_start_pos ())
					 (Parsing.symbol_end_pos ()))
		   "perturbation"
		   (fun f -> Format.pp_print_string
			       f "'set' keyword is replaced by 'do'");
		 ($1,$3)} /*For backward compatibility*/
    ;

effect_list:
    | OP_PAR effect_list CL_PAR {$2}
    | effect {[$1]}
    | effect SEMICOLON effect_list {$1::$3}
    ;

effect:
    | LABEL ASSIGN alg_expr
      /*updating the rate of a rule -backward compatibility*/
				{
				  ExceptionDefn.deprecated
				    ~pos:(Location.of_pos (Parsing.symbol_start_pos ())
							  (Parsing.symbol_end_pos ()))
				    "perturbation effect"
				    (fun f ->
				     Format.pp_print_string
				       f "use $UPDATE perturbation instead of the ':=' assignment (see Manual)");
					Ast.UPDATE (($1,rhs_pos 1),$3)}
    | ASSIGN2 LABEL alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | TRACK LABEL boolean
	    {Ast.CFLOWLABEL ($3,($2,rhs_pos 2))}
    | TRACK non_empty_mixture boolean
	    {Ast.CFLOWMIX ($3,($2,rhs_pos 2))}
    | FLUX print_expr boolean
	   {if $3 then Ast.FLUX $2 else Ast.FLUXOFF $2}
    | INTRO multiple_mixture
	    {let (alg,mix) = $2 in Ast.INTRO (alg,mix)}
    | INTRO error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'"))}
    | DELETE multiple_mixture
	     {let (alg,mix) = $2 in Ast.DELETE (alg,mix)}
    | DELETE error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'"))}
    | ID LAR alg_expr /*updating the value of a token*/
						{Ast.UPDATE_TOK (($1,rhs_pos 1),$3)}
    | SNAPSHOT print_expr {Ast.SNAPSHOT $2}
    | STOP print_expr {Ast.STOP $2}
    | PRINT SMALLER print_expr GREATER {(Ast.PRINT ([],$3))}
    | PRINTF print_expr SMALLER print_expr GREATER { Ast.PRINT ($2,$4) }
    | PLOTENTRY { Ast.PLOTENTRY }
    ;

print_expr:
  /*empty*/ {[]}
    | STRING {[Ast.Str_pexpr (add_pos $1)]}
    | alg_expr {[Ast.Alg_pexpr $1]}
    | STRING DOT print_expr {Ast.Str_pexpr ($1, rhs_pos 1)::$3}
    | alg_expr DOT print_expr {Ast.Alg_pexpr $1::$3}
    ;

boolean:
    | TRUE {true}
    | FALSE {false}
    ;

variable_declaration:
    | LABEL non_empty_mixture
	    {let () =
	       ExceptionDefn.deprecated
		~pos:(Location.of_pos (Parsing.symbol_start_pos ())
				      (Parsing.symbol_end_pos ()))
		 "variable"
		 (fun f -> Format.pp_print_string
			     f "use |kappa instance| instead.")
	      in
	      (($1,rhs_pos 1),(Ast.KAPPA_INSTANCE $2,rhs_pos 2))}
    | LABEL alg_expr {(($1,rhs_pos 1),$2)}
    | LABEL error
	    {raise
	       (ExceptionDefn.Syntax_Error
		  (add_pos ("Illegal definition of variable '"^$1^"'")))
	    }
    ;

bool_expr:
    | OP_PAR bool_expr CL_PAR {$2}
    | bool_expr AND bool_expr {add_pos (Ast.BOOL_OP(Operator.AND,$1,$3))}
    | bool_expr OR bool_expr {add_pos (Ast.BOOL_OP(Operator.OR,$1,$3))}
    | alg_expr GREATER alg_expr {add_pos (Ast.COMPARE_OP(Operator.GREATER,$1,$3))}
    | alg_expr SMALLER alg_expr {add_pos (Ast.COMPARE_OP(Operator.SMALLER,$1,$3))}
    | alg_expr EQUAL alg_expr {add_pos (Ast.COMPARE_OP(Operator.EQUAL,$1,$3))}
    | alg_expr DIFF alg_expr {add_pos (Ast.COMPARE_OP(Operator.DIFF,$1,$3))}
    | TRUE {add_pos Ast.TRUE}
    | FALSE {add_pos Ast.FALSE}
    ;

rule_label:
  /*empty */
      {None}
    | LABEL
	{Some (add_pos $1)}
    ;

lhs_rhs:
  mixture token_expr {($1,$2)};

token_expr:
  /*empty*/ {[]}
    | PIPE sum_token {$2}
    | PIPE error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos  "Malformed token expression, I was expecting a_0 t_0 + ... + a_n t_n, where t_i are tokens and a_i any algebraic formula"))}
    ;

sum_token:
    | OP_PAR sum_token CL_PAR {$2}
    | alg_expr TYPE ID {[($1,($3,rhs_pos 3))]}
    | alg_expr TYPE ID PLUS sum_token {let l = $5 in ($1,($3,rhs_pos 3))::l}

mixture:
      /*empty*/ {[]}
    | non_empty_mixture {$1}
;

rule_expression:
    | rule_label lhs_rhs arrow lhs_rhs birate
		 { let pos =
		     Location.of_pos (Parsing.rhs_start_pos 2)
				     (Parsing.symbol_end_pos ()) in
		   let (k2,k1,kback,kback1) = $5 in
		   let lhs,token_l = $2 and rhs,token_r = $4 in
		   ($1,({Ast.lhs=lhs; Ast.rm_token = token_l; Ast.arrow=$3;
			 Ast.rhs=rhs; Ast.add_token = token_r;
			 Ast.k_def=k2; Ast.k_un=k1; Ast.k_op=kback; Ast.k_op_un=kback1},pos))
		 }
    ;

arrow:
    | KAPPA_RAR {Ast.RAR}
    | KAPPA_LRAR {Ast.LRAR}
    ;

constant:
    | INFINITY {add_pos (Ast.CONST (Nbr.F infinity))}
    | FLOAT {add_pos (Ast.CONST (Nbr.F $1))}
    | INT {add_pos (Ast.CONST (Nbr.I $1))}
    | EMAX {add_pos Ast.EMAX}
    | TMAX {add_pos Ast.TMAX}
    | PLOTNUM {add_pos Ast.PLOTNUM}
    | CPUTIME {add_pos (Ast.STATE_ALG_OP (Operator.CPUTIME))}
    ;

variable:
    | PIPE ID PIPE {add_pos (Ast.TOKEN_ID ($2))}
    | PIPE non_empty_mixture PIPE { add_pos (Ast.KAPPA_INSTANCE $2) }
    | LABEL {add_pos (Ast.OBS_VAR ($1))}
    | TIME {add_pos (Ast.STATE_ALG_OP (Operator.TIME_VAR))}
    | EVENT {add_pos (Ast.STATE_ALG_OP (Operator.EVENT_VAR))}
    | NULL_EVENT {add_pos (Ast.STATE_ALG_OP (Operator.NULL_EVENT_VAR))}
    ;

small_alg_expr:
    | OP_PAR alg_expr CL_PAR {$2}
    | constant {$1}
    | variable {$1}
    | MAX small_alg_expr small_alg_expr
	  {add_pos (Ast.BIN_ALG_OP(Operator.MAX,$2,$3))}
    | MIN small_alg_expr small_alg_expr
	  {add_pos (Ast.BIN_ALG_OP(Operator.MIN,$2,$3))}
    | EXPONENT alg_expr {add_pos (Ast.UN_ALG_OP(Operator.EXP,$2))}
    | SINUS alg_expr {add_pos (Ast.UN_ALG_OP(Operator.SINUS,$2))}
    | COSINUS alg_expr {add_pos (Ast.UN_ALG_OP(Operator.COSINUS,$2))}
    | TAN alg_expr {add_pos (Ast.UN_ALG_OP(Operator.TAN,$2))}
    | ABS alg_expr {add_pos (Ast.UN_ALG_OP(Operator.INT,$2))}
    | SQRT alg_expr {add_pos (Ast.UN_ALG_OP(Operator.SQRT,$2))}
    | LOG alg_expr {add_pos (Ast.UN_ALG_OP(Operator.LOG,$2))}
    ;

alg_expr:
    | MINUS alg_expr { add_pos (Ast.UN_ALG_OP(Operator.UMINUS,$2)) }
    | small_alg_expr { $1 }
    | alg_expr MULT alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.MULT,$1,$3))}
    | alg_expr PLUS alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.SUM,$1,$3))}
    | alg_expr DIV alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.DIV,$1,$3))}
    | alg_expr MINUS alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.MINUS,$1,$3))}
    | alg_expr POW alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.POW,$1,$3))}
    | alg_expr MODULO alg_expr {add_pos (Ast.BIN_ALG_OP(Operator.MODULO,$1,$3))}

birate:
    | AT rate {let (k2,k1) = $2 in (k2,k1,None,None)}
    | AT rate COMMA rate {let (k2,k1) = $2 in 
			  let (kback,kback1) = $4 in
			  (k2,k1,Some kback,kback1)}
    | {let pos =
         Location.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()) in
       let () = ExceptionDefn.warning
		  ~pos
		  (fun f -> Format.pp_print_string
			      f "Rule has no kinetics. Default rate of 0.0 is assumed.") in
       (Location.dummy_annot (Ast.CONST (Nbr.F 0.)),None,None,None)}
    ;

rate:
    | alg_expr OP_PAR alg_with_radius CL_PAR {($1,Some $3)}
    | alg_expr {($1,None)}
    ;

alg_with_radius:
    | alg_expr {($1,None)}
    | alg_expr TYPE INT {($1,Some (add_pos $3))}
    ;

multiple_mixture:
    | alg_expr non_empty_mixture {($1,($2, rhs_pos 2))}
      /*conflict here because ID (blah) could be token non_empty mixture or mixture...*/
    | non_empty_mixture
	{(Location.dummy_annot (Ast.CONST (Nbr.one)),add_pos $1)}
    ;

non_empty_mixture:
    | OP_PAR non_empty_mixture CL_PAR {$2}
    | agent_expression COMMA non_empty_mixture {$1 :: $3}
    | agent_expression {[$1]}
    ;

agent_expression:
    | ID OP_PAR interface_expression CL_PAR
	 {(($1,rhs_pos 1), $3)}
    | ID error
	 { raise (ExceptionDefn.Syntax_Error
		    (add_pos ("Malformed agent '"^$1^"'")))}
    ;

interface_expression:
  /*empty*/ {[]}
    | ne_interface_expression {$1}
    ;

ne_interface_expression:
    | port_expression COMMA ne_interface_expression {$1::$3}
    | port_expression {[$1]}
    ;


port_expression:
    | ID internal_state link_state
	 { {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=$3}}
    ;

internal_state:
  /*empty*/ {[]}
    | KAPPA_MRK internal_state {add_pos $1::$2}
    | error
	{raise (ExceptionDefn.Syntax_Error (add_pos "Invalid internal state"))}
    ;

link_state:
  /*empty*/ {add_pos Ast.FREE}
    | KAPPA_LNK INT {(Ast.LNK_VALUE ($2,()),rhs_pos 2)}
    | KAPPA_LNK KAPPA_SEMI {(Ast.LNK_SOME,rhs_pos 2)}
    | KAPPA_LNK ID DOT ID {add_pos (Ast.LNK_TYPE
				      (($2,rhs_pos 2),($4,rhs_pos 4)))}
    | KAPPA_WLD {add_pos Ast.LNK_ANY}
    | KAPPA_LNK error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Invalid link state"))}
;

%%
