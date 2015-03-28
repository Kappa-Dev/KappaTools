%{
  open Mods

  let add_pos x = (x,(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()))
  let rhs_pos i = (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)
%}

%token EOF NEWLINE SEMICOLON COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR
%token AT TYPE LAR CPUTIME EMAX TMAX PLOTNUM PLOTENTRY
%token DO SET REPEAT UNTIL LOG PLUS MULT MINUS MAX MIN DIV SINUS COSINUS TAN
%token POW ABS MODULO SQRT EXPONENT INFINITY TIME EVENT NULL_EVENT PROD_EVENT
%token EQUAL AND OR GREATER SMALLER TRUE FALSE DIFF KAPPA_RAR KAPPA_LRAR
%token <Tools.pos> DELETE INTRO PERT OBS TRACK CONFIG
%token <Tools.pos> KAPPA_WLD KAPPA_SEMI SIGNATURE INIT LET PLOT
%token <Tools.pos> FLUX ASSIGN ASSIGN2 TOKEN KAPPA_LNK PIPE
%token <Tools.pos> PRINT PRINTF
%token <int> INT
%token <string> ID
%token <string> KAPPA_MRK LABEL
%token <float> FLOAT
%token <string*Tools.pos> STRING
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
    | NEWLINE start_rule {$2}
    | EOF {()};

start_rule:
    | newline {$1}
    | rule_expression newline
		       {Ast.result := {!Ast.result with
				       Ast.rules = $1::!Ast.result.Ast.rules};
		       $2}
    | instruction newline
		  {
		    let inst = $1 in
		    begin
		      match inst with
		      | Ast.SIG ag ->
			 (Ast.result:={!Ast.result with
					Ast.signatures=ag::!Ast.result.Ast.signatures}
			 )
		      | Ast.TOKENSIG (str_pos) ->
			 (Ast.result:={!Ast.result with
					Ast.tokens=str_pos::!Ast.result.Ast.tokens}
			 )
		      | Ast.VOLSIG (vol_type,vol,vol_param) ->
			 (Ast.result := {!Ast.result with
					  Ast.volumes=(vol_type,vol,vol_param)::!Ast.result.Ast.volumes})
		      | Ast.INIT (opt_vol,init_t,pos) ->
			 (Ast.result := {!Ast.result with
					  Ast.init=(opt_vol,init_t,pos)::!Ast.result.Ast.init})
		      | Ast.DECLARE var ->
			 (Ast.result := {!Ast.result with
					  Ast.variables = var::!Ast.result.Ast.variables})
		      | Ast.OBS ((lbl,pos),_ as var) ->
			 (*for backward compatibility, shortcut for %var + %plot*)
			 Ast.result :=
			   {!Ast.result with
			     Ast.variables = var::!Ast.result.Ast.variables;
			     Ast.observables = (Ast.OBS_VAR lbl,pos)
						 ::!Ast.result.Ast.observables}
		      | Ast.PLOT expr ->
			 (Ast.result := {!Ast.result with
					  Ast.observables = expr::!Ast.result.Ast.observables})
		      | Ast.PERT ((pre,effect,opt),pos) ->
			 (Ast.result := {!Ast.result with
					  Ast.perturbations =
					    ((pre,effect,opt),pos)
					      ::!Ast.result.Ast.perturbations})
		      | Ast.CONFIG (param_name,value_list) ->
			 (Ast.result := {!Ast.result with
					  Ast.configurations = (param_name,value_list)::!Ast.result.Ast.configurations})
		    end ; $2
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
	   {let (opt_vol,init) = $2 in Ast.INIT (opt_vol,init,$1)}
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
			   | (Ast.CFLOW _ | Ast.CFLOWOFF _
			      | Ast.FLUX _ | Ast.FLUXOFF _) -> true
			   | _ -> false
			  ) mod_expr_list
		     then
		       ExceptionDefn.warning
			 ~pos:(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
			 (fun f ->
			  Format.pp_print_string
			    f "Perturbation need not be applied repeatedly") in
	    Ast.PERT (add_pos (bool_expr,mod_expr_list,Some $5))}
    | CONFIG STRING value_list
	     {Ast.CONFIG ((fst $2,rhs_pos 2),$3)}
    | PERT bool_expr DO effect_list UNTIL bool_expr
      /* backward compatibility */
	   {ExceptionDefn.deprecated
	      ~pos:(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
	      "perturbation"
	      (fun f -> Format.pp_print_string
			  f "use the 'repeat ... until' construction");
	    Ast.PERT (add_pos ($2,$4,Some $6))}
    ;

init_declaration:
    | multiple non_empty_mixture
	       {(None,Ast.INIT_MIX ($1,$2))}
    | ID LAR multiple {(None,Ast.INIT_TOK ($3,($1,rhs_pos 1)))}
    | ID OP_CUR init_declaration CL_CUR
	 {let _,init = $3 in (Some ($1,rhs_pos 1),init)}
    ;

value_list:
    | STRING {[$1]}
    | STRING value_list {$1::$2}
    ;

perturbation_declaration:
    | OP_PAR perturbation_declaration CL_PAR {$2}
    | bool_expr DO effect_list {($1,$3)}
    | bool_expr SET effect_list
		{ExceptionDefn.deprecated
		   ~pos:(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
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
				    ~pos:(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
				    "perturbation effect"
				    (fun f ->
				     Format.pp_print_string
				       f "use $UPDATE perturbation instead of the ':=' assignment (see Manual)");
					Ast.UPDATE (($1,rhs_pos 1),$3)}
    | ASSIGN2 LABEL alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | TRACK LABEL boolean
	    {let ast = if $3 then (fun x -> Ast.CFLOW x)
		       else (fun x -> Ast.CFLOWOFF x) in
	     ast (($2,rhs_pos 2),$1)}
    | FLUX print_expr boolean
	   {if $3 then Ast.FLUX ($2,$1) else Ast.FLUXOFF ($2,$1)}
    | INTRO multiple_mixture
	    {let (alg,mix) = $2 in Ast.INTRO (alg,mix,$1)}
    | INTRO error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'"))}
    | DELETE multiple_mixture
	     {let (alg,mix) = $2 in Ast.DELETE (alg,mix,$1)}
    | DELETE error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'"))}
    | ID LAR alg_expr /*updating the value of a token*/
						{Ast.UPDATE_TOK (($1,rhs_pos 1),$3)}
    | SNAPSHOT print_expr {Ast.SNAPSHOT ($2,$1)}
    | STOP print_expr {Ast.STOP ($2,$1)}
    | PRINT SMALLER print_expr GREATER {(Ast.PRINT ([],$3,$1))}
    | PRINTF print_expr SMALLER print_expr GREATER { Ast.PRINT ($2,$4,$1) }
    | PLOTENTRY { Ast.PLOTENTRY }
    ;

print_expr:
  /*empty*/ {[]}
    | STRING {[add_pos (Ast.Str_pexpr (fst $1))]}
    | alg_expr {[add_pos (Ast.Alg_pexpr (fst $1))]}
    | STRING DOT print_expr {(add_pos (Ast.Str_pexpr (fst $1)))::$3}
    | alg_expr DOT print_expr {(add_pos (Ast.Alg_pexpr (fst $1)))::$3}
    ;

boolean:
    | TRUE {true}
    | FALSE {false}
    ;

variable_declaration:
    | LABEL non_empty_mixture
	    {let () =
	       ExceptionDefn.deprecated
		 ~pos:(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
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
    | bool_expr AND bool_expr {add_pos (Ast.BOOL_OP(Term.AND,$1,$3))}
    | bool_expr OR bool_expr {add_pos (Ast.BOOL_OP(Term.OR,$1,$3))}
    | alg_expr GREATER alg_expr {add_pos (Ast.COMPARE_OP(Term.GREATER,$1,$3))}
    | alg_expr SMALLER alg_expr {add_pos (Ast.COMPARE_OP(Term.SMALLER,$1,$3))}
    | alg_expr EQUAL alg_expr {add_pos (Ast.COMPARE_OP(Term.EQUAL,$1,$3))}
    | alg_expr DIFF alg_expr {add_pos (Ast.COMPARE_OP(Term.DIFF,$1,$3))}
    | TRUE {add_pos Ast.TRUE}
    | FALSE {add_pos Ast.FALSE}
    ;

multiple:
    | INT {add_pos (Ast.CONST (Nbr.I $1)) }
    | FLOAT {add_pos (Ast.CONST (Nbr.F $1)) }
    | LABEL {add_pos (Ast.OBS_VAR ($1)) }
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
    | rule_label lhs_rhs arrow lhs_rhs AT rate
		 { let pos =
		     (Parsing.rhs_start_pos 2, Parsing.symbol_end_pos ()) in
		   let (k2,k1,kback) = $6 in
		   let _ =
		     match kback,$3 with
		     | (None,Ast.LRAR | Some _,Ast.RAR) ->
			raise (ExceptionDefn.Malformed_Decl
				 ("Malformed bi-directional rule expression",pos))
		     | _ -> ()
		   in
		   let lhs,token_l = $2 and rhs,token_r = $4 in
		   ($1,({Ast.lhs=lhs; Ast.rm_token = token_l; Ast.arrow=$3;
			 Ast.rhs=rhs; Ast.add_token = token_r;
			 Ast.k_def=k2; Ast.k_un=k1; Ast.k_op=kback},pos))
		 }
    | rule_label lhs_rhs arrow lhs_rhs
		 {let pos =
		    (Parsing.rhs_start_pos 2, Parsing.symbol_end_pos ()) in
		  let lhs,token_l = $2 and rhs,token_r = $4 in
		  ExceptionDefn.warning
		    ~pos
		    (fun f -> Format.pp_print_string
				f "Rule has no kinetics. Default rate of 0.0 is assumed.");
		  ($1,({Ast.lhs = lhs; Ast.rm_token = token_l; Ast.arrow=$3;
			Ast.rhs=rhs; Ast.add_token = token_r;
			Ast.k_def=(Ast.CONST (Nbr.F 0.),
				   (Lexing.dummy_pos, Lexing.dummy_pos));
			Ast.k_un=None; Ast.k_op=None},pos))}
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
    | CPUTIME {add_pos (Ast.STATE_ALG_OP (Term.CPUTIME))}
    ;

variable:
    | PIPE ID PIPE {add_pos (Ast.TOKEN_ID ($2))}
    | PIPE non_empty_mixture PIPE { add_pos (Ast.KAPPA_INSTANCE $2) }
    | LABEL {add_pos (Ast.OBS_VAR ($1))}
    | TIME {add_pos (Ast.STATE_ALG_OP (Term.TIME_VAR))}
    | EVENT {add_pos (Ast.STATE_ALG_OP (Term.EVENT_VAR))}
    | NULL_EVENT {add_pos (Ast.STATE_ALG_OP (Term.NULL_EVENT_VAR))}
    | PROD_EVENT {add_pos (Ast.STATE_ALG_OP (Term.PROD_EVENT_VAR))}
    ;

small_alg_expr:
    | OP_PAR alg_expr CL_PAR {$2}
    | constant {$1}
    | variable {$1}
    | MAX small_alg_expr small_alg_expr
	  {add_pos (Ast.BIN_ALG_OP(Term.MAX,$2,$3))}
    | MIN small_alg_expr small_alg_expr
	  {add_pos (Ast.BIN_ALG_OP(Term.MIN,$2,$3))}
    | EXPONENT alg_expr {add_pos (Ast.UN_ALG_OP(Term.EXP,$2))}
    | SINUS alg_expr {add_pos (Ast.UN_ALG_OP(Term.SINUS,$2))}
    | COSINUS alg_expr {add_pos (Ast.UN_ALG_OP(Term.COSINUS,$2))}
    | TAN alg_expr {add_pos (Ast.UN_ALG_OP(Term.TAN,$2))}
    | ABS alg_expr {add_pos (Ast.UN_ALG_OP(Term.INT,$2))}
    | SQRT alg_expr {add_pos (Ast.UN_ALG_OP(Term.SQRT,$2))}
    | LOG alg_expr {add_pos (Ast.UN_ALG_OP(Term.LOG,$2))}
    ;

alg_expr:
    | MINUS alg_expr { add_pos (Ast.UN_ALG_OP(Term.UMINUS,$2)) }
    | small_alg_expr { $1 }
    | alg_expr MULT alg_expr {add_pos (Ast.BIN_ALG_OP(Term.MULT,$1,$3))}
    | alg_expr PLUS alg_expr {add_pos (Ast.BIN_ALG_OP(Term.SUM,$1,$3))}
    | alg_expr DIV alg_expr {add_pos (Ast.BIN_ALG_OP(Term.DIV,$1,$3))}
    | alg_expr MINUS alg_expr {add_pos (Ast.BIN_ALG_OP(Term.MINUS,$1,$3))}
    | alg_expr POW alg_expr {add_pos (Ast.BIN_ALG_OP(Term.POW,$1,$3))}
    | alg_expr MODULO alg_expr {add_pos (Ast.BIN_ALG_OP(Term.MODULO,$1,$3))}

rate:
    | alg_expr OP_PAR alg_with_radius CL_PAR {($1,Some $3,None)}
    | alg_expr {($1,None,None)}
    | alg_expr COMMA alg_expr {($1,None,Some $3)}
    ;

alg_with_radius:
    | alg_expr {($1,None)}
    | alg_expr TYPE alg_expr {($1,Some $3)}
    ;

multiple_mixture:
    | alg_expr non_empty_mixture {($1,$2)}
      /*conflict here because ID (blah) could be token non_empty mixture or mixture...*/
    | non_empty_mixture
	{((Ast.CONST (Nbr.F 1.),(Lexing.dummy_pos,Lexing.dummy_pos)),$1)}
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
    | KAPPA_LNK INT {(Ast.LNK_VALUE $2,rhs_pos 2)}
    | KAPPA_LNK KAPPA_SEMI {(Ast.LNK_SOME,rhs_pos 2)}
    | KAPPA_LNK ID DOT ID {add_pos (Ast.LNK_TYPE
				      (($2,rhs_pos 2),($4,rhs_pos 4)))}
    | KAPPA_WLD {add_pos Ast.LNK_ANY}
    | KAPPA_LNK error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Invalid link state"))}
;

%%
