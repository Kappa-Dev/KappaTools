/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  let add_pos x =
    (x,Locality.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
  let rhs_pos i =
  Locality.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
%}

%token EOF NEWLINE SEMICOLON COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR AT TYPE LAR
%token CPUTIME EMAX TMAX PLOTENTRY DELETE INTRO TRACK DO SET REPEAT SPECIES_OF
%token UNTIL LOG PLUS MULT MINUS MAX MIN DIV SINUS COSINUS TAN POW ABS MODULO
%token SQRT EXPONENT INFINITY TIME EVENT NULL_EVENT PIPE EQUAL AND OR
%token GREATER SMALLER TRUE FALSE DIFF KAPPA_RAR KAPPA_LRAR KAPPA_LNK
%token SIGNATURE INIT LET PLOT PERT OBS TOKEN CONFIG KAPPA_WLD KAPPA_SEMI
%token FLUX ASSIGN PRINTF STOP SNAPSHOT RUN THEN ELSE
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

%nonassoc THEN

%start start_rule
%type <Ast.parsing_compil -> Ast.parsing_compil> start_rule

%start interactive_command
%type <(Ast.mixture,string) Ast.command> interactive_command

%start standalone_effect_list
%type <(Ast.mixture,string) Ast.modif_expr list> standalone_effect_list

%start standalone_bool_expr
%type <(Ast.mixture,string) Alg_expr.bool Locality.annot> standalone_bool_expr

%% /*Grammar rules*/

newline:
    | NEWLINE start_rule {$2}
    | EOF {fun c -> c};

start_rule:
    | newline {$1}
    | rule_expression newline
        {fun c -> let r = $2 c in {r with Ast.rules = $1::r.Ast.rules}}
    | edit_rule_expression newline
        {fun c -> let r = $2 c in
	{r with Ast.edit_rules = $1::r.Ast.edit_rules}}
    | instruction newline
		  { fun c -> let r = $2 c in
		      match $1 with
		      | Ast.SIG ag ->
			 {r with Ast.signatures=ag::r.Ast.signatures}
		      | Ast.TOKENSIG (str_pos) ->
			 {r with Ast.tokens=str_pos::r.Ast.tokens}
		      | Ast.VOLSIG (vol_type,vol,vol_param) ->
			 {r with Ast.volumes=(vol_type,vol,vol_param)::r.Ast.volumes}
		      | Ast.INIT (opt_vol,alg,init_t) ->
			 {r with Ast.init=(opt_vol,alg,init_t)::r.Ast.init}
		      | Ast.DECLARE var ->
			 {r with Ast.variables = var::r.Ast.variables}
		      | Ast.OBS ((lbl,pos),_ as var) ->
			 (*for backward compatibility, shortcut for %var + %plot*)
			   {r with
			     Ast.variables = var::r.Ast.variables;
			     Ast.observables = (Alg_expr.ALG_VAR lbl,pos)
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
	   {let (opt_vol,alg,init) = $2 in Ast.INIT (opt_vol,alg,init)}
    | INIT error
	{ raise (ExceptionDefn.Syntax_Error
		   (add_pos "Malformed initial condition"))}

    | LET variable_declaration {Ast.DECLARE $2}
    | OBS variable_declaration {Ast.OBS $2}
    | PLOT alg_expr {Ast.PLOT $2}
    | PLOT error {raise (ExceptionDefn.Syntax_Error
			   (add_pos "Malformed plot instruction, an algebraic expression is expected"))}
    | PERT perturbation_declaration
	   {let (bool_expr,mod_expr_list) = $2 in
	    Ast.PERT (add_pos (bool_expr,mod_expr_list,None))}
    | PERT REPEAT perturbation_declaration UNTIL bool_expr
	   {let (bool_expr,mod_expr_list) = $3 in
	    let () = if List.exists
			  (fun effect ->
			   match effect with
			   | (Ast.CFLOWLABEL _ | Ast.CFLOWMIX _
			      | Ast.FLUX _ | Ast.FLUXOFF _
			      | Ast.SPECIES_OF _) -> true
			   | (Ast.STOP _ | Ast.INTRO _ | Ast.DELETE _
			     | Ast.UPDATE _ | Ast.UPDATE_TOK _ | Ast.PRINT _
			     | Ast.SNAPSHOT _ | Ast.PLOTENTRY) -> false
			  ) mod_expr_list
		     then
		       ExceptionDefn.warning
			 ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
					       (Parsing.symbol_end_pos ()))
			 (fun f ->
			  Format.pp_print_string
			    f "Perturbation need not be applied repeatedly") in
	    Ast.PERT (add_pos (bool_expr,mod_expr_list,Some $5))}
    | PERT REPEAT perturbation_declaration error
	{ raise (ExceptionDefn.Syntax_Error
		   (add_pos "Expect \"until\" statement"))}
    | CONFIG STRING value_list
	     {Ast.CONFIG (($2,rhs_pos 2),$3)}
    | PERT bool_expr DO effect_list UNTIL bool_expr
      /* backward compatibility */
	   {ExceptionDefn.deprecated
	      ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
				    (Parsing.symbol_end_pos ()))
	      "perturbation"
	      (fun f -> Format.pp_print_string
			  f "use the 'repeat ... until' construction");
	    Ast.PERT (add_pos ($2,$4,Some $6))}
    ;

init_declaration:
    | alg_expr non_empty_mixture
	       {(None,$1,(Ast.INIT_MIX $2,rhs_pos 2))}
    | ID LAR alg_expr {(None,$3,(Ast.INIT_TOK $1,rhs_pos 1))}
    | ID OP_CUR init_declaration CL_CUR
	 {let _,alg,init = $3 in (Some ($1,rhs_pos 1),alg,init)}
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
		   ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
					 (Parsing.symbol_end_pos ()))
		   "perturbation"
		   (fun f -> Format.pp_print_string
			       f "'set' keyword is replaced by 'do'");
		 ($1,$3)} /*For backward compatibility*/
    ;

standalone_effect_list: effect_list EOF {$1}

effect_list:
    | OP_PAR effect_list CL_PAR {$2}
    | effect {[$1]}
    | effect SEMICOLON effect_list {$1::$3}
    ;

effect:
    | ASSIGN ID alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | ASSIGN LABEL alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | TRACK LABEL boolean
	    {Ast.CFLOWLABEL ($3,($2,rhs_pos 2))}
    | TRACK non_empty_mixture boolean
	    {Ast.CFLOWMIX ($3,($2,rhs_pos 2))}
    | FLUX nonempty_print_expr boolean
	   {if $3 then Ast.FLUX (Primitives.RELATIVE,$2) else Ast.FLUXOFF $2}
    | FLUX nonempty_print_expr STRING boolean
	   {if $4 && $3 = "absolute" then Ast.FLUX (Primitives.ABSOLUTE,$2)
	   else if $4 && $3 = "probability" then
	     Ast.FLUX (Primitives.PROBABILITY,$2)
	   else if $4 && $3 = "relative" then Ast.FLUX (Primitives.RELATIVE,$2)
	     else raise (ExceptionDefn.Syntax_Error
	       ("Incorrect FLUX expression",rhs_pos 3))}
    | INTRO alg_expr non_empty_mixture {Ast.INTRO ($2,($3, rhs_pos 3))}
    | INTRO error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'"))}
    | DELETE alg_expr non_empty_mixture {Ast.DELETE ($2,($3, rhs_pos 3))}
    | DELETE error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'"))}
    | ID LAR alg_expr /*updating the value of a token*/
						{Ast.UPDATE_TOK (($1,rhs_pos 1),$3)}
    | SNAPSHOT print_expr {Ast.SNAPSHOT $2}
    | STOP print_expr {Ast.STOP $2}
    | PRINTF print_expr SMALLER print_expr GREATER { Ast.PRINT ($2,$4) }
    | PLOTENTRY { Ast.PLOTENTRY }
    | SPECIES_OF nonempty_print_expr non_empty_mixture boolean { Ast.SPECIES_OF ($4,$2,($3, rhs_pos 3))}
    ;

nonempty_print_expr:
    | STRING {[Primitives.Str_pexpr (add_pos $1)]}
    | mid_alg_expr {[Primitives.Alg_pexpr $1]}
    | STRING DOT nonempty_print_expr {Primitives.Str_pexpr ($1, rhs_pos 1)::$3}
    | mid_alg_expr DOT nonempty_print_expr {Primitives.Alg_pexpr $1::$3}
    ;
print_expr:
    /*empty*/ {[]}
    | nonempty_print_expr {$1}

boolean:
    | TRUE {true}
    | FALSE {false}
    ;

variable_declaration:
    | LABEL non_empty_mixture
	    {let () =
	       ExceptionDefn.deprecated
		~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
				      (Parsing.symbol_end_pos ()))
		 "variable"
		 (fun f -> Format.pp_print_string
			     f "use |kappa instance| instead.")
	      in
	      (($1,rhs_pos 1),(Alg_expr.KAPPA_INSTANCE $2,rhs_pos 2))}
    | LABEL alg_expr {(($1,rhs_pos 1),$2)}
    | ID alg_expr {(($1,rhs_pos 1),$2)}
    | LABEL error
	    {raise
	       (ExceptionDefn.Syntax_Error
		  (add_pos ("Illegal definition of variable '"^$1^"'")))
	    }
    ;

bool_expr:
    | OP_PAR bool_expr CL_PAR {$2}
    | TRUE {add_pos Alg_expr.TRUE}
    | FALSE {add_pos Alg_expr.FALSE}
    | bool_expr AND bool_expr {add_pos (Alg_expr.BOOL_OP(Operator.AND,$1,$3))}
    | bool_expr OR bool_expr {add_pos (Alg_expr.BOOL_OP(Operator.OR,$1,$3))}
    | alg_expr GREATER alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.GREATER,$1,$3))}
    | alg_expr SMALLER alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.SMALLER,$1,$3))}
    | alg_expr EQUAL alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.EQUAL,$1,$3))}
    | alg_expr DIFF alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.DIFF,$1,$3))}
    ;

standalone_bool_expr: bool_expr EOF {$1}

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

edit_rule_expression:
	rule_label mixture token_expr AT rate
	{ let (act,un_act) = $5 in
	$1, {Ast.mix = $2; Ast.delta_token = $3; Ast.act; Ast.un_act} };

rule_expression:
    | rule_label lhs_rhs arrow lhs_rhs birate
		 { let pos =
		     Locality.of_pos (Parsing.rhs_start_pos 2)
				     (Parsing.symbol_end_pos ()) in
		   let (k2,k1,kback,kback1) = $5 in
		   let lhs,token_l = $2 and rhs,token_r = $4 in
		   ($1,({Ast.lhs=lhs; Ast.rm_token = token_l; Ast.bidirectional=$3;
			 Ast.rhs=rhs; Ast.add_token = token_r;
			 Ast.k_def=k2; Ast.k_un=k1; Ast.k_op=kback; Ast.k_op_un=kback1},pos))
		 }
    ;

arrow:
    | KAPPA_RAR {false}
    | KAPPA_LRAR {true}
    ;

constant:
    | INFINITY {add_pos (Alg_expr.CONST (Nbr.F infinity))}
    | FLOAT {add_pos (Alg_expr.CONST (Nbr.F $1))}
    | INT {add_pos (Alg_expr.CONST (Nbr.I $1))}
    | EMAX {add_pos (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR))}
    | TMAX {add_pos (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR))}
    | CPUTIME {add_pos (Alg_expr.STATE_ALG_OP (Operator.CPUTIME))}
    ;

variable:
    | PIPE ID PIPE {add_pos (Alg_expr.TOKEN_ID ($2))}
    | PIPE non_empty_mixture PIPE { add_pos (Alg_expr.KAPPA_INSTANCE $2) }
    | ID {add_pos (Alg_expr.ALG_VAR ($1))}
    | LABEL {add_pos (Alg_expr.ALG_VAR ($1))}
    | TIME {add_pos (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR))}
    | EVENT {add_pos (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR))}
    | NULL_EVENT {add_pos (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR))}
    ;

small_alg_expr:
    | OP_PAR alg_expr CL_PAR {$2}
    | constant {$1}
    | variable {$1}
    | MAX small_alg_expr small_alg_expr
	  {add_pos (Alg_expr.BIN_ALG_OP(Operator.MAX,$2,$3))}
    | MIN small_alg_expr small_alg_expr
	  {add_pos (Alg_expr.BIN_ALG_OP(Operator.MIN,$2,$3))}
    | EXPONENT mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.EXP,$2))}
    | SINUS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.SINUS,$2))}
    | COSINUS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.COSINUS,$2))}
    | TAN mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.TAN,$2))}
    | ABS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.INT,$2))}
    | SQRT mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.SQRT,$2))}
    | LOG mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.LOG,$2))}
    ;

mid_alg_expr:
    | MINUS mid_alg_expr { add_pos (Alg_expr.UN_ALG_OP(Operator.UMINUS,$2)) }
    | small_alg_expr { $1 }
    | mid_alg_expr MULT mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MULT,$1,$3))}
    | mid_alg_expr PLUS mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.SUM,$1,$3))}
    | mid_alg_expr DIV mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.DIV,$1,$3))}
    | mid_alg_expr MINUS mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MINUS,$1,$3))}
    | mid_alg_expr POW mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.POW,$1,$3))}
    | mid_alg_expr MODULO mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MODULO,$1,$3))}

alg_expr:
    | mid_alg_expr {$1}
    | bool_expr THEN alg_expr ELSE small_alg_expr {add_pos (Alg_expr.IF($1,$3,$5))}

birate:
    | AT rate {let (k2,k1) = $2 in (k2,k1,None,None)}
    | AT rate COMMA rate {let (k2,k1) = $2 in
			  let (kback,kback1) = $4 in
			  (k2,k1,Some kback,kback1)}
    | {raise (ExceptionDefn.Syntax_Error (add_pos "rule rate expected"))}
    ;

rate:
    | alg_expr OP_CUR alg_with_radius CL_CUR {($1,Some $3)}
    | alg_expr {($1,None)}
    | OP_CUR alg_with_radius CL_CUR
      {(Locality.dummy_annot (Alg_expr.CONST Nbr.zero),Some $2)}
    | alg_expr OP_CUR CL_CUR
      {($1,Some (Locality.dummy_annot (Alg_expr.CONST Nbr.zero),None))}
    | {raise (ExceptionDefn.Syntax_Error (add_pos "missing rule rate"))}
    ;

alg_with_radius:
    | alg_expr {($1,None)}
    | alg_expr TYPE alg_expr {($1, Some $3)}
    ;

mixture:
      /*empty*/ {[]}
    | OP_PAR mixture CL_PAR {$2}
    | agent_expression COMMA mixture {$1 :: $3}
    | agent_expression {[$1]}
;

non_empty_mixture:
    | ID OP_PAR interface_expression CL_PAR
    { [($1,rhs_pos 1), $3, None] }
    | ID OP_PAR interface_expression CL_PAR COMMA mixture
    { (($1,rhs_pos 1), $3, None) :: $6}
    ;

mod_agent:
	| { None }
	| PLUS { Some Ast.Create }
	| MINUS { Some Ast.Erase };

agent_expression:
    | mod_agent ID OP_PAR interface_expression CL_PAR
	 {(($2,rhs_pos 2), $4, $1)}
    | mod_agent ID error
	 { raise (ExceptionDefn.Syntax_Error
		    (add_pos ("Malformed agent '"^$2^"'")))}
    ;

interface_expression:
  /*empty*/ {[]}
    | port_expression COMMA interface_expression {$1::$3}
    | port_expression {[$1]}
    ;


port_expression:
    | ID internal_state link_state_mod
	 { {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=[];
	    Ast.port_int_mod = None; Ast.port_lnk_mod = $3; } }
    | ID internal_state link_state link_state_mod
	 { {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=$3;
	    Ast.port_int_mod = None; Ast.port_lnk_mod = $4; } }
    | ID internal_state DIV KAPPA_MRK link_state_mod
	 { {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=[];
	    Ast.port_int_mod = Some($4,rhs_pos 4); Ast.port_lnk_mod = $5; } }
    | ID internal_state DIV KAPPA_MRK link_state link_state_mod
	 { {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=$5;
	    Ast.port_int_mod = Some($4,rhs_pos 4); Ast.port_lnk_mod = $6; } }
    ;

internal_state:
  /*empty*/ {[]}
    | KAPPA_MRK internal_state {($1,rhs_pos 1)::$2}
    | error
       {raise (ExceptionDefn.Syntax_Error
       (add_pos "Issue after internal state"))}
    ;

link_state_mod:
	| {None}
	| DIV KAPPA_LNK DOT {Some None}
	| DIV KAPPA_LNK INT {Some (Some ($3,rhs_pos 3))}
	| DIV error
	{raise (ExceptionDefn.Syntax_Error
	  (add_pos "Incorrect link modification"))};


a_link_state:
    | KAPPA_LNK DOT {(Ast.LNK_FREE,rhs_pos 2)}
    | KAPPA_LNK INT {(Ast.LNK_VALUE ($2,()),rhs_pos 2)}
    | KAPPA_LNK KAPPA_SEMI {(Ast.LNK_SOME,rhs_pos 2)}
    | KAPPA_LNK ID DOT ID {add_pos (Ast.LNK_TYPE
				      (($2,rhs_pos 2),($4,rhs_pos 4)))}
    | KAPPA_WLD {add_pos Ast.LNK_ANY}
    | KAPPA_LNK error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Invalid link state"))}
;

link_state:
	| a_link_state link_state {$1::$2}
	| a_link_state {[$1]};

interactive_command:
	| RUN NEWLINE {Ast.RUN (Locality.dummy_annot Alg_expr.FALSE)}
	| RUN bool_expr NEWLINE {Ast.RUN $2}
	| effect_list NEWLINE {Ast.MODIFY $1}
	| EOF {Ast.QUIT}
	| error
	{raise (ExceptionDefn.Syntax_Error (add_pos "Unrecognized command"))}
%%
