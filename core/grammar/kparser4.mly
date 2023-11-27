/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  let add_pos e x =
    (x,
    Loc.of_pos (Parsing.symbol_start_pos ()) (Parsing.rhs_end_pos e))
  let rhs_pos i =
    Loc.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
  let end_pos = Parsing.rhs_end_pos
  let start_pos = Parsing.rhs_start_pos

  let internal_memory = ref []
  let add x = internal_memory := x :: !internal_memory
  let output () =
    let o = List.rev !internal_memory in let () = internal_memory := [] in o
%}

%token EOF COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR OP_BRA CL_BRA AT SEMICOLON
%token PLUS MINUS MULT DIV MOD MAX MIN SINUS COSINUS TAN POW ABS SQRT EXPONENT
%token LOG OR AND NOT THEN ELSE DIFF EQUAL SMALLER GREATER TRUE FALSE INFINITY
%token SHARP UNDERSCORE PIPE RAR LRAR LAR EMAX TMAX CPUTIME TIME EVENT NULL_EVENT
%token COLON NEWLINE BACKSLASH SIGNATURE TOKEN INIT OBS PLOT PERT CONFIG APPLY
%token DELETE INTRO SNAPSHOT STOP FLUX TRACK ASSIGN PRINTF PLOTENTRY SPECIES_OF
%token DO REPEAT ALARM RUN LET
%token <int> INT
%token <float> FLOAT
%token <string> ID LABEL STRING
%token <string> SPACE COMMENT

%start model
%type <Ast.parsing_instruction list> model

%start interactive_command
%type <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.command> interactive_command

%start standalone_effect_list
%type
  <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.modif_expr list> standalone_effect_list

%start standalone_bool_expr
%type <(Ast.mixture,string) Alg_expr.bool Loc.annoted> standalone_bool_expr

%%

annoted:
  | { [] }
  | NEWLINE annoted { "\n"::$2 }
  | SPACE annoted { $1::$2 }
  | COMMENT annoted { $1::$2 }
  ;

nbr:
  | INFINITY { Nbr.F infinity }
  | FLOAT { Nbr.F $1 }
  | INT { Nbr.I $1 }
  ;

link_state:
  | DOT { add_pos 1 LKappa.LNK_FREE }
  | INT { add_pos 1 (LKappa.LNK_VALUE ($1,())) }
  | UNDERSCORE { add_pos 1 LKappa.LNK_SOME }
  | ID annoted DOT annoted ID
    { add_pos 5 (LKappa.LNK_TYPE (($1,rhs_pos 1),($5,rhs_pos 5))) }
  | SHARP { add_pos 1 LKappa.LNK_ANY }
  | ID annoted error
    { raise (ExceptionDefn.Syntax_Error (add_pos 3 "incomplete link state")) }
  ;

link_states:
  | link_state annoted { [$1] }
  | link_state annoted link_states { $1 :: $3 }
  | link_state annoted COMMA annoted link_states { $1 :: $5 }
  ;

link_modif:
  | { None }
  | DIV annoted DOT annoted { Some None }
  | DIV annoted INT annoted { Some (Some ($3, rhs_pos 3)) }
  | DIV annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "incomplete link modification")) }
  ;

internal_state:
  | ID { add_pos 1 (Some $1) }
  | SHARP { add_pos 1 None }
  ;

internal_states:
  | internal_state annoted { [$1] }
  | internal_state annoted internal_states { $1 :: $3 }
  | internal_state annoted COMMA annoted internal_states { $1 :: $5 }
  ;

internal_modif:
  | { None }
  | DIV annoted ID annoted { Some ($3, rhs_pos 3) }
  | DIV annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "incomplete link modification")) }
  ;

site_link:
  | annoted link_states link_modif CL_BRA { ($2, $3) }
  | annoted error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid linking state or missing ']'",rhs_pos 2)) }
  ;

site_internal:
  | internal_states internal_modif CL_CUR { ($1, $2) }
  | error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid internal state or missing '}'",rhs_pos 3)) }
  ;

counter_modif:
  | PLUS annoted EQUAL annoted INT { ($5, rhs_pos 5) }
  | PLUS annoted EQUAL annoted MINUS annoted INT { (- $7, rhs_pos 7) }
  | MINUS annoted EQUAL annoted INT { (- $5, rhs_pos 5) }
  ;

counter_test:
  | EQUAL annoted INT { (Ast.CEQ $3,rhs_pos 3) }
  | GREATER annoted EQUAL annoted INT { (Ast.CGTE $5,rhs_pos 5) }
  | EQUAL annoted ID { (Ast.CVAR $3,rhs_pos 3) }
  ;

site_counter:
  | counter_modif annoted CL_CUR annoted { (None, $1) }
  | counter_test annoted CL_CUR annoted { (Some $1, Loc.annot_with_dummy 0) }
  | counter_test annoted DIV annoted counter_modif annoted CL_CUR annoted
    { (Some $1,$5) }
  ;

site:
  | ID annoted OP_BRA site_link annoted OP_CUR annoted site_internal annoted
    { let (port_link, port_link_mod) = $4 in
      let (port_int, port_int_mod) = $8 in
      Ast.Port
        { Ast.port_name=($1,rhs_pos 1); Ast.port_int;
          Ast.port_link; Ast.port_int_mod; Ast.port_link_mod; } }
  | ID annoted OP_CUR annoted site_internal annoted OP_BRA site_link annoted
    { let (port_int, port_int_mod) = $5 in
      let (port_link, port_link_mod) = $8 in
      Ast.Port
        { Ast.port_name=($1,rhs_pos 1); Ast.port_int;
          Ast.port_link; Ast.port_int_mod; Ast.port_link_mod; } }
  | ID annoted OP_BRA site_link annoted
    { let (port_link, port_link_mod) = $4 in
      Ast.Port
        { Ast.port_name=($1,rhs_pos 1); Ast.port_int=[];
          Ast.port_link; Ast.port_int_mod=None; Ast.port_link_mod; } }
  | ID annoted OP_CUR annoted site_internal annoted
    { let (port_int, port_int_mod) = $5 in
      Ast.Port
        { Ast.port_name=($1,rhs_pos 1);Ast.port_link=[];
          Ast.port_int; Ast.port_int_mod; Ast.port_link_mod=None; } }
  | ID annoted OP_CUR annoted site_counter
    { let (counter_test,counter_delta) = $5 in
      Ast.Counter
        { Ast.counter_name=($1,rhs_pos 1); Ast.counter_test; Ast.counter_delta } }
  | ID annoted
    { Ast.Port
        { Ast.port_name=($1,rhs_pos 1);Ast.port_link=[]; Ast.port_int=[];
          Ast.port_int_mod=None; Ast.port_link_mod=None; } }
  ;

interface:
  | { [] }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 ("Malformed site expression"))) }
  | site interface { $1 :: $2 }
  | site COMMA annoted interface { $1 :: $4 }
  ;

agent_modif:
  | annoted { Ast.NoMod,start_pos 1,$1 }
  | annoted PLUS annoted { Ast.Create,end_pos 2,$3 }
  | annoted MINUS annoted { Ast.Erase,end_pos 2,$3 }
  ;

agent:
  | DOT annoted { (Ast.Absent (rhs_pos 1),end_pos 1,$2) }
  | ID annoted OP_PAR annoted interface CL_PAR agent_modif
    { let modif,pend,an = $7 in
      (Ast.Present (($1,rhs_pos 1), $5, modif),pend,an) }
  | ID annoted COLON annoted ID annoted OP_PAR annoted interface CL_PAR agent_modif
    { let modif,pend,an = $11 in
      (Ast.Present (($5,rhs_pos 5), $9, modif),pend,an) }
  | ID annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Malformed agent '"^$1^"'"))) }
  ;

pattern:
  | agent COMMA annoted pattern
{ let (x,_,_) = $1 in
  match $4 with
  | (y::z,pend,p) -> ((x::y)::z,pend,p)
  | ([],_,_) ->
     raise (ExceptionDefn.Internal_Error
              (add_pos 4 ("assertion failure in pattern parsing"))) }
  | agent BACKSLASH annoted pattern
{ let (x,_,_) = $1 in let (y,pend,p) = $4 in ([x]::y,pend,p) }
  | agent { let (x,pend,p) = $1 in ([[x]],pend,p) }
  ;

constant:
  | nbr { add_pos 1 (Alg_expr.CONST $1) }
  | EMAX { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR)) }
  | TMAX { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR)) }
  | CPUTIME { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.CPUTIME)) }
  ;

variable:
  | PIPE annoted ID annoted PIPE { add_pos 5 (Alg_expr.TOKEN_ID ($3)) }
  | PIPE annoted pattern PIPE
    { let (p,_,_) = $3 in add_pos 4 (Alg_expr.KAPPA_INSTANCE p) }
  | ID { add_pos 1 (Alg_expr.ALG_VAR ($1)) }
  | LABEL { add_pos 1 (Alg_expr.ALG_VAR ($1)) }
  | TIME { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR)) }
  | EVENT { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR)) }
  | NULL_EVENT
    { add_pos 1 (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR)) }
  ;

small_alg_expr:
  | OP_PAR annoted alg_expr CL_PAR { let (x,_,_) = $3 in x }
  | constant { $1 }
  | variable { $1 }
  | MAX annoted small_alg_expr annoted small_alg_expr
    { add_pos 5 (Alg_expr.BIN_ALG_OP(Operator.MAX,$3,$5)) }
  | MIN annoted small_alg_expr annoted small_alg_expr
    { add_pos 5 (Alg_expr.BIN_ALG_OP(Operator.MIN,$3,$5)) }
  | EXPONENT annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.EXP,$3)) }
  | SINUS annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.SINUS,$3)) }
  | COSINUS annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.COSINUS,$3)) }
  | TAN annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.TAN,$3)) }
  | ABS annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.INT,$3)) }
  | SQRT annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.SQRT,$3)) }
  | LOG annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.LOG,$3)) }
  | MINUS annoted small_alg_expr
    { add_pos 3 (Alg_expr.UN_ALG_OP(Operator.UMINUS,$3)) }
  ;

alg_expr_up_to_mod:
  | small_alg_expr annoted { ($1,end_pos 1,$2) }
  | small_alg_expr annoted POW annoted alg_expr_up_to_mod
    { let (x,y,z) = $5 in
      (add_pos 4 (Alg_expr.BIN_ALG_OP(Operator.POW,$1,x)),y,z) }
  ;

alg_expr_up_to_prod:
  | alg_expr_up_to_mod { $1 }
  | alg_expr_up_to_prod MOD annoted alg_expr_up_to_mod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP (Operator.MODULO,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  ;

alg_expr_up_to_sum:
  | alg_expr_up_to_prod { $1 }
  | alg_expr_up_to_sum MULT annoted alg_expr_up_to_prod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.MULT,x,y),
       Loc.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_sum DIV annoted alg_expr_up_to_prod
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.DIV,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  ;

alg_expr_up_to_if:
  | alg_expr_up_to_sum { $1 }
  | alg_expr_up_to_if PLUS annoted alg_expr_up_to_sum
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.SUM,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if MINUS annoted alg_expr_up_to_sum
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_ALG_OP(Operator.MINUS,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }

alg_expr:
  | alg_expr_up_to_if { $1 }
  | bool_expr THEN annoted alg_expr ELSE annoted small_alg_expr annoted
    { let (i,_,_) = $1 in
      let (t,_,_) = $4 in
  ((Alg_expr.IF(i,t,$7),
    Loc.of_pos (start_pos 1) (end_pos 7)),end_pos 7,$8) }
  ;

boolean:
  | TRUE { true }
  | FALSE { false }
  ;

small_bool_expr:
  | OP_PAR annoted bool_expr CL_PAR { let (x,_,_) = $3 in x }
  | TRUE { add_pos 1 Alg_expr.TRUE }
  | FALSE { add_pos 1 Alg_expr.FALSE }
  | NOT annoted small_bool_expr
    { add_pos 3 (Alg_expr.UN_BOOL_OP(Operator.NOT,$3)) }
  ;

bool_expr_comp:
  | small_bool_expr annoted { ($1,end_pos 1, $2) }
  | alg_expr_up_to_if GREATER annoted alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.GREATER,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if SMALLER annoted alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.SMALLER,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if EQUAL annoted alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.EQUAL,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  | alg_expr_up_to_if DIFF annoted alg_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.COMPARE_OP(Operator.DIFF,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  ;

bool_expr_no_or:
  | bool_expr_comp { $1 }
  | bool_expr_comp AND annoted bool_expr_no_or
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_BOOL_OP(Operator.AND,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  ;

bool_expr:
  | bool_expr_no_or { $1 }
  | bool_expr_no_or OR annoted bool_expr
    { let (y,pend,an) = $4 in
      let (x,_,_) = $1 in
      ((Alg_expr.BIN_BOOL_OP(Operator.OR,x,y),
        Loc.of_pos (start_pos 1) pend),
       pend,an) }
  ;

standalone_bool_expr:
  | annoted bool_expr EOF { let (x,_,_) = $2 in x }
  | annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 2 "Problematic boolean expression")) }
  ;

arrow:
  | RAR {false}
  | LRAR {true}
  ;

sum_token:
  | small_alg_expr annoted ID annoted { [($1,($3,rhs_pos 3))],end_pos 3,$4 }
  | small_alg_expr annoted ID annoted COMMA annoted sum_token
    { let (l,pend,an) = $7 in ($1,($3,rhs_pos 3)) :: l,pend,an }
  ;

rule_side:
  | pattern { let (p,pend,an) = $1 in (p,[],pend,an) }
  | pattern PIPE annoted sum_token
    { let (p,_,_) = $1 in
      let (t,pend,an) = $4 in (p, t, pend, an) }
  | PIPE annoted sum_token { let (t,pend,an) = $3 in ([], t, pend, an) }
  | pattern PIPE annoted error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos 4 "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  | PIPE annoted error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos 3 "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  ;

rule_content:
  | rule_side arrow annoted rule_side
    { let (lhs,rm_token,_,_) = $1 in
      let (rhs,add_token,pend,an) = $4 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs; Ast.add_token},$2,pend,an) }
  | rule_side arrow annoted
    { let (lhs,rm_token,_,_) = $1 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs=[]; Ast.add_token=[]},$2,end_pos 2,$3) }
  | arrow annoted rule_side
    { let (rhs,add_token,pend,an) = $3 in
      (Ast.Arrow {Ast.lhs=[]; Ast.rm_token=[]; Ast.rhs; Ast.add_token},$1,pend,an) }
  | rule_side
    { let (mix,delta_token,pend,an) = $1 in
      (Ast.Edit {Ast.mix; Ast.delta_token},false,pend,an) }
  ;

alg_with_radius:
  | alg_expr { let (x,_,_) = $1 in (x,None) }
  | alg_expr COLON annoted alg_expr
    { let (x,_,_) = $1 in let (y,_,_) = $4 in (x, Some y) }
  ;

rate:
  | OP_CUR annoted alg_with_radius CL_CUR annoted alg_expr
    { let (b,pend,an) = $6 in (b,Some $3,pend,an) }
  | alg_expr OP_CUR annoted alg_with_radius CL_CUR annoted
    { let (x,_,_) = $1 in (x,Some $4,end_pos 5,$6) }
  | alg_expr { let (a,pend,an) = $1 in (a,None,pend,an) }
  ;

birate:
  | AT annoted rate { let (k2,k1,pend,an) = $3 in (k2,k1,None,None,pend,an) }
  | AT annoted rate COMMA annoted rate
    { let (k2,k1,_,_) = $3 in
      let (kback,kback1,pend,an) = $6 in
      (k2,k1,Some kback,kback1,pend,an) }
  ;

rule:
  | rule_content birate
    { let (k_def,k_un,k_op,k_op_un,pos_end,_annot) = $2 in
      let (rewrite,bidirectional,_,_) = $1 in
      ({
        Ast.rewrite;Ast.bidirectional;
        Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un;
      },Loc.of_pos (start_pos 1) pos_end) }
  | rule_content error
    { raise (ExceptionDefn.Syntax_Error (add_pos 2 "rule rate expected")) }
  ;

variable_declaration:
  | LABEL annoted alg_expr { let (v,pend,an) = $3 in (($1,rhs_pos 1),v,pend,an) }
  | ID annoted alg_expr { let (v,pend,an) = $3 in (($1,rhs_pos 1),v,pend,an) }
  | LABEL annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Illegal definition of variable '"^$1^"'"))) }
  | ID annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 ("Illegal definition of variable '"^$1^"'"))) }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 ("label expected"))) }
  ;

id_list:
  | ID annoted { [ $1,rhs_pos 1 ] }
  | ID annoted COMMA annoted id_list { ($1,rhs_pos 1) :: $5 }
  ;

init_declaration:
  | alg_expr pattern
    { let (v,_,_) = $1 in
      let (p,pend,_) = $2 in
      (v,Ast.INIT_MIX (p,Loc.of_pos (start_pos 2) pend)) }
  | alg_expr OP_PAR annoted pattern CL_PAR annoted
    { let (v,_,_) = $1 in
      let (p,pend,_) = $4 in
      (v,Ast.INIT_MIX (p,Loc.of_pos (start_pos 4) pend)) }
  | alg_expr id_list
    { let (v,_,_) = $1 in (v,Ast.INIT_TOK $2) }
/*
  | ID annoted OP_CUR annoted init_declaration CL_CUR annoted
    { let (_,alg,init) = $5 in (Some ($1,rhs_pos 1),alg,init) }
*/
  | ID LAR annoted alg_expr
    { let (v,_,_) = $4 in (v,Ast.INIT_TOK [$1,rhs_pos 1])}
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 "Malformed initial condition")) }
  ;

value_list:
  | STRING annoted {[$1, rhs_pos 1]}
  | STRING annoted value_list {($1,rhs_pos 1)::$3}
  ;

nonempty_print_expr:
  | STRING annoted
    { ([Primitives.Str_pexpr (add_pos 1 $1)],end_pos 1,$2) }
  | alg_expr_up_to_if
    { let (a,pend,p) = $1 in ([Primitives.Alg_pexpr a],pend,p) }
  | print_expr_list { $1 }
  | OP_PAR annoted print_expr_list CL_PAR annoted
    { let (v,_,an) = $3 in (v,end_pos 4,an @ $5) }
  ;

print_expr_list:
  | STRING annoted DOT annoted nonempty_print_expr
    { let (l,pend,p) = $5 in (Primitives.Str_pexpr ($1, rhs_pos 1)::l,pend,p) }
  | alg_expr_up_to_if DOT annoted nonempty_print_expr
    { let (l,pend,p) = $4 in
      let (v,_,_) = $1 in
      (Primitives.Alg_pexpr v::l,pend,p) }
  ;

print_expr:
  | annoted { ([],start_pos 1,$1) }
  | annoted nonempty_print_expr { $2 }
  ;

effect:
  | ASSIGN annoted ID annoted alg_expr
    { let (a,pend,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),pend,p) }
  | ASSIGN annoted LABEL annoted alg_expr
    { let (a,pend,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),pend,p) }
  | TRACK annoted LABEL annoted boolean annoted
    { (Ast.CFLOWLABEL ($5,($3,rhs_pos 3)),end_pos 5,$6) }
  | TRACK annoted pattern boolean annoted
    { let (pat,epat,_) = $3 in
      (Ast.CFLOWMIX ($4,(pat,Loc.of_pos (start_pos 3) epat)),end_pos 4, $5) }
  | FLUX annoted nonempty_print_expr boolean annoted
    { let (p,_,_) = $3 in
      ((if $4 then Ast.DIN (Primitives.RELATIVE,p) else Ast.DINOFF p),
       end_pos 4,$5) }
  | FLUX annoted nonempty_print_expr STRING annoted boolean annoted
    { let (p,_,_) = $3 in
      if $6 && $4 = "absolute" then
        (Ast.DIN (Primitives.ABSOLUTE,p),end_pos 6,$7)
      else if $6 && $4 = "probability" then
        (Ast.DIN (Primitives.PROBABILITY,p),end_pos 6,$7)
      else if $6 && $4 = "relative" then
        (Ast.DIN (Primitives.RELATIVE,p),end_pos 6,$7)
      else raise (ExceptionDefn.Syntax_Error
                    ("Incorrect DIN expression",rhs_pos 4)) }
  | APPLY annoted alg_expr rule_content
    { let (rewrite,_,pend,an) = $4 in
      let (v,_,_) = $3 in
      Ast.APPLY(v,
                ({ Ast.rewrite; Ast.bidirectional = false;
                   Ast.k_def=Alg_expr.const Nbr.zero;Ast.k_un=None;
                   Ast.k_op=None; Ast.k_op_un=None},
                 Loc.of_pos (start_pos 3) pend)),
      pend,an
    }
  | INTRO annoted alg_expr pattern
    { let (m,pend,p) = $4 in
      let (v,_,_) = $3 in
      (Ast.APPLY(v,
                 ({Ast.rewrite =
		   Ast.Edit {Ast.mix=Ast.to_created_mixture m;
                     Ast.delta_token=[];};
		   Ast.bidirectional=false;
                   Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                   Ast.k_op=None; Ast.k_op_un=None},
                  Loc.of_pos (start_pos 4) pend)),
       pend,p) }
  | INTRO annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3 "Malformed intervention instruction, I was expecting \
'$ADD alg_expression kappa_expression'")) }
  | DELETE annoted alg_expr pattern
    { let (m,pend,p) = $4 in
      let (v,_,_) = $3 in
      (Ast.APPLY(v,
               ({Ast.rewrite =
		 Ast.Edit {Ast.mix=Ast.to_erased_mixture m;
                   Ast.delta_token=[];};
		 Ast.bidirectional=false;
                 Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                 Ast.k_op=None; Ast.k_op_un=None},
                Loc.of_pos (start_pos 4) pend)),
       pend,p) }
  | DELETE annoted error
           { raise (ExceptionDefn.Syntax_Error
                      (add_pos 3 "Malformed intervention instruction, I was \
expecting '$DEL alg_expression kappa_expression'")) }
  | SNAPSHOT print_expr { let (s,pend,p) = $2 in (Ast.SNAPSHOT (false,s),pend,p) }
  | SNAPSHOT print_expr boolean annoted { let (s,_,_) = $2 in (Ast.SNAPSHOT ($3,s),end_pos 3,$4) }
  | STOP print_expr { let (s,pend,p) = $2 in (Ast.STOP s,pend,p) }
  | PRINTF print_expr GREATER print_expr
    { let (f,pend,p) = $4 in let (c,_,_) = $2 in (Ast.PRINT (f,c),pend,p) }
  | PRINTF print_expr { let (c,pend,p) = $2 in (Ast.PRINT ([],c),pend,p) }
  | PLOTENTRY annoted { (Ast.PLOTENTRY,end_pos 1,$2) }
  | SPECIES_OF annoted pattern boolean annoted GREATER print_expr
    {
      let (file,pend,p) = $7 in
      let (pat,pendp,_) = $3 in
      (Ast.SPECIES_OF ($4,file,(pat, Loc.of_pos (start_pos 3) pendp)),
       pend,p) }
  ;

idin:
| ID annoted LAR annoted alg_expr {
   let (v,pend,p) = $5 in
   let tk = ($1,rhs_pos 1) in
    (Ast.APPLY(Alg_expr.const Nbr.one,
        ({Ast.rewrite =
              Ast.Edit
                  {Ast.mix=[];
                   Ast.delta_token =
                      [(Alg_expr.BIN_ALG_OP(Operator.MINUS,v,(Alg_expr.TOKEN_ID $1,rhs_pos 1)),rhs_pos 1),tk];
                    };
              Ast.bidirectional=false;
              Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
              Ast.k_op=None; Ast.k_op_un=None}, Loc.of_pos (start_pos  4) pend)),pend,p)
   }
| ID annoted LAR error
     { raise (ExceptionDefn.Syntax_Error
                (add_pos 3 "Malformed intervention instruction, I was \
expecting 'ID <- alg_expression'")) }
| ID error
        { raise (ExceptionDefn.Syntax_Error
                   (add_pos 2 "Malformed intervention instruction, I was \
   expecting 'ID <- alg_expression'")) };
;

effect_or_idin:
  | effect {$1}
  | idin {$1}

partial_effect_list:
  | OP_PAR annoted partial_effect_list CL_PAR annoted { $3 }
  | effect_or_idin SEMICOLON annoted { let (e,_,_) = $1 in ([e],end_pos 2,$3) }
  | effect_or_idin { let (e,p,a) = $1 in ([e],p,a) }
  | effect_or_idin SEMICOLON annoted partial_effect_list
    { let (e,_,_) = $1 in let (l,pend,a) = $4 in (e::l,pend,a) }

partial_effect_list_at_least_one_idin:
      | idin SEMICOLON annoted { let (e,_,_) = $1 in ([e],end_pos 2,$3) }
      | idin { let (e,p,a) = $1 in ([e],p,a) }
      | idin SEMICOLON annoted partial_effect_list
          { let (e,_,_) = $1 in let (l,pend,a) = $4 in (e::l,pend,a) }

effect_list:
  | OP_PAR annoted partial_effect_list CL_PAR annoted { $3 }
  | OP_PAR annoted partial_effect_list CL_PAR annoted SEMICOLON annoted {let (e,_,_) =  $3 in e,end_pos 6,$7 }
  | effect SEMICOLON annoted { let (e,_,_) = $1 in ([e],end_pos 2,$3) }
  | effect SEMICOLON annoted effect_list
    { let (e,_,_) = $1 in let (l,pend,a) = $4 in (e::l,pend,a) }
  ;

standalone_effect_list:
  | annoted partial_effect_list EOF { let (e,_,_) = $2 in e }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 "Problematic effect list")) }
  ;

perturbation_alarm:
  | annoted { None }
  | annoted ALARM annoted nbr annoted { Some $4 }
  | annoted ALARM error
    { raise (ExceptionDefn.Syntax_Error (add_pos 3 "alarm takes a number as argument")) }
  ;

perturbation_post_closed:
  | REPEAT annoted bool_expr { let (b,pend,p) = $3 in (Some b,pend,p) }

perturbation_post:
  | { (None, Parsing.symbol_start_pos (),[]) }
  | perturbation_post_closed {$1}
  ;

perturbation_declaration:
  | perturbation_alarm bool_expr DO annoted effect_list perturbation_post
    { let (pre,_,_) = $2 in
      let (e,_,_) = $5 in
      let (post,_,_) = $6 in
      ($1,Some pre,e,post) }
  | perturbation_alarm DO annoted effect_list perturbation_post
    { let (e,_,_) = $4 in let (post,_,_) = $5 in ($1,None,e,post) }

  | perturbation_alarm bool_expr DO annoted partial_effect_list_at_least_one_idin perturbation_post_closed
    { let (pre,_,_) = $2 in
      let (e,_,_) = $5 in
      let (post,_,_) = $6 in
      ($1,Some pre,e,post) }
  | perturbation_alarm DO annoted partial_effect_list_at_least_one_idin perturbation_post_closed
        { let (e,_,_) = $4 in
          let (post,_,_) = $5 in
          ($1,None,e,post) }
  ;

sentence:
  | LABEL annoted rule
    { add (Ast.RULE(Some ($1, rhs_pos 1),$3)) }
  | LABEL annoted EQUAL annoted alg_expr
    { let (v,_,_) = $5 in add (Ast.DECLARE (($1,rhs_pos 1),v)) }
  | rule { add (Ast.RULE (None,$1)) }
  | SIGNATURE annoted agent { let (a,_,_) = $3 in add (Ast.SIG a) }
  | SIGNATURE annoted error
    { raise
        (ExceptionDefn.Syntax_Error (add_pos 3 "Malformed agent signature")) }
  | TOKEN annoted ID annoted { add (Ast.TOKENSIG ($3,rhs_pos 3)) }
  | PLOT annoted alg_expr { let (v,_,_) = $3 in add (Ast.PLOT v) }
  | PLOT annoted error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 3
                  "Malformed plot instruction, \
an algebraic expression is expected")) }
  | LET annoted variable_declaration
    { let (i,v,_,_) = $3 in add (Ast.DECLARE (i,v)) }
  | OBS annoted variable_declaration { let (i,v,_,_) = $3 in add (Ast.OBS (i,v)) }
  | INIT annoted init_declaration
    { let (alg,init) = $3 in add (Ast.INIT (alg,init)) }
  | PERT perturbation_declaration { add (Ast.PERT ($2, rhs_pos 2)) }
  | CONFIG annoted STRING annoted value_list
    { add (Ast.CONFIG (($3,rhs_pos 3),$5)) }
  ;

model_body:
  | sentence model_body { $2 }
  | EOF { output () }
  ;

model:
  | annoted model_body { $2 }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos 1 "Incorrect beginning of sentence !!!")) }
  ;

interactive_command:
  | annoted RUN annoted SEMICOLON { Ast.RUN (Loc.annot_with_dummy Alg_expr.FALSE) }
  | annoted RUN annoted bool_expr SEMICOLON { let (pause,_,_) = $4 in Ast.RUN pause }
  | annoted effect SEMICOLON { let (eff,_,_) = $2 in Ast.MODIFY [eff] }
  | annoted EOF { Ast.QUIT }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos 1 "Unrecognized command")) }
  ;
