/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  let add_pos x =
    (x,
    Locality.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
  let rhs_pos i =
    Locality.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)

  let internal_memory = ref []
  let add x = internal_memory := x :: !internal_memory
  let output () =
    let o = List.rev !internal_memory in let () = internal_memory := [] in o
%}

%token EOF COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR OP_BRA CL_BRA AT SEMICOLON LOG
%token PLUS MINUS MULT DIV MOD MAX MIN SINUS COSINUS TAN POW ABS SQRT EXPONENT
%token OR AND NOT THEN ELSE DIFF EQUAL SMALLER GREATER TRUE FALSE INFINITY
%token SHARP UNDERSCORE PIPE RAR LRAR EMAX TMAX CPUTIME TIME EVENT NULL_EVENT
%token COLON NEWLINE SIGNATURE TOKEN INIT LET OBS PLOT PERT CONFIG RUN
%token DELETE INTRO SNAPSHOT STOP FLUX TRACK ASSIGN PRINTF PLOTENTRY SPECIES_OF
%token DO REPEAT ALARM
%token <int> INT
%token <float> FLOAT
%token <string> ID LABEL STRING
%token <string> SPACE COMMENT UNKNOWN

%start model
%type <Ast.parsing_instruction list> model

%start interactive_command
%type <(Ast.mixture,Ast.mixture,string) Ast.command> interactive_command

%start standalone_effect_list
%type
  <(Ast.mixture,Ast.mixture,string) Ast.modif_expr list> standalone_effect_list

%start standalone_bool_expr
%type <(Ast.mixture,string) Alg_expr.bool Locality.annot> standalone_bool_expr

%%

annot:
  | { [] }
  | NEWLINE annot { "\n"::$2 }
  | SPACE annot { $1::$2 }
  | COMMENT annot { $1::$2 }
  ;

nbr:
  | INFINITY { Nbr.F infinity }
  | FLOAT { Nbr.F $1 }
  | INT { Nbr.I $1 }
  ;

link_state:
  | DOT { add_pos Ast.LNK_FREE }
  | INT { add_pos (Ast.LNK_VALUE ($1,())) }
  | UNDERSCORE { add_pos Ast.LNK_SOME }
  | ID annot DOT annot ID
    { add_pos (Ast.LNK_TYPE (($1,rhs_pos 1),($5,rhs_pos 5))) }
  | SHARP { add_pos Ast.LNK_ANY }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error (add_pos "incomplete link state")) }
  ;

link_states:
  | link_state annot { [$1] }
  | link_state annot link_states { $1 :: $3 }
  | link_state annot COMMA annot link_states { $1 :: $5 }
  ;

link_modif:
  | { None }
  | DIV annot DOT annot { Some None }
  | DIV annot INT annot { Some (Some ($3, rhs_pos 3)) }
  | DIV annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "incomplete link modification")) }
  ;

internal_state:
  | ID { add_pos (Some $1) }
  | SHARP { add_pos None }
  ;

internal_states:
  | internal_state annot { [$1] }
  | internal_state annot internal_states { $1 :: $3 }
  | internal_state annot COMMA annot internal_states { $1 :: $5 }
  ;

internal_modif:
  | { None }
  | DIV annot ID annot { Some ($3, rhs_pos 3) }
  | DIV annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "incomplete link modification")) }
  ;

site_link:
  | annot link_states link_modif CL_BRA annot { ($2, $3) }
  | annot error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid linking state or missing ']'",rhs_pos 4)) }
  ;

site_internal:
  | internal_states internal_modif CL_CUR annot { ($1, $2) }
  | error
    { raise (ExceptionDefn.Syntax_Error
               ("invalid internal state or missing '}'",rhs_pos 3)) }
  ;

counter_modif:
  | PLUS annot EQUAL annot INT { ($5, rhs_pos 5) }
  | PLUS annot EQUAL annot MINUS annot INT { (- $7, rhs_pos 7) }
  | MINUS annot EQUAL annot INT { (- $5, rhs_pos 5) }
  ;

counter_test:
  | EQUAL annot INT { (Ast.CEQ $3,rhs_pos 3) }
  | GREATER annot EQUAL annot INT { (Ast.CGTE $5,rhs_pos 5) }
  | EQUAL annot ID { (Ast.CVAR $3,rhs_pos 3) }
  ;

site_counter:
  | counter_modif annot CL_CUR annot { (None, $1) }
  | counter_test annot CL_CUR annot { (Some $1, Locality.dummy_annot 0) }
  | counter_test annot DIV annot counter_modif annot CL_CUR annot
    { (Some $1,$5) }
  ;

site:
  | ID annot OP_BRA site_link OP_CUR annot site_internal
    { let (port_lnk, port_lnk_mod) = $4 in
      let (port_int, port_int_mod) = $7 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
          Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; } }
  | ID annot OP_CUR annot site_internal OP_BRA site_link
    { let (port_int, port_int_mod) = $5 in
      let (port_lnk, port_lnk_mod) = $7 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
          Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; } }
  | ID annot OP_BRA site_link
    { let (port_lnk, port_lnk_mod) = $4 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1); Ast.port_int=[];
          Ast.port_lnk; Ast.port_int_mod=None; Ast.port_lnk_mod; } }
  | ID annot OP_CUR annot site_internal
    { let (port_int, port_int_mod) = $5 in
      Ast.Port
        { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[];
          Ast.port_int; Ast.port_int_mod; Ast.port_lnk_mod=None; } }
  | ID annot OP_CUR annot site_counter
    { let (count_test,count_delta) = $5 in
      Ast.Counter
        { Ast.count_nme=($1,rhs_pos 1); Ast.count_test; Ast.count_delta } }
  | ID annot
    { Ast.Port
        { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[]; Ast.port_int=[];
          Ast.port_int_mod=None; Ast.port_lnk_mod=None; } }
  ;

interface:
  | { [] }
  | site interface { $1 :: $2 }
  | site COMMA annot interface { $1 :: $4 }
  ;

agent_modif:
  | annot { None,$1 }
  | annot PLUS annot { Some Ast.Create,$3 }
  | annot MINUS annot { Some Ast.Erase,$3 }
  ;

agent_no_err:
  | DOT annot { (Ast.Absent (rhs_pos 1),$2) }
  | ID annot OP_PAR annot interface CL_PAR agent_modif
    { let modif,an = $7 in
      (Ast.Present (($1,rhs_pos 1), $5, modif),an) }
  | ID annot COLON annot ID annot OP_PAR annot interface CL_PAR agent_modif
    { let modif,an = $11 in
      (Ast.Present (($5,rhs_pos 5), $9, modif),an) }
  ;

agent:
  | agent_no_err { $1 }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos ("Malformed agent '"^$1^"'"))) }
  ;

pattern_no_err:
  | agent_no_err COMMA annot pattern_no_err { (fst $1) :: $4 }
  | agent_no_err { [fst $1] }
  ;

pattern:
  | agent COMMA annot pattern
    { let (x,_) = $1 in let (y,p) =$4 in (x::y,p) }
  | agent { let (x,p) = $1 in ([x],p) }
  ;

constant:
  | nbr { add_pos (Alg_expr.CONST $1) }
  | EMAX { add_pos (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR)) }
  | TMAX { add_pos (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR)) }
  | CPUTIME { add_pos (Alg_expr.STATE_ALG_OP (Operator.CPUTIME)) }
  ;

variable:
  | PIPE annot ID annot PIPE { add_pos (Alg_expr.TOKEN_ID ($3)) }
  | PIPE annot pattern PIPE
    { add_pos (Alg_expr.KAPPA_INSTANCE (fst $3)) }
  | ID { add_pos (Alg_expr.ALG_VAR ($1)) }
  | LABEL { add_pos (Alg_expr.ALG_VAR ($1)) }
  | TIME { add_pos (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR)) }
  | EVENT { add_pos (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR)) }
  | NULL_EVENT
    { add_pos (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR)) }
  ;

small_alg_expr:
  | OP_PAR annot alg_expr CL_PAR { fst $3 }
  | constant { $1 }
  | variable { $1 }
  | MAX annot small_alg_expr annot small_alg_expr
    { add_pos (Alg_expr.BIN_ALG_OP(Operator.MAX,$3,$5)) }
  | MIN annot small_alg_expr annot small_alg_expr
    { add_pos (Alg_expr.BIN_ALG_OP(Operator.MIN,$3,$5)) }
  | EXPONENT annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.EXP,$3)) }
  | SINUS annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.SINUS,$3)) }
  | COSINUS annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.COSINUS,$3)) }
  | TAN annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.TAN,$3)) }
  | ABS annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.INT,$3)) }
  | SQRT annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.SQRT,$3)) }
  | LOG annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.LOG,$3)) }
  | MINUS annot small_alg_expr
    { add_pos (Alg_expr.UN_ALG_OP(Operator.UMINUS,$3)) }
  ;

alg_expr_up_to_mod:
  | small_alg_expr annot { ($1,$2) }
  | alg_expr_up_to_mod POW annot small_alg_expr annot
    { (add_pos (Alg_expr.BIN_ALG_OP(Operator.POW,fst $1,$4)),$5) }
  ;

alg_expr_up_to_prod:
  | alg_expr_up_to_mod { $1 }
  | alg_expr_up_to_mod MOD annot alg_expr_up_to_prod
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_ALG_OP(Operator.MODULO,fst $1,y)),an) }
  ;

alg_expr_up_to_sum:
  | alg_expr_up_to_prod { $1 }
  | alg_expr_up_to_prod MULT annot alg_expr_up_to_sum
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_ALG_OP(Operator.MULT,fst $1,y)),an) }
  | alg_expr_up_to_prod DIV annot alg_expr_up_to_sum
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_ALG_OP(Operator.DIV,fst $1,y)),an) }
  ;

alg_expr_up_to_if:
  | alg_expr_up_to_sum { $1 }
  | alg_expr_up_to_sum PLUS annot alg_expr_up_to_if
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_ALG_OP(Operator.SUM,fst $1,y)),an) }
  | alg_expr_up_to_sum MINUS annot alg_expr_up_to_if
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_ALG_OP(Operator.MINUS,fst $1,y)),an) }

alg_expr:
  | alg_expr_up_to_if { $1 }
  | bool_expr THEN annot alg_expr ELSE annot small_alg_expr annot
    { (add_pos (Alg_expr.IF(fst $1,fst $4,$7)),$8) }
  ;

boolean:
  | TRUE { true }
  | FALSE { false }
  ;

small_bool_expr:
  | OP_PAR annot bool_expr CL_PAR { fst $3 }
  | TRUE { add_pos Alg_expr.TRUE }
  | FALSE { add_pos Alg_expr.FALSE }
  | NOT annot small_bool_expr
    { add_pos (Alg_expr.UN_BOOL_OP(Operator.NOT,$3)) }
  ;

bool_expr_comp:
  | small_bool_expr annot { ($1,$2) }
  | alg_expr_up_to_if GREATER annot alg_expr
    { let (y,an) = $4 in
      (add_pos (Alg_expr.COMPARE_OP(Operator.GREATER,fst $1,y)),an) }
  | alg_expr_up_to_if SMALLER annot alg_expr
    { let (y,an) = $4 in
      (add_pos (Alg_expr.COMPARE_OP(Operator.SMALLER,fst $1,y)),an) }
  | alg_expr_up_to_if EQUAL annot alg_expr
    { let (y,an) = $4 in
      (add_pos (Alg_expr.COMPARE_OP(Operator.EQUAL,fst $1,y)),an) }
  | alg_expr_up_to_if DIFF annot alg_expr
    { let (y,an) = $4 in
      (add_pos (Alg_expr.COMPARE_OP(Operator.DIFF,fst $1,y)),an) }
  ;

bool_expr_no_or:
  | bool_expr_comp { $1 }
  | bool_expr_comp AND annot bool_expr_no_or
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_BOOL_OP(Operator.AND,fst $1,y)),an) }
  ;

bool_expr:
  | bool_expr_no_or { $1 }
  | bool_expr_no_or OR annot bool_expr
    { let (y,an) = $4 in
      (add_pos (Alg_expr.BIN_BOOL_OP(Operator.OR,fst $1,y)),an) }
  ;

standalone_bool_expr:
  | annot bool_expr EOF { fst $2 }
  | annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "Problematic boolean expression")) }
  ;

arrow:
  | RAR {false}
  | LRAR {true}
  ;

sum_token:
  | small_alg_expr annot ID annot { [($1,($3,rhs_pos 3))] }
  | small_alg_expr annot ID annot COMMA annot sum_token
    { ($1,($3,rhs_pos 3)) :: $7 }
  ;

rule_side:
  | pattern { (fst $1,[]) }
  | pattern PIPE annot sum_token { (fst $1, $4) }
  | PIPE annot sum_token { ([], $3) }
  | pattern PIPE annot error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos  "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  | PIPE annot error
    { raise (ExceptionDefn.Syntax_Error
	(add_pos  "Malformed token expression, I was expecting a_0 t_0, ... \
, a_n t_n where t_i are tokens and a_i any algebraic formula")) }
  ;

rule_content:
  | rule_side arrow annot rule_side
    { let (lhs,rm_token) = $1 in
      let (rhs,add_token) = $4 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs; Ast.add_token},$2) }
  | rule_side arrow
    { let (lhs,rm_token) = $1 in
      (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs=[]; Ast.add_token=[]},$2) }
  | arrow annot rule_side
    { let (rhs,add_token) = $3 in
      (Ast.Arrow {Ast.lhs=[]; Ast.rm_token=[]; Ast.rhs; Ast.add_token},$1) }
  | rule_side
    { let (mix,delta_token) = $1 in
      (Ast.Edit {Ast.mix; Ast.delta_token},false) }
  ;

alg_with_radius:
  | alg_expr { (fst $1,None) }
  | alg_expr COLON annot alg_expr { (fst $1, Some (fst $4)) }
  ;

rate:
  | OP_CUR annot alg_with_radius CL_CUR annot alg_expr
    { let (b,an) = $6 in (b,Some $3,an) }
  | alg_expr OP_CUR annot alg_with_radius CL_CUR annot { (fst $1,Some $4,$6) }
  | alg_expr { let (a,an) = $1 in (a,None,an) }
  ;

birate:
  | AT annot rate { let (k2,k1,an) = $3 in (k2,k1,None,None,an) }
  | AT annot rate COMMA annot rate
    { let (k2,k1,_) = $3 in
      let (kback,kback1,an) = $6 in
      (k2,k1,Some kback,kback1,an) }
  ;

rule:
  | rule_content birate
    { let (k_def,k_un,k_op,k_op_un,_annot) = $2 in
      let (rewrite,bidirectional) = $1 in
      add_pos {
          Ast.rewrite;Ast.bidirectional;
          Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un;
    } }
  | rule_content error
    { raise (ExceptionDefn.Syntax_Error (add_pos "rule rate expected")) }
  ;

variable_declaration:
  | LABEL annot alg_expr { (($1,rhs_pos 1),fst $3) }
  | ID annot alg_expr { (($1,rhs_pos 1),fst $3) }
  | LABEL annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos ("Illegal definition of variable '"^$1^"'"))) }
  | ID annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos ("Illegal definition of variable '"^$1^"'"))) }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos ("label expected"))) }
  ;

init_declaration:
  | alg_expr pattern_no_err { (None,fst $1,(Ast.INIT_MIX $2,rhs_pos 2)) }
  | alg_expr OP_PAR annot pattern CL_PAR annot
    { (None,fst $1,(Ast.INIT_MIX (fst $4),rhs_pos 4)) }
  | alg_expr ID annot { (None,fst $1,(Ast.INIT_TOK $2,rhs_pos 2)) }
  | ID OP_CUR annot init_declaration CL_CUR annot
    { let (_,alg,init) = $4 in (Some ($1,rhs_pos 1),alg,init) }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "Malformed initial condition")) }
  ;

value_list:
  | STRING annot {[$1, rhs_pos 1]}
  | STRING annot value_list {($1,rhs_pos 1)::$3}
  ;

nonempty_print_expr:
  | STRING annot
    { ([Primitives.Str_pexpr (add_pos $1)],$2) }
  | alg_expr_up_to_if
    { let (a,p) = $1 in ([Primitives.Alg_pexpr a],p) }
  | STRING annot DOT annot nonempty_print_expr
    { let (l,p) = $5 in (Primitives.Str_pexpr ($1, rhs_pos 1)::l,p) }
  | alg_expr_up_to_if DOT annot nonempty_print_expr
    { let (l,p) = $4 in (Primitives.Alg_pexpr (fst $1)::l,p) }
  ;

print_expr:
  | annot { ([],$1) }
  | annot STRING annot { ([Primitives.Str_pexpr (add_pos $2)],$3) }
  | annot OP_PAR annot nonempty_print_expr CL_PAR annot { $4 }
  ;

effect:
  | ASSIGN annot ID annot alg_expr
    { let (a,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),p) }
  | ASSIGN annot LABEL annot alg_expr
    { let (a,p) = $5 in (Ast.UPDATE (($3,rhs_pos 3),a),p) }
  | TRACK annot LABEL annot boolean annot
    { (Ast.CFLOWLABEL ($5,($3,rhs_pos 3)),$6) }
  | TRACK annot pattern boolean annot
    { (Ast.CFLOWMIX ($4,(fst $3,rhs_pos 3)),$5) }
  | FLUX annot nonempty_print_expr boolean annot
    { let (p,_) = $3 in
      ((if $4 then Ast.FLUX (Primitives.RELATIVE,p) else Ast.FLUXOFF p),$5) }
  | FLUX annot nonempty_print_expr STRING annot boolean annot
    { let (p,_) = $3 in
      if $6 && $4 = "absolute" then
        (Ast.FLUX (Primitives.ABSOLUTE,p),$7)
      else if $6 && $4 = "probability" then
        (Ast.FLUX (Primitives.PROBABILITY,p),$7)
      else if $6 && $4 = "relative" then
        (Ast.FLUX (Primitives.RELATIVE,p),$7)
      else raise (ExceptionDefn.Syntax_Error
                    ("Incorrect FLUX expression",rhs_pos 4)) }
  | INTRO annot alg_expr pattern
    { let (m,p) = $4 in (Ast.INTRO (fst $3,(m, rhs_pos 4)),p) }
  | INTRO annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "Malformed perturbation instruction, I was expecting \
'$ADD alg_expression kappa_expression'")) }
  | DELETE annot alg_expr pattern
    { let (m,p) = $4 in (Ast.DELETE (fst $3,(m, rhs_pos 4)),p) }
  | DELETE annot error
           { raise (ExceptionDefn.Syntax_Error
                      (add_pos "Malformed perturbation instruction, I was \
expecting '$DEL alg_expression kappa_expression'")) }
/*
  | ID annot LAR annot alg_expr
    { let (n,a) = $5 in (Ast.UPDATE_TOK (($1,rhs_pos 1),n),p) }
*/
  | SNAPSHOT print_expr { let (s,p) = $2 in (Ast.SNAPSHOT s,p) }
  | STOP print_expr { let (s,p) = $2 in (Ast.STOP s,p) }
  | PRINTF print_expr GREATER print_expr
    { let (f,p) = $4 in (Ast.PRINT (f,fst $2),p) }
  | PRINTF print_expr { let (c,p) = $2 in (Ast.PRINT ([],c),p) }
  | PLOTENTRY annot { (Ast.PLOTENTRY,$2) }
  | SPECIES_OF print_expr pattern boolean annot
    { (Ast.SPECIES_OF ($4,fst $2,(fst $3, rhs_pos 3)),$5) }
  ;

effect_list:
  | OP_PAR annot effect_list CL_PAR annot { $3 }
  | effect { let (e,p) = $1 in ([e],p) }
  | effect SEMICOLON annot effect_list { let (l,a) = $4 in ((fst $1)::l,a) }
  ;

standalone_effect_list:
  | annot effect_list EOF { fst $2 }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos "Problematic effect list")) }
  ;

perturbation_alarm:
  | annot { None }
  | annot ALARM annot nbr annot { Some $4 }
  ;

perturbation_post:
  | { (None, []) }
  | REPEAT annot bool_expr { let (b,p) = $3 in (Some b,p) }
  ;

perturbation_declaration:
  | perturbation_alarm bool_expr DO annot effect_list perturbation_post
    { ($1,Some (fst $2),fst $5,fst $6) }
  | perturbation_alarm DO annot effect_list perturbation_post
    { ($1,None,fst $4,fst $5) }
  ;

sentence:
  | LABEL annot rule
    { add (Ast.RULE(Some ($1, rhs_pos 1),(fst $3, rhs_pos 3))) }
  | LABEL annot EQUAL annot alg_expr
    { add (Ast.DECLARE (($1,rhs_pos 1),fst $5)) }
  | rule { add (Ast.RULE (None,(fst $1, rhs_pos 1))) }
  | SIGNATURE annot agent { add (Ast.SIG (fst $3)) }
  | SIGNATURE annot error
    { raise (ExceptionDefn.Syntax_Error (add_pos "Malformed agent signature")) }
  | TOKEN annot ID annot { add (Ast.TOKENSIG ($3,rhs_pos 3)) }
  | PLOT annot alg_expr { add (Ast.PLOT (fst $3)) }
  | PLOT annot error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos
                  "Malformed plot instruction, \
an algebraic expression is expected")) }
  | LET annot variable_declaration { add (Ast.DECLARE $3) }
  | OBS annot variable_declaration { add (Ast.OBS $3) }
  | INIT annot init_declaration
    { let (opt_vol,alg,init) = $3 in add (Ast.INIT (opt_vol,alg,init)) }
  | PERT perturbation_declaration { add (Ast.PERT ($2, rhs_pos 2)) }
  | CONFIG annot STRING annot value_list
    { add (Ast.CONFIG (($3,rhs_pos 3),$5)) }
  ;

model_body:
  | sentence model_body { $2 }
  | EOF { output () }
  ;

model:
  | annot model_body { $2 }
  | error
    { raise (ExceptionDefn.Syntax_Error
               (add_pos "Incorrect beginning of sentence")) }
  ;

interactive_command:
  | annot RUN annot EOF { Ast.RUN (Locality.dummy_annot Alg_expr.FALSE) }
  | annot RUN annot bool_expr EOF { Ast.RUN (fst $4) }
  | annot effect_list EOF { Ast.MODIFY (fst $2) }
  | annot EOF { Ast.QUIT }
  | error
    { raise (ExceptionDefn.Syntax_Error (add_pos "Unrecognized command")) }
  ;
