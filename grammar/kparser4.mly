/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  open Result

  let add_pos x =
    (x,
    Locality.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
  let rhs_pos i =
    Locality.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
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
%type <Ast.parsing_instruction list * string Locality.annot list> model

%start interactive_command
%type
  <((Ast.mixture,Ast.mixture,string) Ast.command, string Locality.annot) Result.result>
    interactive_command

%start standalone_effect_list
%type
  <((Ast.mixture,Ast.mixture,string) Ast.modif_expr list,string Locality.annot) Result.result>
    standalone_effect_list

%start standalone_bool_expr
%type
  <((Ast.mixture,string) Alg_expr.bool Locality.annot,string Locality.annot) Result.result>
    standalone_bool_expr

%%

annot:
  | { [] }
  | NEWLINE annot { "\n"::$2 }
  | SPACE annot { $1::$2 }
  | COMMENT annot { $1::$2 }
  ;

recovery:
  | error NEWLINE { () }
  | error EOF { () }
  | error COMMENT { () }
  ;

nbr:
  | INFINITY { Nbr.F infinity }
  | FLOAT { Nbr.F $1 }
  | INT { Nbr.I $1 }
  ;

link_state:
  | DOT { Ok (add_pos Ast.LNK_FREE) }
  | INT { Ok (add_pos (Ast.LNK_VALUE ($1,()))) }
  | UNDERSCORE { Ok (add_pos Ast.LNK_SOME) }
  | ID annot DOT annot ID
    { Ok (add_pos (Ast.LNK_TYPE (($1,rhs_pos 1),($5,rhs_pos 5)))) }
  | SHARP { Ok (add_pos Ast.LNK_ANY) }
  | ID recovery { Error (add_pos "incomplete link state") }
  ;

link_states:
  | link_state annot { Result_util.map (fun x -> [x]) $1 }
  | link_state annot link_states
    { Result_util.map2 (fun x y -> x::y) $1 $3 }
  | link_state annot COMMA annot link_states
    { Result_util.map2 (fun x y -> x::y) $1 $5 }
  ;

link_modif:
  | { Ok None }
  | DIV annot DOT annot { Ok (Some None) }
  | DIV annot INT annot { Ok (Some (Some ($3, rhs_pos 3))) }
  | DIV { Error (add_pos "incomplete link modification") }
  ;

internal_state:
  | ID { add_pos (Some $1) }
  | SHARP { add_pos None }
  ;

internal_states:
  | internal_state annot { Ok [$1] }
  | internal_state annot internal_states
    { Result_util.map (fun l -> $1 :: l) $3 }
  | internal_state annot COMMA annot internal_states
    { Result_util.map (fun l -> $1 :: l) $5 }
  ;

internal_modif:
  | { Ok None }
  | DIV annot ID annot { Ok (Some ($3, rhs_pos 3)) }
  | DIV { Error (add_pos "incomplete link modification") }
  ;

site_link:
  | annot link_states link_modif CL_BRA annot
    { Result_util.map2 (fun x y -> (x,y)) $2 $3 }
  | annot link_states link_modif recovery
    { Error ("invalid linking state or missing ']'",rhs_pos 4) }
  ;

site_internal:
  | annot internal_states internal_modif CL_CUR annot
    { Result_util.map2 (fun x y -> (x,y)) $2 $3 }
  | annot internal_states internal_modif recovery
    { Error ("invalid internal state or missing '}'",rhs_pos 4) }
  ;

site:
  | ID annot OP_BRA site_link OP_CUR site_internal
    { match $4 with
      | Error _ as e -> e
      | Ok (port_lnk, port_lnk_mod) ->
         match $6 with
         | Error _ as e -> e
         | Ok (port_int, port_int_mod) ->
            Ok (Ast.Port
                  { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
                    Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; }) }
  | ID annot OP_CUR site_internal OP_BRA site_link
    { match $4 with
      | Error _ as e -> e
      | Ok (port_int, port_int_mod) ->
         match $6 with
         | Error _ as e -> e
         | Ok (port_lnk, port_lnk_mod) ->
            Ok (Ast.Port
                  { Ast.port_nme=($1,rhs_pos 1); Ast.port_int;
                    Ast.port_lnk; Ast.port_int_mod; Ast.port_lnk_mod; }) }
  | ID annot OP_BRA site_link
    { match $4 with
      | Error _ as e -> e
      | Ok (port_lnk, port_lnk_mod) ->
         Ok (Ast.Port
               { Ast.port_nme=($1,rhs_pos 1); Ast.port_int=[];
                 Ast.port_lnk; Ast.port_int_mod=None; Ast.port_lnk_mod; }) }
  | ID annot OP_CUR site_internal
    { match $4 with
      | Error _ as e -> e
      | Ok (port_int, port_int_mod) ->
         Ok (Ast.Port
               { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[];
                 Ast.port_int; Ast.port_int_mod; Ast.port_lnk_mod=None; }) }
  | ID annot
    { Ok (Ast.Port
            { Ast.port_nme=($1,rhs_pos 1);Ast.port_lnk=[]; Ast.port_int=[];
              Ast.port_int_mod=None; Ast.port_lnk_mod=None; }) }
  ;

interface:
  | { Ok [] }
  | site interface { Result_util.map2 (fun x y -> x::y) $1 $2 }
  | site COMMA annot interface { Result_util.map2 (fun x y -> x::y) $1 $4 }
  ;

agent_modif:
  | annot { None,$1 }
  | annot PLUS annot { Some Ast.Create,$3 }
  | annot MINUS annot { Some Ast.Erase,$3 }
  ;

agent_no_err:
  | DOT annot { Ok (Ast.Absent (rhs_pos 1),$2) }
  | ID annot OP_PAR annot interface CL_PAR agent_modif
    { let modif,an = $7 in
      Result_util.map (fun i -> Ast.Present (($1,rhs_pos 1), i, modif),an) $5 }
  ;

agent:
  | agent_no_err { $1 }
  | ID annot recovery
    { Error (add_pos ("Malformed agent '"^$1^"'")) }
  ;

pattern_no_err:
  | agent_no_err COMMA annot pattern_no_err
    { Result_util.map2 (fun (x,_) y -> x::y) $1 $4 }
  | agent_no_err { Result_util.map (fun (x,_) -> [x]) $1 }
  ;

pattern:
  | agent COMMA annot pattern
    { Result_util.map2 (fun (x,_) (y,p) -> x::y,p) $1 $4 }
/*
  | agent pattern
    { Result_util.map2 (fun (x,_) (y,p) -> x::y,p) $1 $2 }
*/
  | agent { Result_util.map (fun (x,p) -> [x],p) $1 }
  ;

constant:
  | nbr { add_pos (Alg_expr.CONST $1) }
  | EMAX { add_pos (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR)) }
  | TMAX { add_pos (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR)) }
  | CPUTIME { add_pos (Alg_expr.STATE_ALG_OP (Operator.CPUTIME)) }
  ;

variable:
  | PIPE annot ID annot PIPE { Ok (add_pos (Alg_expr.TOKEN_ID ($3))) }
  | PIPE annot pattern PIPE
    { Result_util. map (fun (x,_) -> add_pos (Alg_expr.KAPPA_INSTANCE x)) $3 }
  | ID { Ok (add_pos (Alg_expr.ALG_VAR ($1))) }
  | LABEL { Ok (add_pos (Alg_expr.ALG_VAR ($1))) }
  | TIME { Ok (add_pos (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR))) }
  | EVENT { Ok (add_pos (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR))) }
  | NULL_EVENT
    { Ok (add_pos (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR))) }
  ;

small_alg_expr:
  | OP_PAR annot alg_expr CL_PAR { Result_util.map fst $3 }
  | constant { Ok $1 }
  | variable { $1 }
  | MAX annot small_alg_expr annot small_alg_expr
    { Result_util.map2
        (fun x y -> add_pos (Alg_expr.BIN_ALG_OP(Operator.MAX,x,y)))
        $3 $5 }
  | MIN annot small_alg_expr annot small_alg_expr
    { Result_util.map2
        (fun x y -> add_pos (Alg_expr.BIN_ALG_OP(Operator.MIN,x,y)))
        $3 $5 }
  | EXPONENT annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.EXP,x))) $3 }
  | SINUS annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.SINUS,x))) $3 }
  | COSINUS annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.COSINUS,x))) $3 }
  | TAN annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.TAN,x))) $3 }
  | ABS annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.INT,x))) $3 }
  | SQRT annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.SQRT,x))) $3 }
  | LOG annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.LOG,x))) $3 }
  | MINUS annot small_alg_expr
    { Result_util.map
        (fun x -> add_pos (Alg_expr.UN_ALG_OP(Operator.UMINUS,x))) $3 }
  ;

alg_expr_up_to_mod:
  | small_alg_expr annot { Result_util.map (fun x -> (x,$2)) $1 }
  | alg_expr_up_to_mod POW annot small_alg_expr annot
    { Result_util.map2
        (fun (x,_) y -> add_pos (Alg_expr.BIN_ALG_OP(Operator.POW,x,y)),$5)
        $1 $4 }
  ;

alg_expr_up_to_prod:
  | alg_expr_up_to_mod { $1 }
  | alg_expr_up_to_mod MOD annot alg_expr_up_to_prod
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.BIN_ALG_OP(Operator.MODULO,x,y)),an)
        $1 $4 }
  ;

alg_expr_up_to_sum:
  | alg_expr_up_to_prod { $1 }
  | alg_expr_up_to_prod MULT annot alg_expr_up_to_sum
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.BIN_ALG_OP(Operator.MULT,x,y)),an)
        $1 $4 }
  | alg_expr_up_to_prod DIV annot alg_expr_up_to_sum
    { Result_util.map2
        (fun (x,_) (y,an) -> add_pos (Alg_expr.BIN_ALG_OP(Operator.DIV,x,y)),an)
        $1 $4 }
  ;

alg_expr_up_to_if:
  | alg_expr_up_to_sum { $1 }
  | alg_expr_up_to_sum PLUS annot alg_expr_up_to_if
    { Result_util.map2
        (fun (x,_) (y,an) -> add_pos (Alg_expr.BIN_ALG_OP(Operator.SUM,x,y)),an)
        $1 $4 }
  | alg_expr_up_to_sum MINUS annot alg_expr_up_to_if
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.BIN_ALG_OP(Operator.MINUS,x,y)),an)
        $1 $4 }

alg_expr:
  | alg_expr_up_to_if { $1 }
  | bool_expr THEN annot alg_expr ELSE annot small_alg_expr annot
    { match $1 with
      | Error _ as e -> e
      | Ok (b,_) ->
         Result_util.map2
           (fun (t,_) e -> add_pos (Alg_expr.IF(b,t,e)),$8) $4 $7 }
  ;

boolean:
  | TRUE { true }
  | FALSE { false }
  ;

small_bool_expr:
  | OP_PAR annot bool_expr CL_PAR { Result_util.map fst $3 }
  | TRUE { Ok (add_pos Alg_expr.TRUE) }
  | FALSE { Ok (add_pos Alg_expr.FALSE) }
  | NOT annot small_bool_expr
    { Result_util.map
        (fun e -> add_pos (Alg_expr.UN_BOOL_OP(Operator.NOT,e))) $3 }
  ;

bool_expr_comp:
  | small_bool_expr annot { Result_util.map (fun x -> (x,$2)) $1 }
  | alg_expr_up_to_if GREATER annot alg_expr
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.COMPARE_OP(Operator.GREATER,x,y)),an)
        $1 $4 }
  | alg_expr_up_to_if SMALLER annot alg_expr
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.COMPARE_OP(Operator.SMALLER,x,y)),an)
        $1 $4 }
  | alg_expr_up_to_if EQUAL annot alg_expr
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.COMPARE_OP(Operator.EQUAL,x,y)),an)
        $1 $4 }
  | alg_expr_up_to_if DIFF annot alg_expr
    { Result_util.map2
        (fun (x,_) (y,an) ->
          add_pos (Alg_expr.COMPARE_OP(Operator.DIFF,x,y)),an)
        $1 $4 }
  ;

bool_expr_no_or:
  | bool_expr_comp { $1 }
  | bool_expr_comp AND annot bool_expr_no_or
    { Result_util.map2
        (fun (x,_) (y,p) -> add_pos (Alg_expr.BIN_BOOL_OP(Operator.AND,x,y)),p)
        $1 $4 }
  ;

bool_expr:
  | bool_expr_no_or { $1 }
  | bool_expr_no_or OR annot bool_expr
    { Result_util.map2
        (fun (x,_) (y,p) -> add_pos (Alg_expr.BIN_BOOL_OP(Operator.OR,x,y)),p)
        $1 $4 }
  ;

standalone_bool_expr:
  | annot bool_expr EOF { Result_util.map fst $2 }
  | recovery { Error (add_pos "Problematic boolean expression") }
  ;

arrow:
  | RAR {false}
  | LRAR {true}
  ;

sum_token:
  | small_alg_expr annot ID annot
    { Result_util.map (fun x -> [(x,($3,rhs_pos 3))]) $1 }
  | small_alg_expr annot ID annot PLUS annot sum_token
    { Result_util.map2 (fun x l -> (x,($3,rhs_pos 3))::l) $1 $7 }
  ;

rule_side:
  | pattern { Result_util.map (fun (x,_) -> (x,[])) $1 }
  | pattern PIPE annot sum_token
    { Result_util.map2 (fun (mix,_) delta_token -> (mix, delta_token)) $1 $4 };
  | PIPE annot sum_token
    { Result_util.map (fun delta_token -> ([], delta_token)) $3 };
  | pattern PIPE annot recovery
    { Error
	(add_pos  "Malformed token expression, I was expecting a_0 t_0 + ... \
+ a_n t_n, where t_i are tokens and a_i any algebraic formula") }
  | PIPE annot recovery
    { Error
	(add_pos  "Malformed token expression, I was expecting a_0 t_0 + ... \
+ a_n t_n, where t_i are tokens and a_i any algebraic formula") }
  ;

rule_content:
  | rule_side arrow annot rule_side
    { Result_util.map2
        (fun (lhs,rm_token) (rhs,add_token) ->
          (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs; Ast.add_token},$2))
      $1 $4
    }
  | rule_side arrow
    { Result_util.map
        (fun (lhs,rm_token) ->
          (Ast.Arrow {Ast.lhs; Ast.rm_token; Ast.rhs=[]; Ast.add_token=[]},$2))
      $1
    }
  | arrow annot rule_side
    { Result_util.map
        (fun (rhs,add_token) ->
          (Ast.Arrow {Ast.lhs=[]; Ast.rm_token=[]; Ast.rhs; Ast.add_token},$1))
    $3
    }
  | rule_side
    { Result_util.map
        (fun (mix,delta_token) -> Ast.Edit {Ast.mix; Ast.delta_token},false)
        $1 }
  ;

alg_with_radius:
  | alg_expr { Result_util.map (fun (a,_) -> (a,None)) $1 }
  | alg_expr SEMICOLON annot alg_expr
    { Result_util.map2 (fun (a,_) (r,_) -> (a, Some r)) $1 $4 }
  ;

rate:
  | OP_CUR annot alg_with_radius CL_CUR annot alg_expr
    { Result_util.map2 (fun u (b,an) -> (b,Some u,an)) $3 $6 }
  | alg_expr OP_CUR annot alg_with_radius CL_CUR annot
    { Result_util.map2 (fun (b,_) u -> (b,Some u,$6)) $1 $4 }
  | alg_expr { Result_util.map (fun (a,an) -> (a,None,an)) $1 }
  | OP_CUR annot CL_CUR annot alg_expr
    { Result_util.map
        (fun (b,an) ->
          (b, Some (Locality.dummy_annot (Alg_expr.CONST Nbr.zero),None),an))
        $5 }
  ;

birate:
  | AT annot rate
    { Result_util.map (fun (k2,k1,an) -> (k2,k1,None,None,an)) $3 }
  | AT annot rate COMMA annot rate
    { Result_util.map2
        (fun (k2,k1,_) (kback,kback1,an) ->
          (k2,k1,Some kback,kback1,an)) $3 $6 }
  | recovery {Error (add_pos "rule rate expected")}
  ;

rule:
  | rule_content birate
    { Result_util.map2
        (fun (k_def,k_un,k_op,k_op_un,_annot) (rewrite,bidirectional) ->
          add_pos {
              Ast.rewrite;Ast.bidirectional;
              Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un;
        }) $2 $1 }
  ;

variable_declaration:
  | LABEL annot alg_expr
    { Result_util.map (fun (a,_) -> (($1,rhs_pos 1),a)) $3 }
  | ID annot alg_expr
    { Result_util.map (fun (a,_) -> (($1,rhs_pos 1),a)) $3 }
  | LABEL annot recovery
    { Error (add_pos ("Illegal definition of variable '"^$1^"'")) }
  | ID annot recovery
    { Error (add_pos ("Illegal definition of variable '"^$1^"'")) }
  ;

init_declaration:
  | alg_expr pattern_no_err
    { Result_util.map2
        (fun (n,_) p -> (None,n,(Ast.INIT_MIX p,rhs_pos 2))) $1 $2 }
  | alg_expr OP_PAR annot pattern CL_PAR annot
    { Result_util.map2
        (fun (n,_) (p,_) -> (None,n,(Ast.INIT_MIX p,rhs_pos 4))) $1 $4 }
  | alg_expr ID annot
    { Result_util.map (fun (a,_) -> (None,a,(Ast.INIT_TOK $2,rhs_pos 2))) $1 }
  | ID OP_CUR annot init_declaration CL_CUR annot
    { Result_util.map (fun (_,alg,init) -> (Some ($1,rhs_pos 1),alg,init)) $4 }
  | recovery
    { Error (add_pos "Malformed initial condition") }
  ;

value_list:
  | STRING annot {[$1, rhs_pos 1]}
  | STRING annot value_list {($1,rhs_pos 1)::$3}
  ;

nonempty_print_expr:
  | STRING annot
    { Ok ([Primitives.Str_pexpr (add_pos $1)],$2) }
  | alg_expr_up_to_if
    { Result_util.map (fun (a,p) -> [Primitives.Alg_pexpr a],p) $1 }
  | STRING annot DOT annot nonempty_print_expr
    { Result_util.map
      (fun (l,p) -> Primitives.Str_pexpr ($1, rhs_pos 1)::l,p) $5 }
  | alg_expr_up_to_if DOT annot nonempty_print_expr
    { Result_util.map2
      (fun (a,_) (l,p) -> Primitives.Alg_pexpr a::l,p) $1 $4 }
  ;

print_expr:
  | annot { Ok ([],$1) }
  | annot STRING annot
    { Ok ([Primitives.Str_pexpr (add_pos $2)],$3) }
  | annot OP_PAR annot nonempty_print_expr CL_PAR annot { $4 }
  ;

effect:
  | ASSIGN annot ID annot alg_expr
    { Result_util.map (fun (a,p) -> Ast.UPDATE (($3,rhs_pos 3),a),p) $5 }
  | ASSIGN annot LABEL annot alg_expr
    { Result_util.map (fun (a,p) -> Ast.UPDATE (($3,rhs_pos 3),a),p) $5 }
  | TRACK annot LABEL annot boolean annot
    { Ok (Ast.CFLOWLABEL ($5,($3,rhs_pos 3)),$6) }
  | TRACK annot pattern boolean annot
    { Result_util.map (fun (m,_) -> Ast.CFLOWMIX ($4,(m,rhs_pos 3)),$5) $3 }
  | FLUX annot nonempty_print_expr boolean annot
    { Result_util.map
        (fun (p,_) ->
          (if $4 then Ast.FLUX (Primitives.RELATIVE,p) else Ast.FLUXOFF p),$5)
        $3 }
  | FLUX annot nonempty_print_expr STRING annot boolean annot
    { Result_util.bind
        (fun (p,_) ->
          if $6 && $4 = "absolute" then
            Ok (Ast.FLUX (Primitives.ABSOLUTE,p),$7)
          else if $6 && $4 = "probability" then
            Ok (Ast.FLUX (Primitives.PROBABILITY,p),$7)
          else if $6 && $4 = "relative" then
            Ok (Ast.FLUX (Primitives.RELATIVE,p),$7)
          else Error ("Incorrect FLUX expression",rhs_pos 4)) $3 }
  | INTRO annot alg_expr pattern
    { Result_util.map2
      (fun (a,_) (m,p) -> Ast.INTRO (a,(m, rhs_pos 4)),p) $3 $4 }
  | INTRO annot error
    { Error
	(add_pos "Malformed perturbation instruction, I was expecting \
'$ADD alg_expression kappa_expression'") }
  | DELETE annot alg_expr pattern
    { Result_util.map2
      (fun (a,_) (m,p) -> Ast.DELETE (a,(m, rhs_pos 4)),p) $3 $4 }
  | DELETE annot error
    { Error
	(add_pos "Malformed perturbation instruction, I was expecting \
'$DEL alg_expression kappa_expression'") }
/*
  | ID annot LAR annot alg_expr
    { Result_util.map (fun (n,a) -> Ast.UPDATE_TOK (($1,rhs_pos 1),n),p) $5 }
*/
  | SNAPSHOT print_expr
    { Result_util.map (fun (s,p) -> Ast.SNAPSHOT s,p) $2 }
  | STOP print_expr
    { Result_util.map (fun (s,p) -> Ast.SNAPSHOT s,p) $2 }
  | PRINTF print_expr SMALLER annot nonempty_print_expr GREATER annot
    { Result_util.map2 (fun (f,_) (c,_) -> Ast.PRINT (f,c),$7) $2 $5 }
  | PLOTENTRY annot { Ok (Ast.PLOTENTRY,$2) }
  | SPECIES_OF print_expr pattern boolean annot
    { Result_util.map2
      (fun (f,_) (m,_) -> Ast.SPECIES_OF ($4,f,(m, rhs_pos 3)),$5) $2 $3 }
  ;

effect_list:
  | OP_PAR annot effect_list CL_PAR annot { $3 }
  | effect
    { Result_util.map (fun (e,p) -> ([e],p)) $1 }
  | effect SEMICOLON annot effect_list
    { Result_util.map2 (fun (e,_) (l,a) -> (e::l,a)) $1 $4 }
  ;

standalone_effect_list:
  | annot effect_list EOF { Result_util.map fst $2 }
  | recovery { Error (add_pos "Problematic effect list") }
  ;

perturbation_alarm:
  | annot { None }
  | annot ALARM annot nbr annot { Some $4 }
  ;

perturbation_post:
  | { Ok (None, []) }
  | REPEAT annot bool_expr { Result_util.map (fun (b,p) -> Some b,p) $3 }
  ;

perturbation_declaration:
  | perturbation_alarm bool_expr DO annot effect_list perturbation_post
    { match $2 with
      | Error e -> Error e
      | Ok (pre,_) ->
         Result_util.map2 (fun (e,_) (post,_) -> ($1,Some pre,e,post)) $5 $6 }
  | perturbation_alarm DO annot effect_list perturbation_post
    { Result_util.map2 (fun (e,_) (post,_) -> ($1,None,e,post)) $4 $5 }
  ;

sentence:
  | LABEL annot rule
    { Result_util.map
        (fun (r,_) -> Ast.RULE(Some ($1, rhs_pos 1),(r, rhs_pos 3))) $3 }
  | LABEL annot EQUAL annot alg_expr
    { Result_util.map
        (fun (r,_) -> Ast.DECLARE (($1,rhs_pos 1),r)) $5 }
  | rule
    { Result_util.map
        (fun (r,_) -> Ast.RULE (None,(r, rhs_pos 1))) $1 }
  | SIGNATURE annot agent
    { Result_util.map (fun (r,_) -> Ast.SIG r) $3 }
  | SIGNATURE annot recovery
    { Error (add_pos "Malformed agent signature") }
  | TOKEN annot ID annot
    { Ok (Ast.TOKENSIG ($3,rhs_pos 3)) }
  | PLOT annot alg_expr
    { Result_util.map (fun (r,_) -> Ast.PLOT r) $3 }
  | PLOT annot recovery
    { Error
        (add_pos
           "Malformed plot instruction, an algebraic expression is expected") }
  | LET annot variable_declaration
    { Result_util.map (fun x -> Ast.DECLARE x) $3 }
  | OBS annot variable_declaration
    { Result_util.map (fun x -> Ast.OBS x) $3 }
  | INIT annot init_declaration
    { Result_util.map
        (fun (opt_vol,alg,init) -> Ast.INIT (opt_vol,alg,init)) $3 }
  | PERT perturbation_declaration
    { Result_util.map (fun p -> Ast.PERT (add_pos p)) $2 }
  | CONFIG annot STRING annot value_list
    { Ok (Ast.CONFIG (($3,rhs_pos 3),$5)) }
  | UNKNOWN annot
    { Error ("Unparsable sequence\""^$1^"\"",rhs_pos 1) }
  | recovery
    { Error (add_pos "Incorrect beginning of sentence") }
  ;

model_body:
  | sentence model_body
    { let out,err = $2 in
      match $1 with
      | Ok o -> o::out,err
      | Error e -> out,e::err }
  | EOF { ([],[]) }
  ;

model:
  | annot model_body { $2 }
  ;

interactive_command:
  | annot RUN annot EOF { Ok (Ast.RUN (Locality.dummy_annot Alg_expr.FALSE)) }
  | annot RUN annot bool_expr EOF
    { Result_util.map (fun (b,_) -> Ast.RUN b) $4 }
  | annot effect_list EOF { Result_util.map (fun (x,_) -> Ast.MODIFY x) $2 }
  | annot EOF { Ok Ast.QUIT }
  | recovery { Error (add_pos "Unrecognized command") }
  ;
