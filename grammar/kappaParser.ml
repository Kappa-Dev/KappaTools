exception Error

type token = 
  | UNTIL of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 8 "kappaParser.ml"
)
  | TRUE of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 13 "kappaParser.ml"
)
  | TMAX of (
# 8 "kappaParser.mly"
       (Tools.pos)
# 18 "kappaParser.ml"
)
  | TIME of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 23 "kappaParser.ml"
)
  | TAN of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 28 "kappaParser.ml"
)
  | STOP of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 33 "kappaParser.ml"
)
  | SQRT of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 38 "kappaParser.ml"
)
  | SNAPSHOT of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 43 "kappaParser.ml"
)
  | SMALLER of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 48 "kappaParser.ml"
)
  | SINUS of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 53 "kappaParser.ml"
)
  | SIGNATURE of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 58 "kappaParser.ml"
)
  | SET of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 63 "kappaParser.ml"
)
  | REF of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 68 "kappaParser.ml"
)
  | POW of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 73 "kappaParser.ml"
)
  | PLUS_RADIUS of (
# 11 "kappaParser.mly"
       (int)
# 78 "kappaParser.ml"
)
  | PLUS of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 83 "kappaParser.ml"
)
  | PLOT of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 88 "kappaParser.ml"
)
  | PIPE
  | PERT of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 94 "kappaParser.ml"
)
  | OR of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 99 "kappaParser.ml"
)
  | OP_PAR
  | OBS of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 105 "kappaParser.ml"
)
  | NOT of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 110 "kappaParser.ml"
)
  | NEWLINE
  | MULT of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 116 "kappaParser.ml"
)
  | MODULO of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 121 "kappaParser.ml"
)
  | MINUS of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 126 "kappaParser.ml"
)
  | LOG of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 131 "kappaParser.ml"
)
  | LET of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 136 "kappaParser.ml"
)
  | LABEL of (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 141 "kappaParser.ml"
)
  | KAPPA_WLD of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 146 "kappaParser.ml"
)
  | KAPPA_SEMI of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 151 "kappaParser.ml"
)
  | KAPPA_RAR
  | KAPPA_NOPOLY of (
# 8 "kappaParser.mly"
       (Tools.pos)
# 157 "kappaParser.ml"
)
  | KAPPA_MRK of (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 162 "kappaParser.ml"
)
  | KAPPA_LNK
  | INTRO of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 168 "kappaParser.ml"
)
  | INT of (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 173 "kappaParser.ml"
)
  | INIT of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 178 "kappaParser.ml"
)
  | INFINITY of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 183 "kappaParser.ml"
)
  | ID of (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 188 "kappaParser.ml"
)
  | GREATER of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 193 "kappaParser.ml"
)
  | FLOAT of (
# 12 "kappaParser.mly"
       (float*Tools.pos)
# 198 "kappaParser.ml"
)
  | FALSE of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 203 "kappaParser.ml"
)
  | EXPONENT of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 208 "kappaParser.ml"
)
  | EVENT of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 213 "kappaParser.ml"
)
  | EQUAL of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 218 "kappaParser.ml"
)
  | EOF
  | EMAX of (
# 8 "kappaParser.mly"
       (Tools.pos)
# 224 "kappaParser.ml"
)
  | DOT_RADIUS of (
# 11 "kappaParser.mly"
       (int)
# 229 "kappaParser.ml"
)
  | DOT
  | DO of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 235 "kappaParser.ml"
)
  | DIV of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 240 "kappaParser.ml"
)
  | DELETE of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 245 "kappaParser.ml"
)
  | COSINUS of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 250 "kappaParser.ml"
)
  | COMMA
  | CL_PAR
  | AT
  | AND of (
# 6 "kappaParser.mly"
       (Tools.pos)
# 258 "kappaParser.ml"
)
  | ABS of (
# 7 "kappaParser.mly"
       (Tools.pos)
# 263 "kappaParser.ml"
)

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState154
  | MenhirState151
  | MenhirState150
  | MenhirState149
  | MenhirState148
  | MenhirState146
  | MenhirState144
  | MenhirState139
  | MenhirState135
  | MenhirState129
  | MenhirState128
  | MenhirState126
  | MenhirState123
  | MenhirState122
  | MenhirState120
  | MenhirState115
  | MenhirState113
  | MenhirState109
  | MenhirState108
  | MenhirState104
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState98
  | MenhirState95
  | MenhirState93
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState77
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState49
  | MenhirState48
  | MenhirState45
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState32
  | MenhirState21
  | MenhirState8
  | MenhirState6
  | MenhirState5
  | MenhirState2
  | MenhirState0


# 1 "kappaParser.mly"
  

# 348 "kappaParser.ml"
let _eRR =
  Error

let rec _menhir_goto_ne_interface_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ne_interface_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 360 "kappaParser.ml"
        )) * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ne_interface_expression) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv735 * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_3 : 'tv_ne_interface_expression) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_ne_interface_expression = 
# 296 "kappaParser.mly"
 (Ast.PORT_SEP(_1,_3))
# 372 "kappaParser.ml"
         in
        _menhir_goto_ne_interface_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv736)) : 'freshtv738)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv741 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 380 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ne_interface_expression) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv739) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : 'tv_ne_interface_expression) = _v in
        ((let _v : 'tv_interface_expression = 
# 291 "kappaParser.mly"
 (_1)
# 391 "kappaParser.ml"
         in
        _menhir_goto_interface_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv740)) : 'freshtv742)
    | _ ->
        _menhir_fail ()

and _menhir_goto_multiple_mixture : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_multiple_mixture -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv729 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 405 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_multiple_mixture) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv727 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 413 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_multiple_mixture) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_modif_expr = 
# 131 "kappaParser.mly"
 (let (alg,mix) = _2 in Ast.INTRO (alg,mix,_1))
# 421 "kappaParser.ml"
         in
        _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)) : 'freshtv730)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv733 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 429 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_multiple_mixture) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv731 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 437 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_multiple_mixture) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_modif_expr = 
# 135 "kappaParser.mly"
 (let (alg,mix) = _2 in Ast.DELETE (alg,mix,_1))
# 445 "kappaParser.ml"
         in
        _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv732)) : 'freshtv734)
    | _ ->
        _menhir_fail ()

and _menhir_goto_rate : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rate -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv725 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_rate) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv723 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let (_6 : 'tv_rate) = _v in
    ((let ((((_menhir_stack, _menhir_s, _1), _, _2), _3), _, _4) = _menhir_stack in
    let _v : 'tv_rule_expression = 
# 167 "kappaParser.mly"
 (let (k2,k1) = _6 in 
		(_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=k2; Ast.k_un=k1})
	)
# 467 "kappaParser.ml"
     in
    _menhir_goto_rule_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)) : 'freshtv726)

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 474 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv721 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 483 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv722)

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 527 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv719 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 536 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv720)

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 580 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv717 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 589 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv718)

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 633 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv715 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 642 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv716)

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 686 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv713 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 695 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv714)

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 739 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv711 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 748 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv712)

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 792 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv709 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 801 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv710)

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 845 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 854 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv708)

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_alg_expr -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 898 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 907 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv706)

and _menhir_run69 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_alg_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv703 * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
    let _v : 'tv_alg_expr = 
# 205 "kappaParser.mly"
 (_2)
# 958 "kappaParser.ml"
     in
    _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv704)

and _menhir_goto_rule_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rule_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv701 * _menhir_state * 'tv_rule_expression) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv699 * _menhir_state * 'tv_rule_expression) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | NEWLINE ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv700)) : 'freshtv702)

and _menhir_goto_arrow : _menhir_env -> 'ttv_tail -> 'tv_arrow -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv697 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv695 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | OP_PAR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | AT | EOF | NEWLINE ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv696)) : 'freshtv698)

and _menhir_error12 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1007 "kappaParser.ml"
)) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1011 "kappaParser.ml"
)) * _menhir_state * 'tv_internal_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv693 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1018 "kappaParser.ml"
    )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1022 "kappaParser.ml"
    )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
    ((let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv691) = Obj.magic _menhir_stack in
    ((let _v : 'tv_link_state = 
# 334 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid link state"))
# 1030 "kappaParser.ml"
     in
    _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv692)) : 'freshtv694)

and _menhir_goto_link_state : _menhir_env -> 'ttv_tail -> 'tv_link_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv689 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1040 "kappaParser.ml"
    )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1044 "kappaParser.ml"
    )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
    let (_v : 'tv_link_state) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv687 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1051 "kappaParser.ml"
    )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
    let (_3 : 'tv_link_state) = _v in
    ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
    let _v : 'tv_port_expression = 
# 311 "kappaParser.mly"
 (let (id,pos) = _1 in {Ast.port_nme=id; Ast.port_int=_2; Ast.port_lnk=_3; Ast.port_pos=pos})
# 1058 "kappaParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv685) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_port_expression) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv683 * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv681 * _menhir_state * 'tv_port_expression) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv675 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1078 "kappaParser.ml"
        )) * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1085 "kappaParser.ml"
        )) * _menhir_state * 'tv_port_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv674)) : 'freshtv676)
    | CL_PAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv677 * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_ne_interface_expression = 
# 298 "kappaParser.mly"
 (Ast.PORT_SEP(_1,Ast.EMPTY_INTF))
# 1102 "kappaParser.ml"
         in
        _menhir_goto_ne_interface_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv678)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv679 * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv680)) : 'freshtv682)) : 'freshtv684)) : 'freshtv686)) : 'freshtv688)) : 'freshtv690)

and _menhir_goto_non_empty_mixture : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_non_empty_mixture -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv647 * _menhir_state) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv645 * _menhir_state) * _menhir_state * 'tv_non_empty_mixture) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CL_PAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv639 * _menhir_state) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : 'tv_non_empty_mixture = 
# 254 "kappaParser.mly"
 (_2)
# 1136 "kappaParser.ml"
             in
            _menhir_goto_non_empty_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)) : 'freshtv642)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)) : 'freshtv648)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv651 * _menhir_state * 'tv_agent_expression) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv649 * _menhir_state * 'tv_agent_expression) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_non_empty_mixture = 
# 256 "kappaParser.mly"
 (Ast.COMMA (_1,_3))
# 1155 "kappaParser.ml"
         in
        _menhir_goto_non_empty_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv650)) : 'freshtv652)
    | MenhirState115 | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv655 * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv653 * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_multiple_mixture = 
# 249 "kappaParser.mly"
 ((Ast.FLOAT (1.,Tools.no_pos),_1))
# 1167 "kappaParser.ml"
         in
        _menhir_goto_multiple_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)) : 'freshtv656)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv659 * _menhir_state * 'tv_alg_expr) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv657 * _menhir_state * 'tv_alg_expr) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_multiple_mixture = 
# 247 "kappaParser.mly"
 ((_1,_2))
# 1179 "kappaParser.ml"
         in
        _menhir_goto_multiple_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv658)) : 'freshtv660)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv663 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1187 "kappaParser.ml"
        )) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv661 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1193 "kappaParser.ml"
        )) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_variable_declaration = 
# 94 "kappaParser.mly"
                          (Ast.VAR_KAPPA (_2,_1))
# 1199 "kappaParser.ml"
         in
        _menhir_goto_variable_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv662)) : 'freshtv664)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv667 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1207 "kappaParser.ml"
        )) * 'tv_multiple) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv665 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1213 "kappaParser.ml"
        )) * 'tv_multiple) * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
        let _v : 'tv_instruction = 
# 77 "kappaParser.mly"
 (Ast.INIT (_2,_3,_1))
# 1219 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv666)) : 'freshtv668)
    | MenhirState144 | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv671 * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv669 * _menhir_state * 'tv_non_empty_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_mixture = 
# 162 "kappaParser.mly"
 (_1)
# 1231 "kappaParser.ml"
         in
        _menhir_goto_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv670)) : 'freshtv672)
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_internal_state = 
# 315 "kappaParser.mly"
          ([])
# 1242 "kappaParser.ml"
     in
    _menhir_goto_internal_state _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1249 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv637 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1258 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | KAPPA_MRK _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | CL_PAR | COMMA | KAPPA_LNK | KAPPA_WLD _ ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv638)

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1274 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv635) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1284 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_modif_expr = 
# 141 "kappaParser.mly"
 (Ast.STOP _1)
# 1289 "kappaParser.ml"
     in
    _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1296 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv633) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1306 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_modif_expr = 
# 139 "kappaParser.mly"
 (Ast.SNAPSHOT _1)
# 1311 "kappaParser.ml"
     in
    _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv634)

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv631 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DELETE _v ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | INTRO _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | LABEL _v ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | OP_PAR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | SNAPSHOT _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | STOP _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv632)

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1343 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv629 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1352 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | SET _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1361 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1366 "kappaParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv623 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1374 "kappaParser.ml"
        )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1378 "kappaParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ABS _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | COSINUS _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | EMAX _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | EVENT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | EXPONENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | FLOAT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | INFINITY _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | INT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | LABEL _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | LOG _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | OP_PAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SINUS _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | SQRT _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | TAN _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | TIME _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | TMAX _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv624)) : 'freshtv626)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv627 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 1425 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv628)) : 'freshtv630)

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1433 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv621 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1442 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | OP_PAR ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv622)

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1488 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv619 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1497 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | OP_PAR ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv620)

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bool_expr -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1543 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv617 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1552 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FALSE _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NOT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | OP_PAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TRUE _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv618)

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_bool_expr -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1602 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv615 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1611 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FALSE _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NOT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | OP_PAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TRUE _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv616)

and _menhir_goto_alg_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_alg_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv485 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1667 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1673 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 231 "kappaParser.mly"
 (Ast.ABS (_2,_1))
# 1679 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv489 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1687 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv487 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1693 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 227 "kappaParser.mly"
 (Ast.COSINUS (_2,_1))
# 1699 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)) : 'freshtv490)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv493 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1707 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1713 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 223 "kappaParser.mly"
 (Ast.EXP (_2,_1))
# 1719 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)) : 'freshtv494)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1727 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv495 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1733 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 235 "kappaParser.mly"
 (Ast.LOG (_2,_1))
# 1739 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)) : 'freshtv498)
    | MenhirState104 | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CL_PAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv500)) : 'freshtv502)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1774 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1782 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | AND _ | CL_PAR | DIV _ | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | MODULO _ | MULT _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv503 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1793 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 219 "kappaParser.mly"
 (Ast.POW (_1,_3,_2))
# 1799 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv506)) : 'freshtv508)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1811 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1819 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | AND _ | CL_PAR | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1836 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 213 "kappaParser.mly"
 (Ast.SUM (_1,_3,_2))
# 1842 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv512)) : 'freshtv514)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv519 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1854 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv517 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1862 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | AND _ | CL_PAR | DIV _ | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | MULT _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1875 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 211 "kappaParser.mly"
 (Ast.MULT (_1,_3,_2))
# 1881 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv518)) : 'freshtv520)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1893 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv523 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1901 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | AND _ | CL_PAR | DIV _ | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | MODULO _ | MULT _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv521 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1912 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 221 "kappaParser.mly"
 (Ast.MODULO (_1,_3,_2))
# 1918 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv522)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv524)) : 'freshtv526)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv531 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1930 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv529 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1938 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | AND _ | CL_PAR | DIV _ | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | MULT _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 1951 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 215 "kappaParser.mly"
 (Ast.DIV (_1,_3,_2))
# 1957 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv530)) : 'freshtv532)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1969 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1977 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | AND _ | CL_PAR | DO _ | EOF | EQUAL _ | GREATER _ | ID _ | MINUS _ | NEWLINE | OP_PAR | OR _ | PIPE | PLUS _ | SMALLER _ | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv533 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 1994 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_alg_expr = 
# 217 "kappaParser.mly"
 (Ast.MINUS (_1,_3,_2))
# 2000 "kappaParser.ml"
             in
            _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv534)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv536)) : 'freshtv538)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv541 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2012 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv539 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2018 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 225 "kappaParser.mly"
 (Ast.SINUS (_2,_1))
# 2024 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2032 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv543 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2038 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 233 "kappaParser.mly"
 (Ast.SQRT (_2,_1))
# 2044 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)) : 'freshtv546)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv549 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2052 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv547 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2058 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_alg_expr = 
# 229 "kappaParser.mly"
 (Ast.TAN (_2,_1))
# 2064 "kappaParser.ml"
         in
        _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)) : 'freshtv550)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv555 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2072 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv553 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2080 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv551 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2101 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _, _2) = _menhir_stack in
            let _v : 'tv_instruction = 
# 85 "kappaParser.mly"
 (Ast.PLOT _2)
# 2107 "kappaParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv554)) : 'freshtv556)
    | MenhirState74 | MenhirState120 | MenhirState88 | MenhirState90 | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | EQUAL _v ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | GREATER _v ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | SMALLER _v ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv558)) : 'freshtv560)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv565 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2150 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv563 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2158 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | AND _ | CL_PAR | DO _ | EOF | NEWLINE | OR _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2179 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_bool_expr = 
# 115 "kappaParser.mly"
 (Ast.SMALLER (_1,_3,_2))
# 2185 "kappaParser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv564)) : 'freshtv566)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv571 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2197 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv569 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2205 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | AND _ | CL_PAR | DO _ | EOF | NEWLINE | OR _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2226 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_bool_expr = 
# 113 "kappaParser.mly"
 (Ast.GREATER (_1,_3,_2))
# 2232 "kappaParser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv568)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv570)) : 'freshtv572)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv577 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2244 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv575 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2252 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | AND _ | CL_PAR | DO _ | EOF | NEWLINE | OR _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv573 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2273 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : 'tv_bool_expr = 
# 117 "kappaParser.mly"
 (Ast.EQUAL (_1,_3,_2))
# 2279 "kappaParser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv576)) : 'freshtv578)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv581 * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv579 * _menhir_state) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CL_PAR ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | EQUAL _v ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | GREATER _v ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | SMALLER _v ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv580)) : 'freshtv582)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv587 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2324 "kappaParser.ml"
        )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2328 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv585 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2336 "kappaParser.ml"
        )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2340 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | CL_PAR | EOF | NEWLINE | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv583 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2361 "kappaParser.ml"
            )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 2365 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
            let _v : 'tv_modif_expr = 
# 137 "kappaParser.mly"
 (let lab,pos_lab = _1 in Ast.UPDATE (lab,pos_lab,_3,_2))
# 2371 "kappaParser.ml"
             in
            _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv586)) : 'freshtv588)
    | MenhirState115 | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591 * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | ID _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | OP_PAR ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv590)) : 'freshtv592)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2412 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2420 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv593 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2441 "kappaParser.ml"
            )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : 'tv_variable_declaration = 
# 95 "kappaParser.mly"
                 (Ast.VAR_ALG (_2,_1))
# 2447 "kappaParser.ml"
             in
            _menhir_goto_variable_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv594)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv596)) : 'freshtv598)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv607 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv605 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | PIPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv601 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState148 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv599 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) * _menhir_state) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ABS _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | COSINUS _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | EMAX _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | EVENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | EXPONENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | FLOAT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | INFINITY _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | LABEL _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | LOG _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | OP_PAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | SINUS _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | SQRT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | TAN _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | TIME _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | TMAX _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv600)) : 'freshtv602)
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv603 * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_rate = 
# 242 "kappaParser.mly"
 ((_1,None))
# 2528 "kappaParser.ml"
             in
            _menhir_goto_rate _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv606)) : 'freshtv608)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv613 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv611 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) * _menhir_state) * _menhir_state * 'tv_alg_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DIV _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | MINUS _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | MODULO _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | MULT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | PLUS _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | POW _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv609 * _menhir_state * 'tv_alg_expr) * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _), _, _3) = _menhir_stack in
            let _v : 'tv_rate = 
# 240 "kappaParser.mly"
 ((_1,Some _3))
# 2563 "kappaParser.ml"
             in
            _menhir_goto_rate _menhir_env _menhir_stack _menhir_s _v) : 'freshtv610)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv612)) : 'freshtv614)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mixture : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mixture -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | KAPPA_NOPOLY _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            let (_v : (
# 8 "kappaParser.mly"
       (Tools.pos)
# 2592 "kappaParser.ml"
            )) = _v in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv457) = Obj.magic _menhir_stack in
            let (_1 : (
# 8 "kappaParser.mly"
       (Tools.pos)
# 2600 "kappaParser.ml"
            )) = _v in
            ((let _v : 'tv_arrow = 
# 178 "kappaParser.mly"
 (Ast.RAR_NOPOLY _1)
# 2605 "kappaParser.ml"
             in
            _menhir_goto_arrow _menhir_env _menhir_stack _v) : 'freshtv458)) : 'freshtv460)
        | KAPPA_RAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv461) = Obj.magic _menhir_stack in
            ((let _v : 'tv_arrow = 
# 176 "kappaParser.mly"
 (Ast.RAR)
# 2617 "kappaParser.ml"
             in
            _menhir_goto_arrow _menhir_env _menhir_stack _v) : 'freshtv462)) : 'freshtv464)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)) : 'freshtv470)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv481 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv479 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv473 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv471 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ABS _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | COSINUS _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | EMAX _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | EVENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | EXPONENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | FLOAT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | INFINITY _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | LABEL _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | LOG _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | OP_PAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | SINUS _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | SQRT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | TAN _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | TIME _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | TMAX _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv472)) : 'freshtv474)
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv475 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, _1), _, _2), _3), _, _4) = _menhir_stack in
            let _v : 'tv_rule_expression = 
# 171 "kappaParser.mly"
 ((_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=(Ast.FLOAT (1.0,Tools.no_pos)); Ast.k_un=None}))
# 2687 "kappaParser.ml"
             in
            _menhir_goto_rule_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv477 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)
    | _ ->
        _menhir_fail ()

and _menhir_goto_internal_state : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_internal_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv415 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2709 "kappaParser.ml"
        )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv413 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2715 "kappaParser.ml"
        )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_internal_state = 
# 317 "kappaParser.mly"
 (let m,pos = _1 in m::_2)
# 2721 "kappaParser.ml"
         in
        _menhir_goto_internal_state _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)) : 'freshtv416)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv455 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2729 "kappaParser.ml"
        )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2733 "kappaParser.ml"
        )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv453 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2741 "kappaParser.ml"
        )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2745 "kappaParser.ml"
        )) * _menhir_state * 'tv_internal_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | KAPPA_LNK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2754 "kappaParser.ml"
            )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2758 "kappaParser.ml"
            )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2765 "kappaParser.ml"
            )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2769 "kappaParser.ml"
            )) * _menhir_state * 'tv_internal_state) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv431 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2778 "kappaParser.ml"
                )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2782 "kappaParser.ml"
                )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
                let (_v : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2787 "kappaParser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv429 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2795 "kappaParser.ml"
                )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2799 "kappaParser.ml"
                )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2803 "kappaParser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | DOT ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv425 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2812 "kappaParser.ml"
                    )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2816 "kappaParser.ml"
                    )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2820 "kappaParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv423 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2827 "kappaParser.ml"
                    )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2831 "kappaParser.ml"
                    )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2835 "kappaParser.ml"
                    )) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | ID _v ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ((('freshtv419 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2844 "kappaParser.ml"
                        )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2848 "kappaParser.ml"
                        )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2852 "kappaParser.ml"
                        )) = Obj.magic _menhir_stack in
                        let (_v : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2857 "kappaParser.ml"
                        )) = _v in
                        ((let _ = _menhir_discard _menhir_env in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ('freshtv417) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2864 "kappaParser.ml"
                        )) = Obj.magic _menhir_stack in
                        let (_4 : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2869 "kappaParser.ml"
                        )) = _v in
                        ((let (_menhir_stack, _2) = _menhir_stack in
                        let _v : 'tv_link_state = 
# 330 "kappaParser.mly"
 (Ast.LNK_TYPE (_2,_4))
# 2875 "kappaParser.ml"
                         in
                        _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv418)) : 'freshtv420)
                    | _ ->
                        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                        _menhir_env._menhir_shifted <- (-1);
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : ((('freshtv421 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2885 "kappaParser.ml"
                        )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2889 "kappaParser.ml"
                        )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2893 "kappaParser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _) = _menhir_stack in
                        _menhir_error12 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv422)) : 'freshtv424)) : 'freshtv426)
                | _ ->
                    assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv427 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2904 "kappaParser.ml"
                    )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2908 "kappaParser.ml"
                    )) * _menhir_state * 'tv_internal_state) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2912 "kappaParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _) = _menhir_stack in
                    _menhir_error12 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv428)) : 'freshtv430)) : 'freshtv432)
            | INT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv435 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2921 "kappaParser.ml"
                )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2925 "kappaParser.ml"
                )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
                let (_v : (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 2930 "kappaParser.ml"
                )) = _v in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
                let (_2 : (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 2938 "kappaParser.ml"
                )) = _v in
                ((let _v : 'tv_link_state = 
# 326 "kappaParser.mly"
 (Ast.LNK_VALUE _2)
# 2943 "kappaParser.ml"
                 in
                _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv434)) : 'freshtv436)
            | KAPPA_SEMI _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv439 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2951 "kappaParser.ml"
                )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2955 "kappaParser.ml"
                )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
                let (_v : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2960 "kappaParser.ml"
                )) = _v in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
                let (_2 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2968 "kappaParser.ml"
                )) = _v in
                ((let _v : 'tv_link_state = 
# 328 "kappaParser.mly"
 (Ast.LNK_SOME _2)
# 2973 "kappaParser.ml"
                 in
                _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv438)) : 'freshtv440)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_error12 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv442)) : 'freshtv444)
        | KAPPA_WLD _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv447 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2985 "kappaParser.ml"
            )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 2989 "kappaParser.ml"
            )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 2994 "kappaParser.ml"
            )) = _v in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
            let (_1 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 3002 "kappaParser.ml"
            )) = _v in
            ((let _v : 'tv_link_state = 
# 332 "kappaParser.mly"
 (Ast.LNK_ANY _1)
# 3007 "kappaParser.ml"
             in
            _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv446)) : 'freshtv448)
        | CL_PAR | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
            ((let _v : 'tv_link_state = 
# 324 "kappaParser.mly"
 (Ast.FREE)
# 3016 "kappaParser.ml"
             in
            _menhir_goto_link_state _menhir_env _menhir_stack _v) : 'freshtv450)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3026 "kappaParser.ml"
            )) * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3030 "kappaParser.ml"
            )) * _menhir_state * 'tv_internal_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)) : 'freshtv454)) : 'freshtv456)
    | _ ->
        _menhir_fail ()

and _menhir_goto_agent_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_agent_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 3046 "kappaParser.ml"
        )) * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv397 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 3052 "kappaParser.ml"
        )) * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_instruction = 
# 73 "kappaParser.mly"
 ((Ast.SIG (_2,_1)))
# 3058 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState139 | MenhirState144 | MenhirState135 | MenhirState123 | MenhirState115 | MenhirState102 | MenhirState113 | MenhirState108 | MenhirState109 | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_agent_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_agent_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | OP_PAR ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv402)) : 'freshtv404)
        | AT | CL_PAR | EOF | KAPPA_NOPOLY _ | KAPPA_RAR | NEWLINE | UNTIL _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv405 * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : 'tv_non_empty_mixture = 
# 258 "kappaParser.mly"
 (Ast.COMMA(_1,Ast.EMPTY_MIX))
# 3093 "kappaParser.ml"
             in
            _menhir_goto_non_empty_mixture _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)
    | _ ->
        _menhir_fail ()

and _menhir_goto_interface_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_interface_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv395 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3113 "kappaParser.ml"
    )) * _menhir_state * 'tv_interface_expression) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv393 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3121 "kappaParser.ml"
    )) * _menhir_state * 'tv_interface_expression) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CL_PAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3130 "kappaParser.ml"
        )) * _menhir_state * 'tv_interface_expression) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv387 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3137 "kappaParser.ml"
        )) * _menhir_state * 'tv_interface_expression) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _v : 'tv_agent_expression = 
# 282 "kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id; Ast.ag_intf=_3; Ast.ag_pos=pos})
# 3143 "kappaParser.ml"
         in
        _menhir_goto_agent_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)) : 'freshtv390)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv391 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3153 "kappaParser.ml"
        )) * _menhir_state * 'tv_interface_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3161 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv385 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3170 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | KAPPA_MRK _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CL_PAR | COMMA | KAPPA_LNK | KAPPA_WLD _ ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv386)

and _menhir_goto_bool_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bool_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3192 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3198 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : 'tv_bool_expr = 
# 119 "kappaParser.mly"
 (Ast.NOT (_2,_1))
# 3204 "kappaParser.ml"
         in
        _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv342)) : 'freshtv344)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state * 'tv_bool_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | CL_PAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv347 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv345 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : 'tv_bool_expr = 
# 107 "kappaParser.mly"
 (_2)
# 3228 "kappaParser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
        | OR _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)) : 'freshtv354)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv361 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3245 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv359 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3253 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | CL_PAR | DO _ | EOF | NEWLINE | OR _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv355 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3264 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
            let _v : 'tv_bool_expr = 
# 111 "kappaParser.mly"
 (Ast.OR (_1,_3,_2))
# 3270 "kappaParser.ml"
             in
            _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv357 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3280 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv365 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3289 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3295 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _1), _2), _, _3) = _menhir_stack in
        let _v : 'tv_bool_expr = 
# 109 "kappaParser.mly"
 (Ast.AND (_1,_3,_2))
# 3301 "kappaParser.ml"
         in
        _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv375 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3309 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3317 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | DO _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv369 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3328 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3333 "kappaParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3341 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3345 "kappaParser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DELETE _v ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | INTRO _v ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LABEL _v ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | OP_PAR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SNAPSHOT _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | STOP _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv368)) : 'freshtv370)
        | OR _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv371 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3374 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)) : 'freshtv376)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv383 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3383 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3387 "kappaParser.ml"
        )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3391 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv381 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3399 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3403 "kappaParser.ml"
        )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3407 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv377 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3420 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3424 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3428 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s, _1), _, _2), _), _, _4), _), _, _6) = _menhir_stack in
            let _v : 'tv_instruction = 
# 91 "kappaParser.mly"
 (Ast.PERT (_2,_4,_1,Some _6))
# 3434 "kappaParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv379 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3444 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3448 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3452 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_variable -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : 'tv_variable) = _v in
    ((let _v : 'tv_alg_expr = 
# 209 "kappaParser.mly"
 (_1)
# 3472 "kappaParser.ml"
     in
    _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)) : 'freshtv340)

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_constant -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_constant) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : 'tv_constant) = _v in
    ((let _v : 'tv_alg_expr = 
# 207 "kappaParser.mly"
 (_1)
# 3489 "kappaParser.ml"
     in
    _menhir_goto_alg_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)) : 'freshtv336)

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv331 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | OP_PAR ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv332)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_mixture = 
# 160 "kappaParser.mly"
 (Ast.EMPTY_MIX)
# 3545 "kappaParser.ml"
     in
    _menhir_goto_mixture _menhir_env _menhir_stack _menhir_s _v

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv329 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | OP_PAR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv330)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_start_rule : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "kappaParser.mly"
      (unit)
# 3574 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 25 "kappaParser.mly"
      (unit)
# 3585 "kappaParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : (
# 25 "kappaParser.mly"
      (unit)
# 3593 "kappaParser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_newline = 
# 31 "kappaParser.mly"
 (_2)
# 3599 "kappaParser.ml"
         in
        _menhir_goto_newline _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)) : 'freshtv324)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 25 "kappaParser.mly"
      (unit)
# 3609 "kappaParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : (
# 25 "kappaParser.mly"
      (unit)
# 3617 "kappaParser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv326)) : 'freshtv328)
    | _ ->
        _menhir_fail ()

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_internal_state = 
# 319 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid internal state"))
# 3632 "kappaParser.ml"
     in
    _menhir_goto_internal_state _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instruction -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_instruction) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | NEWLINE ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv316)) : 'freshtv318)

and _menhir_goto_modif_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_modif_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * 'tv_modif_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CL_PAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : 'tv_modif_expr = 
# 129 "kappaParser.mly"
 (_2)
# 3679 "kappaParser.ml"
             in
            _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv313 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3694 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3698 "kappaParser.ml"
        )) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv311 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3706 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3710 "kappaParser.ml"
        )) * _menhir_state * 'tv_modif_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | UNTIL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv305 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3719 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3723 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3728 "kappaParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv303 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3736 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3740 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3744 "kappaParser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ABS _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | COSINUS _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | EMAX _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | EVENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | EXPONENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | FALSE _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | FLOAT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | INFINITY _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | LABEL _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | LOG _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | NOT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | OP_PAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | SINUS _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | SQRT _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | TAN _v ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | TIME _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | TMAX _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | TRUE _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv304)) : 'freshtv306)
        | EOF | NEWLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv307 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3795 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3799 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, _1), _, _2), _), _, _4) = _menhir_stack in
            let _v : 'tv_instruction = 
# 89 "kappaParser.mly"
 (Ast.PERT (_2,_4,_1,None))
# 3805 "kappaParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv309 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3815 "kappaParser.ml"
            )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3819 "kappaParser.ml"
            )) * _menhir_state * 'tv_modif_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)) : 'freshtv314)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_variable_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3834 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_variable_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 3842 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_variable_declaration) = _v in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : 'tv_instruction = 
# 83 "kappaParser.mly"
 (Ast.OBS _2)
# 3850 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 3858 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_variable_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 3866 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_variable_declaration) = _v in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : 'tv_instruction = 
# 81 "kappaParser.mly"
 (Ast.DECLARE _2)
# 3874 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : (
# 25 "kappaParser.mly"
      (unit)
# 3888 "kappaParser.ml"
    ) = 
# 68 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Syntax error"))
# 3892 "kappaParser.ml"
     in
    _menhir_goto_start_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3899 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv281 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3908 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | OP_PAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3917 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3924 "kappaParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CL_PAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState5 in
            ((let _v : 'tv_interface_expression = 
# 289 "kappaParser.mly"
 (Ast.EMPTY_INTF)
# 3937 "kappaParser.ml"
             in
            _menhir_goto_interface_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv274)) : 'freshtv276)
    | AT | CL_PAR | COMMA | EOF | KAPPA_NOPOLY _ | KAPPA_RAR | NEWLINE | UNTIL _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3949 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_agent_expression = 
# 284 "kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id;Ast.ag_intf=Ast.EMPTY_INTF;Ast.ag_pos=pos})
# 3955 "kappaParser.ml"
         in
        _menhir_goto_agent_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 3965 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv269 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv270)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4018 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4028 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_bool_expr = 
# 121 "kappaParser.mly"
 (Ast.TRUE _1)
# 4033 "kappaParser.ml"
     in
    _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kappaParser.mly"
       (Tools.pos)
# 4040 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 8 "kappaParser.mly"
       (Tools.pos)
# 4050 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 191 "kappaParser.mly"
 (let pos = _1 in Ast.TMAX pos)
# 4055 "kappaParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4062 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4072 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_variable = 
# 198 "kappaParser.mly"
 (Ast.TIME_VAR _1)
# 4077 "kappaParser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4084 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv261 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4093 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv262)

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4137 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv259 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4146 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv260)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4190 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4199 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv258)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FALSE _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | NOT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | OP_PAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | TRUE _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv256)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4294 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv253 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4303 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FALSE _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NOT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | OP_PAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TRUE _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv254)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4353 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv251 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4362 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv252)

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 4406 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 4416 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_variable = 
# 196 "kappaParser.mly"
 (Ast.OBS_VAR _1)
# 4421 "kappaParser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 4428 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 4438 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 187 "kappaParser.mly"
 (let i,pos = _1 in Ast.FLOAT (float_of_int i,pos))
# 4443 "kappaParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4450 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4460 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 183 "kappaParser.mly"
 (Ast.INFINITY _1)
# 4465 "kappaParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "kappaParser.mly"
       (float*Tools.pos)
# 4472 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 12 "kappaParser.mly"
       (float*Tools.pos)
# 4482 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 185 "kappaParser.mly"
 (let f,pos = _1 in Ast.FLOAT (f,pos))
# 4487 "kappaParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv244)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4494 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv241) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 6 "kappaParser.mly"
       (Tools.pos)
# 4504 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_bool_expr = 
# 123 "kappaParser.mly"
 (Ast.FALSE _1)
# 4509 "kappaParser.ml"
     in
    _menhir_goto_bool_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4516 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv239 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4525 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv240)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4569 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4579 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_variable = 
# 200 "kappaParser.mly"
 (Ast.EVENT_VAR _1)
# 4584 "kappaParser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kappaParser.mly"
       (Tools.pos)
# 4591 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 8 "kappaParser.mly"
       (Tools.pos)
# 4601 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 189 "kappaParser.mly"
 (let pos = _1 in Ast.EMAX pos)
# 4606 "kappaParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4613 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4622 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv234)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4666 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4675 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv232)

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 4719 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv229 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 4728 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | OP_PAR ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv230)

and _menhir_goto_rule_label : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rule_label -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_rule_label) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_rule_label) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | OP_PAR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | KAPPA_NOPOLY _ | KAPPA_RAR ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv226)) : 'freshtv228)

and _menhir_error132 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4796 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv223 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4803 "kappaParser.ml"
    )) = Obj.magic _menhir_stack in
    ((let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv221 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4810 "kappaParser.ml"
    )) = Obj.magic _menhir_stack in
    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    let _v : 'tv_instruction = 
# 79 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed initial condition, I was expecting something of the form '%init: n kappa_expression'"))
# 4816 "kappaParser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)) : 'freshtv224)

and _menhir_goto_multiple : _menhir_env -> 'ttv_tail -> 'tv_multiple -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv219 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4827 "kappaParser.ml"
    )) * 'tv_multiple) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv217 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4835 "kappaParser.ml"
    )) * 'tv_multiple) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | OP_PAR ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv218)) : 'freshtv220)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_goto_newline : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_newline -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_rule_expression) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_newline) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_rule_expression) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_newline) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (
# 25 "kappaParser.mly"
      (unit)
# 4876 "kappaParser.ml"
        ) = 
# 40 "kappaParser.mly"
 (let rule_label,r = _1 in Ast.result := {!Ast.result with Ast.rules = (rule_label,r)::!Ast.result.Ast.rules} ; _2)
# 4880 "kappaParser.ml"
         in
        _menhir_goto_start_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)) : 'freshtv208)
    | MenhirState0 | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_newline) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : 'tv_newline) = _v in
        ((let _v : (
# 25 "kappaParser.mly"
      (unit)
# 4895 "kappaParser.ml"
        ) = 
# 38 "kappaParser.mly"
  (_1)
# 4899 "kappaParser.ml"
         in
        _menhir_goto_start_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_newline) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (_2 : 'tv_newline) = _v in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (
# 25 "kappaParser.mly"
      (unit)
# 4915 "kappaParser.ml"
        ) = 
# 42 "kappaParser.mly"
 (
		let inst = _1 in
		begin 
			match inst with
				| Ast.SIG (ag,pos) -> 
						(Ast.result:={!Ast.result with 
						Ast.signatures=(ag,pos)::!Ast.result.Ast.signatures}
						)
				| Ast.INIT (n,mix,pos) ->  
					(Ast.result := {!Ast.result with 
					Ast.init=(n,mix,pos)::!Ast.result.Ast.init})
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
		end ; _2 
	)
# 4943 "kappaParser.ml"
         in
        _menhir_goto_start_rule _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)) : 'freshtv216)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_rule_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv59 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv61 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv63 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv65 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) * _menhir_state * 'tv_mixture) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * 'tv_rule_label) * _menhir_state * 'tv_mixture) * 'tv_arrow) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_rule_label) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 4997 "kappaParser.ml"
        )) * 'tv_multiple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _) = _menhir_stack in
        _menhir_error132 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv72)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5006 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        (_menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState128 : 'freshtv76)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5019 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5028 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5034 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState123 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5042 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : 'tv_variable_declaration = 
# 97 "kappaParser.mly"
 (let str,pos = _1 in
		raise 
		(ExceptionDefn.Syntax_Error 
		(Printf.sprintf "Variable '%s' should be either a pure kappa expression or an algebraic expression on variables" str)
		) 
	)
# 5054 "kappaParser.ml"
         in
        _menhir_goto_variable_declaration _menhir_env _menhir_stack _menhir_s _v) : 'freshtv80)) : 'freshtv82)) : 'freshtv84)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5062 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv87 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5071 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5075 "kappaParser.ml"
        )) * _menhir_state * 'tv_modif_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5079 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5088 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_agent_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5117 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5123 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState102 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5131 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : 'tv_modif_expr = 
# 133 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed perturbation instruction, I was expecting '$(ADD) alg_expression kappa_expression'"))
# 5138 "kappaParser.ml"
         in
        _menhir_goto_modif_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)) : 'freshtv104)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5146 "kappaParser.ml"
        )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5150 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5159 "kappaParser.ml"
        )) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5163 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5177 "kappaParser.ml"
        )) * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5181 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5195 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_bool_expr) * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5204 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5213 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5222 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv123 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5231 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5240 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5249 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5258 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5272 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5286 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5295 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5304 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5313 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5322 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5331 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5340 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5349 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv153 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5358 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5367 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5376 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5385 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5394 "kappaParser.ml"
        )) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state * 'tv_alg_expr) * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5403 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_alg_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5417 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5426 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5435 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5444 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5458 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5467 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5476 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5485 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5491 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState32 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5499 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : 'tv_instruction = 
# 87 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed plot instruction, I was expecting an algebraic expression of variables"))
# 5506 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5514 "kappaParser.ml"
        )) * _menhir_state * 'tv_port_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5523 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 : 'freshtv192)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5531 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        (_menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 : 'freshtv194)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5539 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5548 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5554 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState2 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5562 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : 'tv_instruction = 
# 75 "kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))
# 5569 "kappaParser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
        (_menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 : 'freshtv204)

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_rule_label = 
# 151 "kappaParser.mly"
 ({Ast.lbl_nme = None ; Ast.lbl_ref = None})
# 5582 "kappaParser.ml"
     in
    _menhir_goto_rule_label _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5589 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5598 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv54)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5612 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5621 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LABEL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5630 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5635 "kappaParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5643 "kappaParser.ml"
        )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5647 "kappaParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | OP_PAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5656 "kappaParser.ml"
            )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5660 "kappaParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5667 "kappaParser.ml"
            )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5671 "kappaParser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LABEL _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv35 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5680 "kappaParser.ml"
                )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5684 "kappaParser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5689 "kappaParser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv33 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5697 "kappaParser.ml"
                )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5701 "kappaParser.ml"
                )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5705 "kappaParser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CL_PAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv29 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5714 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5718 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5722 "kappaParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _ = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv27 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5729 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5733 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5737 "kappaParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s, _), _2), _4) = _menhir_stack in
                    let _v : 'tv_rule_label = 
# 155 "kappaParser.mly"
 (let ref,pos = _2 and lab,pos' = _4 in {Ast.lbl_nme=Some (lab,pos') ; Ast.lbl_ref = Some (ref,pos)})
# 5743 "kappaParser.ml"
                     in
                    _menhir_goto_rule_label _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)) : 'freshtv30)
                | _ ->
                    assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv31 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5753 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5757 "kappaParser.ml"
                    )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5761 "kappaParser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)) : 'freshtv36)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5772 "kappaParser.ml"
                )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5776 "kappaParser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)) : 'freshtv42)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5787 "kappaParser.ml"
            )) * (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 5791 "kappaParser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5802 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)) : 'freshtv52)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5810 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5819 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | OP_PAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv26)

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5863 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv23 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5872 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | ABS _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | COSINUS _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | EMAX _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | EVENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | EXPONENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FALSE _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FLOAT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INFINITY _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LABEL _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LOG _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | NOT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | OP_PAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SINUS _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | SQRT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | TAN _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | TIME _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | TMAX _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | TRUE _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv24)

and _menhir_run122 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5922 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 6 "kappaParser.mly"
       (Tools.pos)
# 5931 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LABEL _v ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv22)

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv19 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | INIT _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | LABEL _v ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | LET _v ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | NEWLINE ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | OBS _v ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | PERT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | PLOT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | REF _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | SIGNATURE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | ID _ | KAPPA_NOPOLY _ | KAPPA_RAR | OP_PAR ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv20)

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5980 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 5989 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LABEL _v ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv18)

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 6003 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 10 "kappaParser.mly"
       (string*Tools.pos)
# 6013 "kappaParser.ml"
    )) = _v in
    ((let _v : 'tv_rule_label = 
# 153 "kappaParser.mly"
 (let lab,pos = _1 in {Ast.lbl_nme=Some (lab,pos) ; Ast.lbl_ref = None})
# 6018 "kappaParser.ml"
     in
    _menhir_goto_rule_label _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kappaParser.mly"
       (Tools.pos)
# 6025 "kappaParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 6034 "kappaParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state * (
# 7 "kappaParser.mly"
       (Tools.pos)
# 6043 "kappaParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 6048 "kappaParser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_1 : (
# 9 "kappaParser.mly"
       (int*Tools.pos)
# 6056 "kappaParser.ml"
        )) = _v in
        ((let _v : 'tv_multiple = 
# 146 "kappaParser.mly"
      (let int,_=_1 in int)
# 6061 "kappaParser.ml"
         in
        _menhir_goto_multiple _menhir_env _menhir_stack _v) : 'freshtv8)) : 'freshtv10)
    | ID _ | OP_PAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        ((let _v : 'tv_multiple = 
# 145 "kappaParser.mly"
          (1)
# 6070 "kappaParser.ml"
         in
        _menhir_goto_multiple _menhir_env _menhir_stack _v) : 'freshtv12)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_error132 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv14)

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_newline = 
# 33 "kappaParser.mly"
 (())
# 6086 "kappaParser.ml"
     in
    _menhir_goto_newline _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and start_rule : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 25 "kappaParser.mly"
      (unit)
# 6093 "kappaParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = 4611686018427387903;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INIT _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LABEL _v ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET _v ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | NEWLINE ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OBS _v ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | PERT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | PLOT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | REF _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SIGNATURE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ID _ | KAPPA_NOPOLY _ | KAPPA_RAR | OP_PAR ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2)) : 'freshtv4))



