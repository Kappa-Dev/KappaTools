type token =
  | EOF
  | NEWLINE
  | AT
  | OP_PAR
  | CL_PAR
  | COMMA
  | DOT
  | KAPPA_RAR
  | KAPPA_LNK
  | PIPE
  | LOG of (Tools.pos)
  | PLUS of (Tools.pos)
  | MULT of (Tools.pos)
  | MINUS of (Tools.pos)
  | AND of (Tools.pos)
  | OR of (Tools.pos)
  | GREATER of (Tools.pos)
  | SMALLER of (Tools.pos)
  | EQUAL of (Tools.pos)
  | NOT of (Tools.pos)
  | PERT of (Tools.pos)
  | INTRO of (Tools.pos)
  | DELETE of (Tools.pos)
  | SET of (Tools.pos)
  | DO of (Tools.pos)
  | UNTIL of (Tools.pos)
  | TRUE of (Tools.pos)
  | FALSE of (Tools.pos)
  | SNAPSHOT of (Tools.pos)
  | REF of (Tools.pos)
  | OBS of (Tools.pos)
  | KAPPA_WLD of (Tools.pos)
  | KAPPA_SEMI of (Tools.pos)
  | SIGNATURE of (Tools.pos)
  | INFINITY of (Tools.pos)
  | TIME of (Tools.pos)
  | EVENT of (Tools.pos)
  | INIT of (Tools.pos)
  | LET of (Tools.pos)
  | DIV of (Tools.pos)
  | PLOT of (Tools.pos)
  | SINUS of (Tools.pos)
  | COSINUS of (Tools.pos)
  | TAN of (Tools.pos)
  | SQRT of (Tools.pos)
  | EXPONENT of (Tools.pos)
  | POW of (Tools.pos)
  | ABS of (Tools.pos)
  | MODULO of (Tools.pos)
  | STOP of (Tools.pos)
  | KAPPA_NOPOLY of (Tools.pos)
  | EMAX of (Tools.pos)
  | TMAX of (Tools.pos)
  | INT of (int*Tools.pos)
  | ID of (string*Tools.pos)
  | LABEL of (string*Tools.pos)
  | KAPPA_MRK of (string*Tools.pos)
  | DOT_RADIUS of (int)
  | PLUS_RADIUS of (int)
  | FLOAT of (float*Tools.pos)

open Parsing;;
# 2 "grammar/kappaParser.mly"
# 66 "grammar/kappaParser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* NEWLINE *);
  258 (* AT *);
  259 (* OP_PAR *);
  260 (* CL_PAR *);
  261 (* COMMA *);
  262 (* DOT *);
  263 (* KAPPA_RAR *);
  264 (* KAPPA_LNK *);
  265 (* PIPE *);
    0|]

let yytransl_block = [|
  266 (* LOG *);
  267 (* PLUS *);
  268 (* MULT *);
  269 (* MINUS *);
  270 (* AND *);
  271 (* OR *);
  272 (* GREATER *);
  273 (* SMALLER *);
  274 (* EQUAL *);
  275 (* NOT *);
  276 (* PERT *);
  277 (* INTRO *);
  278 (* DELETE *);
  279 (* SET *);
  280 (* DO *);
  281 (* UNTIL *);
  282 (* TRUE *);
  283 (* FALSE *);
  284 (* SNAPSHOT *);
  285 (* REF *);
  286 (* OBS *);
  287 (* KAPPA_WLD *);
  288 (* KAPPA_SEMI *);
  289 (* SIGNATURE *);
  290 (* INFINITY *);
  291 (* TIME *);
  292 (* EVENT *);
  293 (* INIT *);
  294 (* LET *);
  295 (* DIV *);
  296 (* PLOT *);
  297 (* SINUS *);
  298 (* COSINUS *);
  299 (* TAN *);
  300 (* SQRT *);
  301 (* EXPONENT *);
  302 (* POW *);
  303 (* ABS *);
  304 (* MODULO *);
  305 (* STOP *);
  306 (* KAPPA_NOPOLY *);
  307 (* EMAX *);
  308 (* TMAX *);
  309 (* INT *);
  310 (* ID *);
  311 (* LABEL *);
  312 (* KAPPA_MRK *);
  313 (* DOT_RADIUS *);
  314 (* PLUS_RADIUS *);
  315 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\001\000\001\000\001\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\008\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\006\000\006\000\013\000\013\000\013\000\
\014\000\014\000\003\000\003\000\015\000\015\000\017\000\017\000\
\017\000\017\000\017\000\018\000\018\000\018\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\016\000\016\000\
\012\000\012\000\007\000\007\000\007\000\005\000\005\000\019\000\
\019\000\020\000\020\000\021\000\022\000\022\000\022\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\002\000\001\000\002\000\002\000\
\003\000\002\000\002\000\002\000\002\000\002\000\004\000\006\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\001\000\001\000\003\000\002\000\002\000\002\000\
\003\000\001\000\001\000\000\000\001\000\000\000\001\000\005\000\
\000\000\001\000\006\000\004\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\001\000\
\002\000\001\000\003\000\003\000\001\000\004\000\001\000\000\000\
\001\000\003\000\001\000\003\000\000\000\002\000\001\000\000\000\
\002\000\002\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\094\000\003\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\027\000\028\000\
\047\000\053\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\051\000\049\000\052\000\048\000\000\000\000\000\
\056\000\057\000\000\000\000\000\012\000\008\000\000\000\007\000\
\010\000\037\000\000\000\011\000\014\000\000\000\000\000\004\000\
\005\000\000\000\000\000\042\000\000\000\000\000\000\000\070\000\
\026\000\065\000\066\000\067\000\069\000\064\000\068\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\017\000\000\000\
\000\000\009\000\000\000\000\000\000\000\045\000\046\000\000\000\
\055\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\034\000\035\000\000\000\000\000\000\000\000\000\000\000\081\000\
\000\000\075\000\076\000\000\000\000\000\031\000\074\000\000\000\
\030\000\032\000\000\000\000\000\040\000\087\000\000\000\000\000\
\078\000\000\000\000\000\029\000\073\000\000\000\000\000\086\000\
\000\000\092\000\084\000\082\000\000\000\043\000\093\000\090\000\
\089\000\000\000\000\000\000\000\000\000\091\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\059\000\051\000\060\000\045\000\
\039\000\040\000\116\000\129\000\018\000\061\000\096\000\150\000\
\041\000\042\000\119\000\120\000\121\000\136\000\147\000"

let yysindex = "\024\000\
\001\000\000\000\000\000\000\000\001\000\193\000\204\254\234\254\
\010\255\014\255\234\254\192\255\000\000\000\000\000\000\004\000\
\004\000\019\255\000\000\193\000\247\000\193\000\000\000\000\000\
\000\000\000\000\000\000\247\000\247\000\247\000\247\000\247\000\
\247\000\000\000\000\000\000\000\000\000\000\000\040\001\096\255\
\000\000\000\000\034\255\113\255\000\000\000\000\051\255\000\000\
\000\000\000\000\019\255\000\000\000\000\247\000\207\255\000\000\
\000\000\019\255\055\255\000\000\013\255\091\255\037\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\247\000\
\247\000\247\000\247\000\247\000\247\000\247\000\247\000\247\000\
\193\000\193\000\027\255\022\255\000\000\220\000\000\000\207\255\
\011\255\000\000\036\001\065\255\019\255\000\000\000\000\019\255\
\000\000\000\000\066\255\248\254\066\255\207\255\207\255\207\255\
\248\254\053\255\053\255\000\000\101\255\027\255\170\255\220\000\
\000\000\000\000\094\255\093\255\115\255\000\255\117\255\000\000\
\122\255\000\000\000\000\120\255\124\255\000\000\000\000\230\000\
\000\000\000\000\247\000\193\000\000\000\000\000\000\255\005\255\
\000\000\011\255\247\000\000\000\000\000\207\255\047\255\000\000\
\018\255\000\000\000\000\000\000\150\255\000\000\000\000\000\000\
\000\000\123\255\247\000\078\255\207\255\000\000"

let yyrindex = "\000\000\
\020\255\000\000\000\000\000\000\020\255\000\000\000\000\000\000\
\000\000\029\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\175\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\182\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\129\255\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\124\000\075\000\150\000\039\000\184\000\191\000\
\106\000\011\000\057\000\000\000\079\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\244\255\000\000\000\000\
\130\255\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\255\024\255\
\000\000\000\000\000\000\000\000\000\000\007\000\012\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000"

let yygindex = "\000\000\
\130\000\081\000\000\000\000\000\127\000\000\000\229\255\129\000\
\014\000\255\255\028\000\029\000\000\000\046\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\017\000\000\000"

let yytablesize = 600
let yytable = "\134\000\
\004\000\041\000\043\000\004\000\013\000\018\000\033\000\015\000\
\044\000\046\000\062\000\016\000\145\000\049\000\072\000\071\000\
\087\000\151\000\063\000\094\000\065\000\058\000\038\000\090\000\
\001\000\055\000\038\000\088\000\088\000\110\000\092\000\036\000\
\044\000\062\000\064\000\146\000\084\000\079\000\023\000\080\000\
\098\000\066\000\067\000\068\000\069\000\070\000\071\000\111\000\
\112\000\152\000\081\000\082\000\041\000\089\000\113\000\135\000\
\063\000\088\000\092\000\093\000\081\000\082\000\095\000\047\000\
\118\000\123\000\050\000\091\000\122\000\038\000\153\000\154\000\
\047\000\038\000\058\000\114\000\117\000\073\000\022\000\108\000\
\109\000\115\000\036\000\127\000\127\000\099\000\100\000\101\000\
\102\000\103\000\104\000\105\000\106\000\107\000\097\000\041\000\
\056\000\057\000\079\000\091\000\141\000\072\000\073\000\074\000\
\078\000\060\000\075\000\076\000\077\000\081\000\082\000\079\000\
\085\000\080\000\081\000\086\000\131\000\132\000\133\000\083\000\
\137\000\139\000\021\000\059\000\128\000\128\000\138\000\140\000\
\156\000\078\000\143\000\158\000\080\000\083\000\019\000\048\000\
\079\000\125\000\080\000\052\000\130\000\124\000\000\000\000\000\
\142\000\000\000\025\000\026\000\027\000\061\000\148\000\144\000\
\149\000\028\000\029\000\030\000\031\000\032\000\155\000\033\000\
\072\000\073\000\074\000\034\000\035\000\036\000\047\000\037\000\
\157\000\126\000\000\000\038\000\086\000\000\000\079\000\000\000\
\000\000\000\000\000\000\021\000\000\000\077\000\000\000\024\000\
\000\000\000\000\000\000\000\000\078\000\000\000\025\000\053\000\
\000\000\000\000\054\000\079\000\000\000\080\000\000\000\000\000\
\000\000\021\000\000\000\025\000\026\000\027\000\000\000\000\000\
\000\000\000\000\028\000\029\000\030\000\031\000\032\000\000\000\
\033\000\072\000\073\000\074\000\034\000\035\000\036\000\047\000\
\037\000\025\000\026\000\027\000\038\000\000\000\000\000\000\000\
\028\000\029\000\030\000\031\000\032\000\000\000\033\000\000\000\
\000\000\000\000\034\000\035\000\036\000\078\000\037\000\085\000\
\085\000\000\000\038\000\085\000\079\000\000\000\080\000\000\000\
\003\000\005\000\041\000\041\000\005\000\013\000\018\000\033\000\
\015\000\044\000\033\000\062\000\016\000\062\000\062\000\072\000\
\071\000\000\000\085\000\062\000\006\000\062\000\062\000\062\000\
\062\000\062\000\062\000\062\000\062\000\007\000\008\000\033\000\
\000\000\009\000\062\000\062\000\000\000\010\000\011\000\023\000\
\012\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\062\000\000\000\000\000\023\000\023\000\000\000\013\000\
\000\000\063\000\062\000\063\000\063\000\000\000\023\000\000\000\
\062\000\063\000\000\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\058\000\000\000\058\000\058\000\022\000\
\063\000\063\000\022\000\058\000\000\000\058\000\058\000\058\000\
\058\000\058\000\058\000\058\000\058\000\022\000\000\000\063\000\
\000\000\000\000\058\000\058\000\000\000\000\000\022\000\000\000\
\063\000\000\000\060\000\000\000\060\000\060\000\063\000\000\000\
\000\000\058\000\060\000\000\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\059\000\000\000\059\000\059\000\
\058\000\060\000\060\000\000\000\059\000\000\000\059\000\000\000\
\059\000\059\000\059\000\059\000\059\000\059\000\000\000\000\000\
\060\000\000\000\000\000\059\000\059\000\000\000\061\000\000\000\
\061\000\061\000\000\000\000\000\000\000\000\000\061\000\060\000\
\061\000\000\000\061\000\061\000\061\000\061\000\061\000\061\000\
\000\000\000\000\000\000\000\000\000\000\061\000\061\000\079\000\
\079\000\059\000\079\000\079\000\000\000\079\000\077\000\077\000\
\024\000\077\000\000\000\024\000\077\000\000\000\000\000\025\000\
\000\000\000\000\025\000\020\000\000\000\024\000\024\000\079\000\
\000\000\000\000\021\000\061\000\025\000\025\000\077\000\024\000\
\000\000\000\000\000\000\022\000\000\000\000\000\025\000\000\000\
\000\000\000\000\023\000\024\000\000\000\000\000\086\000\000\000\
\079\000\000\000\025\000\026\000\027\000\021\000\000\000\077\000\
\058\000\028\000\029\000\030\000\031\000\032\000\000\000\033\000\
\072\000\073\000\074\000\034\000\035\000\036\000\000\000\037\000\
\000\000\054\000\000\000\038\000\000\000\025\000\026\000\027\000\
\021\000\000\000\000\000\000\000\028\000\029\000\030\000\031\000\
\032\000\000\000\033\000\000\000\078\000\000\000\034\000\035\000\
\036\000\047\000\037\000\079\000\000\000\080\000\038\000\000\000\
\025\000\026\000\027\000\047\000\000\000\000\000\000\000\028\000\
\029\000\030\000\031\000\032\000\000\000\033\000\000\000\097\000\
\000\000\034\000\035\000\036\000\000\000\037\000\072\000\073\000\
\074\000\038\000\072\000\073\000\074\000\000\000\000\000\075\000\
\076\000\077\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\078\000\000\000\000\000\000\000\078\000\000\000\
\000\000\079\000\000\000\080\000\000\000\079\000\000\000\080\000"

let yycheck = "\000\001\
\000\000\000\000\055\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\001\000\000\000\000\008\001\000\001\000\000\000\000\
\044\000\000\001\020\000\007\001\022\000\003\001\003\001\051\000\
\001\000\012\000\007\001\004\001\005\001\003\001\058\000\003\001\
\055\001\020\000\021\000\031\001\003\001\046\001\000\000\048\001\
\004\001\028\000\029\000\030\000\031\000\032\000\033\000\021\001\
\022\001\032\001\014\001\015\001\007\001\003\001\028\001\056\001\
\000\000\044\000\086\000\005\001\014\001\015\001\050\001\054\001\
\054\001\093\000\053\001\054\000\004\001\050\001\053\001\054\001\
\054\001\054\001\000\000\049\001\055\001\012\001\000\000\081\000\
\082\000\055\001\054\001\111\000\112\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\004\001\050\001\
\016\000\017\000\046\001\086\000\128\000\011\001\012\001\013\001\
\039\001\000\000\016\001\017\001\018\001\014\001\015\001\046\001\
\000\001\048\001\014\001\003\001\023\001\025\001\004\001\024\001\
\004\001\002\001\010\001\000\000\111\000\112\000\005\001\004\001\
\006\001\039\001\132\000\054\001\004\001\004\001\005\000\009\000\
\046\001\110\000\048\001\011\000\112\000\096\000\255\255\255\255\
\131\000\255\255\034\001\035\001\036\001\000\000\138\000\135\000\
\139\000\041\001\042\001\043\001\044\001\045\001\009\001\047\001\
\011\001\012\001\013\001\051\001\052\001\053\001\054\001\055\001\
\155\000\000\001\255\255\059\001\003\001\255\255\000\000\255\255\
\255\255\255\255\255\255\010\001\255\255\000\000\255\255\000\000\
\255\255\255\255\255\255\255\255\039\001\255\255\000\000\000\001\
\255\255\255\255\003\001\046\001\255\255\048\001\255\255\255\255\
\255\255\010\001\255\255\034\001\035\001\036\001\255\255\255\255\
\255\255\255\255\041\001\042\001\043\001\044\001\045\001\255\255\
\047\001\011\001\012\001\013\001\051\001\052\001\053\001\054\001\
\055\001\034\001\035\001\036\001\059\001\255\255\255\255\255\255\
\041\001\042\001\043\001\044\001\045\001\255\255\047\001\255\255\
\255\255\255\255\051\001\052\001\053\001\039\001\055\001\004\001\
\005\001\255\255\059\001\008\001\046\001\255\255\048\001\255\255\
\000\001\001\001\001\001\002\001\001\001\001\001\001\001\001\001\
\001\001\001\001\004\001\001\001\001\001\003\001\004\001\001\001\
\001\001\255\255\031\001\009\001\020\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\029\001\030\001\025\001\
\255\255\033\001\024\001\025\001\255\255\037\001\038\001\001\001\
\040\001\255\255\004\001\255\255\255\255\255\255\255\255\255\255\
\255\255\039\001\255\255\255\255\014\001\015\001\255\255\055\001\
\255\255\001\001\048\001\003\001\004\001\255\255\024\001\255\255\
\054\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\255\255\003\001\004\001\001\001\
\024\001\025\001\004\001\009\001\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\015\001\255\255\039\001\
\255\255\255\255\024\001\025\001\255\255\255\255\024\001\255\255\
\048\001\255\255\001\001\255\255\003\001\004\001\054\001\255\255\
\255\255\039\001\009\001\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\001\001\255\255\003\001\004\001\
\054\001\024\001\025\001\255\255\009\001\255\255\011\001\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\039\001\255\255\255\255\024\001\025\001\255\255\001\001\255\255\
\003\001\004\001\255\255\255\255\255\255\255\255\009\001\054\001\
\011\001\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\025\001\001\001\
\002\001\054\001\004\001\005\001\255\255\007\001\001\001\002\001\
\001\001\004\001\255\255\004\001\007\001\255\255\255\255\001\001\
\255\255\255\255\004\001\003\001\255\255\014\001\015\001\025\001\
\255\255\255\255\010\001\054\001\014\001\015\001\025\001\024\001\
\255\255\255\255\255\255\019\001\255\255\255\255\024\001\255\255\
\255\255\255\255\026\001\027\001\255\255\255\255\003\001\255\255\
\050\001\255\255\034\001\035\001\036\001\010\001\255\255\050\001\
\003\001\041\001\042\001\043\001\044\001\045\001\255\255\047\001\
\011\001\012\001\013\001\051\001\052\001\053\001\255\255\055\001\
\255\255\003\001\255\255\059\001\255\255\034\001\035\001\036\001\
\010\001\255\255\255\255\255\255\041\001\042\001\043\001\044\001\
\045\001\255\255\047\001\255\255\039\001\255\255\051\001\052\001\
\053\001\054\001\055\001\046\001\255\255\048\001\059\001\255\255\
\034\001\035\001\036\001\054\001\255\255\255\255\255\255\041\001\
\042\001\043\001\044\001\045\001\255\255\047\001\255\255\004\001\
\255\255\051\001\052\001\053\001\255\255\055\001\011\001\012\001\
\013\001\059\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\039\001\255\255\255\255\255\255\039\001\255\255\
\255\255\046\001\255\255\048\001\255\255\046\001\255\255\048\001"

let yynames_const = "\
  EOF\000\
  NEWLINE\000\
  AT\000\
  OP_PAR\000\
  CL_PAR\000\
  COMMA\000\
  DOT\000\
  KAPPA_RAR\000\
  KAPPA_LNK\000\
  PIPE\000\
  "

let yynames_block = "\
  LOG\000\
  PLUS\000\
  MULT\000\
  MINUS\000\
  AND\000\
  OR\000\
  GREATER\000\
  SMALLER\000\
  EQUAL\000\
  NOT\000\
  PERT\000\
  INTRO\000\
  DELETE\000\
  SET\000\
  DO\000\
  UNTIL\000\
  TRUE\000\
  FALSE\000\
  SNAPSHOT\000\
  REF\000\
  OBS\000\
  KAPPA_WLD\000\
  KAPPA_SEMI\000\
  SIGNATURE\000\
  INFINITY\000\
  TIME\000\
  EVENT\000\
  INIT\000\
  LET\000\
  DIV\000\
  PLOT\000\
  SINUS\000\
  COSINUS\000\
  TAN\000\
  SQRT\000\
  EXPONENT\000\
  POW\000\
  ABS\000\
  MODULO\000\
  STOP\000\
  KAPPA_NOPOLY\000\
  EMAX\000\
  TMAX\000\
  INT\000\
  ID\000\
  LABEL\000\
  KAPPA_MRK\000\
  DOT_RADIUS\000\
  PLUS_RADIUS\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 31 "grammar/kappaParser.mly"
 (_2)
# 465 "grammar/kappaParser.ml"
               : 'newline))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "grammar/kappaParser.mly"
 (())
# 471 "grammar/kappaParser.ml"
               : 'newline))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 38 "grammar/kappaParser.mly"
  (_1)
# 478 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 40 "grammar/kappaParser.mly"
 (let rule_label,r = _1 in Ast.result := {!Ast.result with Ast.rules = (rule_label,r)::!Ast.result.Ast.rules} ; _2)
# 486 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 42 "grammar/kappaParser.mly"
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
# 518 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Syntax error"))
# 524 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'agent_expression) in
    Obj.repr(
# 73 "grammar/kappaParser.mly"
 ((Ast.SIG (_2,_1)))
# 532 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    Obj.repr(
# 75 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))
# 539 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'multiple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 77 "grammar/kappaParser.mly"
 (Ast.INIT (_2,_3,_1))
# 548 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    Obj.repr(
# 79 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed initial condition, I was expecting something of the form '%init: n kappa_expression'"))
# 555 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_declaration) in
    Obj.repr(
# 81 "grammar/kappaParser.mly"
 (Ast.DECLARE _2)
# 563 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_declaration) in
    Obj.repr(
# 83 "grammar/kappaParser.mly"
 (Ast.OBS _2)
# 571 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 85 "grammar/kappaParser.mly"
 (Ast.PLOT _2)
# 579 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    Obj.repr(
# 87 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed plot instruction, I was expecting an algebraic expression of variables"))
# 586 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'modif_expr) in
    Obj.repr(
# 89 "grammar/kappaParser.mly"
 (Ast.PERT (_2,_4,_1,None))
# 596 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Tools.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'modif_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 91 "grammar/kappaParser.mly"
 (Ast.PERT (_2,_4,_1,Some _6))
# 608 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 94 "grammar/kappaParser.mly"
                          (Ast.VAR_KAPPA (_2,_1))
# 616 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 95 "grammar/kappaParser.mly"
                 (Ast.VAR_ALG (_2,_1))
# 624 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Tools.pos) in
    Obj.repr(
# 97 "grammar/kappaParser.mly"
 (let str,pos = _1 in
		raise 
		(ExceptionDefn.Syntax_Error 
		(Printf.sprintf "Variable '%s' should be either a pure kappa expression or an algebraic expression on variables" str)
		) 
	)
# 636 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    Obj.repr(
# 107 "grammar/kappaParser.mly"
 (_2)
# 643 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 109 "grammar/kappaParser.mly"
 (Ast.AND (_1,_3,_2))
# 652 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 111 "grammar/kappaParser.mly"
 (Ast.OR (_1,_3,_2))
# 661 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 113 "grammar/kappaParser.mly"
 (Ast.GREATER (_1,_3,_2))
# 670 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 115 "grammar/kappaParser.mly"
 (Ast.SMALLER (_1,_3,_2))
# 679 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 117 "grammar/kappaParser.mly"
 (Ast.EQUAL (_1,_3,_2))
# 688 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 119 "grammar/kappaParser.mly"
 (Ast.NOT (_2,_1))
# 696 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 121 "grammar/kappaParser.mly"
 (Ast.TRUE _1)
# 703 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 123 "grammar/kappaParser.mly"
 (Ast.FALSE _1)
# 710 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'modif_expr) in
    Obj.repr(
# 129 "grammar/kappaParser.mly"
 (_2)
# 717 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'multiple_mixture) in
    Obj.repr(
# 131 "grammar/kappaParser.mly"
 (let (alg,mix) = _2 in Ast.INTRO (alg,mix,_1))
# 725 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    Obj.repr(
# 133 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed perturbation instruction, I was expecting '$(ADD) alg_expression kappa_expression'"))
# 732 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'multiple_mixture) in
    Obj.repr(
# 135 "grammar/kappaParser.mly"
 (let (alg,mix) = _2 in Ast.DELETE (alg,mix,_1))
# 740 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string*Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 137 "grammar/kappaParser.mly"
 (let lab,pos_lab = _1 in Ast.UPDATE (lab,pos_lab,_3,_2))
# 749 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 139 "grammar/kappaParser.mly"
 (Ast.SNAPSHOT _1)
# 756 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 141 "grammar/kappaParser.mly"
 (Ast.STOP _1)
# 763 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "grammar/kappaParser.mly"
          (1)
# 769 "grammar/kappaParser.ml"
               : 'multiple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*Tools.pos) in
    Obj.repr(
# 146 "grammar/kappaParser.mly"
      (let int,_=_1 in int)
# 776 "grammar/kappaParser.ml"
               : 'multiple))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "grammar/kappaParser.mly"
 ({Ast.lbl_nme = None ; Ast.lbl_ref = None})
# 782 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Tools.pos) in
    Obj.repr(
# 153 "grammar/kappaParser.mly"
 (let lab,pos = _1 in {Ast.lbl_nme=Some (lab,pos) ; Ast.lbl_ref = None})
# 789 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string*Tools.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string*Tools.pos) in
    Obj.repr(
# 155 "grammar/kappaParser.mly"
 (let ref,pos = _2 and lab,pos' = _4 in {Ast.lbl_nme=Some (lab,pos') ; Ast.lbl_ref = Some (ref,pos)})
# 798 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "grammar/kappaParser.mly"
 (Ast.EMPTY_MIX)
# 804 "grammar/kappaParser.ml"
               : 'mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 162 "grammar/kappaParser.mly"
 (_1)
# 811 "grammar/kappaParser.ml"
               : 'mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rule_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'mixture) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mixture) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'rate) in
    Obj.repr(
# 167 "grammar/kappaParser.mly"
 (let (k2,k1) = _6 in 
		(_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=k2; Ast.k_un=k1})
	)
# 824 "grammar/kappaParser.ml"
               : 'rule_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rule_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'mixture) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'mixture) in
    Obj.repr(
# 171 "grammar/kappaParser.mly"
 ((_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=(Ast.FLOAT (1.0,Tools.no_pos)); Ast.k_un=None}))
# 834 "grammar/kappaParser.ml"
               : 'rule_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "grammar/kappaParser.mly"
 (Ast.RAR)
# 840 "grammar/kappaParser.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 178 "grammar/kappaParser.mly"
 (Ast.RAR_NOPOLY _1)
# 847 "grammar/kappaParser.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 183 "grammar/kappaParser.mly"
 (Ast.INFINITY _1)
# 854 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float*Tools.pos) in
    Obj.repr(
# 185 "grammar/kappaParser.mly"
 (let f,pos = _1 in Ast.FLOAT (f,pos))
# 861 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*Tools.pos) in
    Obj.repr(
# 187 "grammar/kappaParser.mly"
 (let i,pos = _1 in Ast.FLOAT (float_of_int i,pos))
# 868 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 189 "grammar/kappaParser.mly"
 (let pos = _1 in Ast.EMAX pos)
# 875 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 191 "grammar/kappaParser.mly"
 (let pos = _1 in Ast.TMAX pos)
# 882 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Tools.pos) in
    Obj.repr(
# 196 "grammar/kappaParser.mly"
 (Ast.OBS_VAR _1)
# 889 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 198 "grammar/kappaParser.mly"
 (Ast.TIME_VAR _1)
# 896 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 200 "grammar/kappaParser.mly"
 (Ast.EVENT_VAR _1)
# 903 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'alg_expr) in
    Obj.repr(
# 205 "grammar/kappaParser.mly"
 (_2)
# 910 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 207 "grammar/kappaParser.mly"
 (_1)
# 917 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 209 "grammar/kappaParser.mly"
 (_1)
# 924 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 211 "grammar/kappaParser.mly"
 (Ast.MULT (_1,_3,_2))
# 933 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 213 "grammar/kappaParser.mly"
 (Ast.SUM (_1,_3,_2))
# 942 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 215 "grammar/kappaParser.mly"
 (Ast.DIV (_1,_3,_2))
# 951 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 217 "grammar/kappaParser.mly"
 (Ast.MINUS (_1,_3,_2))
# 960 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 219 "grammar/kappaParser.mly"
 (Ast.POW (_1,_3,_2))
# 969 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 221 "grammar/kappaParser.mly"
 (Ast.MODULO (_1,_3,_2))
# 978 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 223 "grammar/kappaParser.mly"
 (Ast.EXP (_2,_1))
# 986 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 225 "grammar/kappaParser.mly"
 (Ast.SINUS (_2,_1))
# 994 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 227 "grammar/kappaParser.mly"
 (Ast.COSINUS (_2,_1))
# 1002 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 229 "grammar/kappaParser.mly"
 (Ast.TAN (_2,_1))
# 1010 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 231 "grammar/kappaParser.mly"
 (Ast.ABS (_2,_1))
# 1018 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 233 "grammar/kappaParser.mly"
 (Ast.SQRT (_2,_1))
# 1026 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 235 "grammar/kappaParser.mly"
 (Ast.LOG (_2,_1))
# 1034 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 240 "grammar/kappaParser.mly"
 ((_1,Some _3))
# 1042 "grammar/kappaParser.ml"
               : 'rate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 242 "grammar/kappaParser.mly"
 ((_1,None))
# 1049 "grammar/kappaParser.ml"
               : 'rate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 247 "grammar/kappaParser.mly"
 ((_1,_2))
# 1057 "grammar/kappaParser.ml"
               : 'multiple_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 249 "grammar/kappaParser.mly"
 ((Ast.FLOAT (1.,Tools.no_pos),_1))
# 1064 "grammar/kappaParser.ml"
               : 'multiple_mixture))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'non_empty_mixture) in
    Obj.repr(
# 254 "grammar/kappaParser.mly"
 (_2)
# 1071 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 256 "grammar/kappaParser.mly"
 (Ast.COMMA (_1,_3))
# 1079 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'agent_expression) in
    Obj.repr(
# 258 "grammar/kappaParser.mly"
 (Ast.COMMA(_1,Ast.EMPTY_MIX))
# 1086 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string*Tools.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'interface_expression) in
    Obj.repr(
# 282 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id; Ast.ag_intf=_3; Ast.ag_pos=pos})
# 1094 "grammar/kappaParser.ml"
               : 'agent_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Tools.pos) in
    Obj.repr(
# 284 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id;Ast.ag_intf=Ast.EMPTY_INTF;Ast.ag_pos=pos})
# 1101 "grammar/kappaParser.ml"
               : 'agent_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 289 "grammar/kappaParser.mly"
 (Ast.EMPTY_INTF)
# 1107 "grammar/kappaParser.ml"
               : 'interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ne_interface_expression) in
    Obj.repr(
# 291 "grammar/kappaParser.mly"
 (_1)
# 1114 "grammar/kappaParser.ml"
               : 'interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'port_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ne_interface_expression) in
    Obj.repr(
# 296 "grammar/kappaParser.mly"
 (Ast.PORT_SEP(_1,_3))
# 1122 "grammar/kappaParser.ml"
               : 'ne_interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'port_expression) in
    Obj.repr(
# 298 "grammar/kappaParser.mly"
 (Ast.PORT_SEP(_1,Ast.EMPTY_INTF))
# 1129 "grammar/kappaParser.ml"
               : 'ne_interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string*Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'internal_state) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'link_state) in
    Obj.repr(
# 311 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.port_nme=id; Ast.port_int=_2; Ast.port_lnk=_3; Ast.port_pos=pos})
# 1138 "grammar/kappaParser.ml"
               : 'port_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 315 "grammar/kappaParser.mly"
          ([])
# 1144 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Tools.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'internal_state) in
    Obj.repr(
# 317 "grammar/kappaParser.mly"
 (let m,pos = _1 in m::_2)
# 1152 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 319 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid internal state"))
# 1158 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 324 "grammar/kappaParser.mly"
 (Ast.FREE)
# 1164 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int*Tools.pos) in
    Obj.repr(
# 326 "grammar/kappaParser.mly"
 (Ast.LNK_VALUE _2)
# 1171 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 328 "grammar/kappaParser.mly"
 (Ast.LNK_SOME _2)
# 1178 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string*Tools.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string*Tools.pos) in
    Obj.repr(
# 330 "grammar/kappaParser.mly"
 (Ast.LNK_TYPE (_2,_4))
# 1186 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tools.pos) in
    Obj.repr(
# 332 "grammar/kappaParser.mly"
 (Ast.LNK_ANY _1)
# 1193 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 334 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid link state"))
# 1199 "grammar/kappaParser.ml"
               : 'link_state))
(* Entry start_rule *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start_rule (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
