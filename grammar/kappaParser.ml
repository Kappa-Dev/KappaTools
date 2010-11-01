type token =
  | EOF
  | NEWLINE
  | AT
  | OP_PAR
  | CL_PAR
  | COMMA
  | DOT
  | PLUS of (Misc.position)
  | KAPPA_RAR
  | KAPPA_LRAR
  | KAPPA_LNK
  | ARROW
  | PIPE
  | MULT of (Misc.position)
  | MINUS of (Misc.position)
  | LOG of (Misc.position)
  | AND of (Misc.position)
  | OR of (Misc.position)
  | GREATER of (Misc.position)
  | SMALLER of (Misc.position)
  | EQUAL of (Misc.position)
  | NOT of (Misc.position)
  | PERT of (Misc.position)
  | INTRO of (Misc.position)
  | DELETE of (Misc.position)
  | SET of (Misc.position)
  | DO of (Misc.position)
  | SIGNAL of (Misc.position)
  | UNTIL of (Misc.position)
  | TRUE of (Misc.position)
  | FALSE of (Misc.position)
  | SNAPSHOT of (Misc.position)
  | REF of (Misc.position)
  | OBS of (Misc.position)
  | KAPPA_WLD of (Misc.position)
  | KAPPA_SEMI of (Misc.position)
  | SIGNATURE of (Misc.position)
  | INFINITY of (Misc.position)
  | TIME of (Misc.position)
  | EVENT of (Misc.position)
  | INIT of (Misc.position)
  | LET of (Misc.position)
  | DIV of (Misc.position)
  | PLOT of (Misc.position)
  | SINUS of (Misc.position)
  | COSINUS of (Misc.position)
  | SQRT of (Misc.position)
  | EXPONENT of (Misc.position)
  | POW of (Misc.position)
  | ABS of (Misc.position)
  | MODULO of (Misc.position)
  | STOP of (Misc.position)
  | KAPPA_NOPOLY of (Misc.position)
  | INT of (int*Misc.position)
  | ID of (string*Misc.position)
  | LABEL of (string*Misc.position)
  | KAPPA_MRK of (string*Misc.position)
  | DOT_RADIUS of (int)
  | PLUS_RADIUS of (int)
  | FLOAT of (float*Misc.position)

open Parsing;;
# 1 "grammar/kappaParser.mly"
  

# 68 "grammar/kappaParser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* NEWLINE *);
  258 (* AT *);
  259 (* OP_PAR *);
  260 (* CL_PAR *);
  261 (* COMMA *);
  262 (* DOT *);
  264 (* KAPPA_RAR *);
  265 (* KAPPA_LRAR *);
  266 (* KAPPA_LNK *);
  267 (* ARROW *);
  268 (* PIPE *);
    0|]

let yytransl_block = [|
  263 (* PLUS *);
  269 (* MULT *);
  270 (* MINUS *);
  271 (* LOG *);
  272 (* AND *);
  273 (* OR *);
  274 (* GREATER *);
  275 (* SMALLER *);
  276 (* EQUAL *);
  277 (* NOT *);
  278 (* PERT *);
  279 (* INTRO *);
  280 (* DELETE *);
  281 (* SET *);
  282 (* DO *);
  283 (* SIGNAL *);
  284 (* UNTIL *);
  285 (* TRUE *);
  286 (* FALSE *);
  287 (* SNAPSHOT *);
  288 (* REF *);
  289 (* OBS *);
  290 (* KAPPA_WLD *);
  291 (* KAPPA_SEMI *);
  292 (* SIGNATURE *);
  293 (* INFINITY *);
  294 (* TIME *);
  295 (* EVENT *);
  296 (* INIT *);
  297 (* LET *);
  298 (* DIV *);
  299 (* PLOT *);
  300 (* SINUS *);
  301 (* COSINUS *);
  302 (* SQRT *);
  303 (* EXPONENT *);
  304 (* POW *);
  305 (* ABS *);
  306 (* MODULO *);
  307 (* STOP *);
  308 (* KAPPA_NOPOLY *);
  309 (* INT *);
  310 (* ID *);
  311 (* LABEL *);
  312 (* KAPPA_MRK *);
  313 (* DOT_RADIUS *);
  314 (* PLUS_RADIUS *);
  315 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\008\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\006\000\006\000\013\000\013\000\013\000\
\014\000\014\000\003\000\003\000\015\000\015\000\017\000\017\000\
\017\000\018\000\018\000\018\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\016\000\016\000\012\000\012\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\005\000\005\000\019\000\
\019\000\020\000\020\000\021\000\022\000\022\000\022\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\001\000\002\000\001\000\002\000\002\000\
\003\000\002\000\002\000\002\000\002\000\002\000\004\000\006\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\001\000\001\000\003\000\002\000\002\000\002\000\
\003\000\001\000\001\000\000\000\001\000\000\000\001\000\005\000\
\000\000\001\000\006\000\004\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\002\000\
\002\000\002\000\003\000\001\000\002\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\004\000\001\000\000\000\
\001\000\003\000\001\000\003\000\000\000\002\000\001\000\000\000\
\002\000\002\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\094\000\001\000\000\000\
\000\000\000\000\005\000\000\000\000\000\027\000\028\000\047\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\049\000\
\050\000\048\000\000\000\000\000\054\000\055\000\000\000\000\000\
\012\000\008\000\000\000\007\000\010\000\037\000\000\000\011\000\
\014\000\000\000\000\000\002\000\003\000\000\000\000\000\042\000\
\000\000\000\000\000\000\026\000\063\000\064\000\066\000\062\000\
\065\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\017\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\046\000\000\000\053\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\034\000\035\000\
\000\000\000\000\000\000\000\000\000\000\081\000\000\000\071\000\
\072\000\073\000\075\000\074\000\076\000\000\000\000\000\031\000\
\070\000\000\000\030\000\032\000\000\000\000\000\040\000\087\000\
\000\000\000\000\078\000\000\000\000\000\029\000\069\000\000\000\
\000\000\086\000\000\000\092\000\084\000\082\000\000\000\043\000\
\093\000\090\000\089\000\000\000\000\000\000\000\000\000\091\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\055\000\047\000\056\000\041\000\
\035\000\036\000\114\000\131\000\018\000\057\000\094\000\152\000\
\037\000\038\000\117\000\118\000\119\000\138\000\149\000"

let yysindex = "\036\000\
\011\000\000\000\000\000\000\000\011\000\185\000\211\254\219\254\
\005\255\008\255\219\254\191\255\000\000\000\000\000\000\003\000\
\003\000\009\255\000\000\185\000\185\000\000\000\000\000\000\000\
\000\000\000\000\232\000\232\000\232\000\232\000\232\000\000\000\
\000\000\000\000\019\001\050\255\000\000\000\000\019\255\143\255\
\000\000\000\000\040\255\000\000\000\000\000\000\009\255\000\000\
\000\000\232\000\242\000\000\000\000\000\009\255\026\255\000\000\
\250\254\206\255\095\255\000\000\000\000\000\000\000\000\000\000\
\000\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
\232\000\232\000\185\000\185\000\074\255\016\255\000\000\213\000\
\000\000\242\000\020\255\000\000\031\255\082\255\009\255\009\255\
\009\255\009\255\009\255\000\000\000\000\009\255\000\000\000\000\
\109\255\234\254\109\255\242\000\242\000\242\000\234\254\030\255\
\030\255\000\000\085\255\074\255\162\255\213\000\000\000\000\000\
\079\255\078\255\116\255\000\255\117\255\000\000\118\255\000\000\
\000\000\000\000\000\000\000\000\000\000\124\255\123\255\000\000\
\000\000\100\255\000\000\000\000\232\000\185\000\000\000\000\000\
\000\255\006\255\000\000\020\255\232\000\000\000\000\000\242\000\
\246\254\000\000\004\255\000\000\000\000\000\000\127\255\000\000\
\000\000\000\000\000\000\122\255\232\000\076\255\242\000\000\000"

let yyrindex = "\000\000\
\033\255\000\000\000\000\000\000\033\255\000\000\000\000\000\000\
\000\000\011\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\129\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000\
\173\000\124\000\193\000\019\000\023\000\100\000\144\000\060\000\
\080\000\000\000\178\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\085\000\000\000\000\000\131\255\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\000\043\255\000\000\000\000\000\000\000\000\000\000\009\000\
\027\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000"

let yygindex = "\000\000\
\131\000\053\000\000\000\000\000\136\000\000\000\028\000\138\000\
\022\000\034\000\039\000\042\000\000\000\059\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\033\000\000\000"

let yytablesize = 581
let yytable = "\136\000\
\079\000\092\000\004\000\153\000\042\000\075\000\076\000\045\000\
\033\000\039\000\004\000\054\000\077\000\036\000\013\000\147\000\
\041\000\040\000\023\000\041\000\018\000\078\000\024\000\015\000\
\044\000\073\000\016\000\074\000\068\000\067\000\087\000\088\000\
\089\000\051\000\095\000\038\000\001\000\066\000\154\000\148\000\
\038\000\058\000\083\000\067\000\068\000\093\000\088\000\088\000\
\061\000\062\000\063\000\064\000\065\000\059\000\060\000\137\000\
\155\000\156\000\043\000\060\000\046\000\082\000\043\000\041\000\
\036\000\075\000\076\000\081\000\052\000\053\000\115\000\085\000\
\072\000\116\000\084\000\077\000\108\000\073\000\073\000\061\000\
\074\000\086\000\090\000\091\000\038\000\120\000\038\000\097\000\
\098\000\099\000\100\000\101\000\102\000\103\000\104\000\105\000\
\109\000\110\000\096\000\025\000\075\000\085\000\054\000\133\000\
\111\000\134\000\066\000\086\000\106\000\107\000\075\000\076\000\
\067\000\068\000\121\000\122\000\123\000\124\000\125\000\135\000\
\139\000\067\000\140\000\056\000\112\000\141\000\142\000\158\000\
\113\000\160\000\130\000\130\000\080\000\066\000\083\000\019\000\
\129\000\129\000\157\000\067\000\068\000\072\000\079\000\058\000\
\044\000\080\000\127\000\073\000\048\000\074\000\072\000\132\000\
\126\000\043\000\144\000\150\000\073\000\143\000\074\000\000\000\
\000\000\128\000\151\000\000\000\080\000\000\000\000\000\145\000\
\072\000\146\000\000\000\000\000\057\000\000\000\073\000\000\000\
\074\000\022\000\159\000\024\000\025\000\026\000\000\000\000\000\
\000\000\000\000\027\000\028\000\029\000\030\000\049\000\031\000\
\059\000\050\000\000\000\032\000\043\000\033\000\024\000\025\000\
\026\000\034\000\000\000\000\000\000\000\027\000\028\000\029\000\
\030\000\095\000\031\000\000\000\066\000\000\000\032\000\043\000\
\033\000\000\000\067\000\068\000\034\000\000\000\000\000\069\000\
\070\000\071\000\000\000\024\000\025\000\026\000\000\000\000\000\
\000\000\000\000\027\000\028\000\029\000\030\000\000\000\031\000\
\000\000\000\000\000\000\032\000\000\000\033\000\000\000\072\000\
\000\000\034\000\000\000\000\000\000\000\073\000\000\000\074\000\
\000\000\079\000\079\000\005\000\079\000\079\000\079\000\079\000\
\079\000\033\000\003\000\005\000\033\000\077\000\077\000\013\000\
\077\000\041\000\041\000\023\000\077\000\018\000\023\000\024\000\
\015\000\044\000\024\000\016\000\079\000\068\000\067\000\000\000\
\006\000\000\000\023\000\023\000\033\000\000\000\024\000\024\000\
\077\000\000\000\007\000\008\000\023\000\000\000\009\000\000\000\
\024\000\000\000\010\000\011\000\079\000\012\000\000\000\000\000\
\000\000\079\000\079\000\000\000\060\000\000\000\060\000\060\000\
\077\000\013\000\060\000\000\000\000\000\000\000\000\000\060\000\
\060\000\060\000\000\000\060\000\060\000\060\000\060\000\060\000\
\061\000\000\000\061\000\061\000\000\000\060\000\061\000\060\000\
\085\000\085\000\000\000\061\000\061\000\061\000\085\000\061\000\
\061\000\061\000\061\000\061\000\025\000\060\000\000\000\025\000\
\000\000\061\000\000\000\061\000\000\000\060\000\000\000\000\000\
\000\000\060\000\000\000\025\000\025\000\000\000\085\000\000\000\
\000\000\061\000\000\000\000\000\056\000\025\000\056\000\056\000\
\000\000\061\000\056\000\000\000\000\000\061\000\000\000\056\000\
\056\000\056\000\000\000\056\000\056\000\056\000\056\000\056\000\
\058\000\000\000\058\000\058\000\000\000\056\000\058\000\056\000\
\000\000\000\000\000\000\058\000\058\000\058\000\000\000\058\000\
\058\000\058\000\058\000\058\000\000\000\056\000\000\000\000\000\
\000\000\058\000\000\000\058\000\000\000\057\000\000\000\057\000\
\057\000\056\000\022\000\057\000\000\000\022\000\000\000\000\000\
\057\000\058\000\057\000\020\000\057\000\057\000\057\000\057\000\
\057\000\059\000\022\000\059\000\059\000\058\000\057\000\059\000\
\057\000\000\000\000\000\022\000\059\000\021\000\059\000\000\000\
\059\000\059\000\059\000\059\000\059\000\022\000\023\000\080\000\
\000\000\000\000\059\000\000\000\059\000\024\000\025\000\026\000\
\000\000\000\000\057\000\000\000\027\000\028\000\029\000\030\000\
\000\000\031\000\050\000\000\000\000\000\032\000\000\000\033\000\
\000\000\000\000\000\000\034\000\000\000\000\000\059\000\000\000\
\066\000\024\000\025\000\026\000\000\000\000\000\067\000\068\000\
\027\000\028\000\029\000\030\000\000\000\031\000\000\000\000\000\
\000\000\032\000\043\000\033\000\024\000\025\000\026\000\034\000\
\000\000\000\000\000\000\027\000\028\000\029\000\030\000\000\000\
\031\000\066\000\000\000\072\000\032\000\000\000\033\000\067\000\
\068\000\073\000\034\000\074\000\069\000\070\000\071\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\000\000\000\000\000\000\000\
\000\000\000\000\073\000\000\000\074\000"

let yycheck = "\000\001\
\000\000\008\001\000\000\000\001\000\001\016\001\017\001\000\001\
\000\000\055\001\000\000\003\001\000\000\003\001\000\000\010\001\
\000\000\055\001\000\000\008\001\000\000\003\001\000\000\000\000\
\000\000\048\001\000\000\050\001\000\000\000\000\005\001\006\001\
\007\001\012\000\004\001\003\001\001\000\007\001\035\001\034\001\
\008\001\020\000\003\001\013\001\014\001\052\001\004\001\005\001\
\027\000\028\000\029\000\030\000\031\000\020\000\021\000\056\001\
\053\001\054\001\054\001\000\000\053\001\040\000\054\001\052\001\
\054\001\016\001\017\001\040\000\016\000\017\000\055\001\050\000\
\042\001\054\001\047\000\026\001\003\001\048\001\048\001\000\000\
\050\001\054\000\057\001\058\001\052\001\004\001\054\001\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\023\001\024\001\004\001\000\000\016\001\080\000\003\001\025\001\
\031\001\028\001\007\001\080\000\075\000\076\000\016\001\017\001\
\013\001\014\001\087\000\088\000\089\000\090\000\091\000\004\001\
\004\001\013\001\005\001\000\000\051\001\002\001\004\001\006\001\
\055\001\054\001\109\000\110\000\004\001\007\001\004\001\005\000\
\109\000\110\000\012\001\013\001\014\001\042\001\000\001\000\000\
\009\000\003\001\108\000\048\001\011\000\050\001\042\001\110\000\
\094\000\054\001\133\000\140\000\048\001\130\000\050\001\255\255\
\255\255\000\001\141\000\255\255\003\001\255\255\255\255\134\000\
\042\001\137\000\255\255\255\255\000\000\255\255\048\001\255\255\
\050\001\000\000\157\000\037\001\038\001\039\001\255\255\255\255\
\255\255\255\255\044\001\045\001\046\001\047\001\000\001\049\001\
\000\000\003\001\255\255\053\001\054\001\055\001\037\001\038\001\
\039\001\059\001\255\255\255\255\255\255\044\001\045\001\046\001\
\047\001\004\001\049\001\255\255\007\001\255\255\053\001\054\001\
\055\001\255\255\013\001\014\001\059\001\255\255\255\255\018\001\
\019\001\020\001\255\255\037\001\038\001\039\001\255\255\255\255\
\255\255\255\255\044\001\045\001\046\001\047\001\255\255\049\001\
\255\255\255\255\255\255\053\001\255\255\055\001\255\255\042\001\
\255\255\059\001\255\255\255\255\255\255\048\001\255\255\050\001\
\255\255\001\001\002\001\001\001\004\001\005\001\006\001\007\001\
\008\001\001\001\000\001\001\001\004\001\001\001\002\001\001\001\
\004\001\001\001\002\001\001\001\008\001\001\001\004\001\001\001\
\001\001\001\001\004\001\001\001\028\001\001\001\001\001\255\255\
\022\001\255\255\016\001\017\001\028\001\255\255\016\001\017\001\
\028\001\255\255\032\001\033\001\026\001\255\255\036\001\255\255\
\026\001\255\255\040\001\041\001\052\001\043\001\255\255\255\255\
\255\255\057\001\058\001\255\255\001\001\255\255\003\001\004\001\
\052\001\055\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\255\255\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\004\001\255\255\026\001\007\001\028\001\
\004\001\005\001\255\255\012\001\013\001\014\001\010\001\016\001\
\017\001\018\001\019\001\020\001\001\001\042\001\255\255\004\001\
\255\255\026\001\255\255\028\001\255\255\050\001\255\255\255\255\
\255\255\054\001\255\255\016\001\017\001\255\255\034\001\255\255\
\255\255\042\001\255\255\255\255\001\001\026\001\003\001\004\001\
\255\255\050\001\007\001\255\255\255\255\054\001\255\255\012\001\
\013\001\014\001\255\255\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\004\001\255\255\026\001\007\001\028\001\
\255\255\255\255\255\255\012\001\013\001\014\001\255\255\016\001\
\017\001\018\001\019\001\020\001\255\255\042\001\255\255\255\255\
\255\255\026\001\255\255\028\001\255\255\001\001\255\255\003\001\
\004\001\054\001\001\001\007\001\255\255\004\001\255\255\255\255\
\012\001\042\001\014\001\003\001\016\001\017\001\018\001\019\001\
\020\001\001\001\017\001\003\001\004\001\054\001\026\001\007\001\
\028\001\255\255\255\255\026\001\012\001\021\001\014\001\255\255\
\016\001\017\001\018\001\019\001\020\001\029\001\030\001\003\001\
\255\255\255\255\026\001\255\255\028\001\037\001\038\001\039\001\
\255\255\255\255\054\001\255\255\044\001\045\001\046\001\047\001\
\255\255\049\001\003\001\255\255\255\255\053\001\255\255\055\001\
\255\255\255\255\255\255\059\001\255\255\255\255\054\001\255\255\
\007\001\037\001\038\001\039\001\255\255\255\255\013\001\014\001\
\044\001\045\001\046\001\047\001\255\255\049\001\255\255\255\255\
\255\255\053\001\054\001\055\001\037\001\038\001\039\001\059\001\
\255\255\255\255\255\255\044\001\045\001\046\001\047\001\255\255\
\049\001\007\001\255\255\042\001\053\001\255\255\055\001\013\001\
\014\001\048\001\059\001\050\001\018\001\019\001\020\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\042\001\255\255\255\255\255\255\
\255\255\255\255\048\001\255\255\050\001"

let yynames_const = "\
  EOF\000\
  NEWLINE\000\
  AT\000\
  OP_PAR\000\
  CL_PAR\000\
  COMMA\000\
  DOT\000\
  KAPPA_RAR\000\
  KAPPA_LRAR\000\
  KAPPA_LNK\000\
  ARROW\000\
  PIPE\000\
  "

let yynames_block = "\
  PLUS\000\
  MULT\000\
  MINUS\000\
  LOG\000\
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
  SIGNAL\000\
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
  SQRT\000\
  EXPONENT\000\
  POW\000\
  ABS\000\
  MODULO\000\
  STOP\000\
  KAPPA_NOPOLY\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 35 "grammar/kappaParser.mly"
  (_1)
# 463 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 37 "grammar/kappaParser.mly"
 (let rule_label,r = _1 in Ast.result := {!Ast.result with Ast.rules = (rule_label,r)::!Ast.result.Ast.rules} ; _2)
# 471 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'newline) in
    Obj.repr(
# 39 "grammar/kappaParser.mly"
 (let inst = _1 in
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
# 502 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "grammar/kappaParser.mly"
        (raise (ExceptionDefn.Syntax_Error "Syntax error"))
# 508 "grammar/kappaParser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 68 "grammar/kappaParser.mly"
 (_2)
# 515 "grammar/kappaParser.ml"
               : 'newline))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "grammar/kappaParser.mly"
 (())
# 521 "grammar/kappaParser.ml"
               : 'newline))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'agent_expression) in
    Obj.repr(
# 75 "grammar/kappaParser.mly"
 ((Ast.SIG (_2,_1)))
# 529 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    Obj.repr(
# 77 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))
# 536 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'multiple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 79 "grammar/kappaParser.mly"
 (Ast.INIT (_2,_3,_1))
# 545 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    Obj.repr(
# 81 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed initial condition, I was expecting something of the form '%init: n kappa_expression'"))
# 552 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_declaration) in
    Obj.repr(
# 83 "grammar/kappaParser.mly"
 (Ast.DECLARE _2)
# 560 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_declaration) in
    Obj.repr(
# 85 "grammar/kappaParser.mly"
 (Ast.OBS _2)
# 568 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 87 "grammar/kappaParser.mly"
 (Ast.PLOT _2)
# 576 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    Obj.repr(
# 89 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed plot instruction, I was expecting an algebraic expression of variables"))
# 583 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'modif_expr) in
    Obj.repr(
# 91 "grammar/kappaParser.mly"
 (Ast.PERT (_2,_4,_1,None))
# 593 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Misc.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'modif_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 93 "grammar/kappaParser.mly"
 (Ast.PERT (_2,_4,_1,Some _6))
# 605 "grammar/kappaParser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 96 "grammar/kappaParser.mly"
                          (Ast.VAR_KAPPA (_2,_1))
# 613 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 97 "grammar/kappaParser.mly"
                 (Ast.VAR_ALG (_2,_1))
# 621 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Misc.position) in
    Obj.repr(
# 99 "grammar/kappaParser.mly"
 (let str,pos = _1 in
		raise 
		(ExceptionDefn.Syntax_Error 
		(Printf.sprintf "Variable '%s' should be either a pure kappa expression or an algebraic expression on variables" str)
		) 
	)
# 633 "grammar/kappaParser.ml"
               : 'variable_declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    Obj.repr(
# 109 "grammar/kappaParser.mly"
 (_2)
# 640 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 111 "grammar/kappaParser.mly"
 (Ast.AND (_1,_3,_2))
# 649 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 113 "grammar/kappaParser.mly"
 (Ast.OR (_1,_3,_2))
# 658 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 115 "grammar/kappaParser.mly"
 (Ast.GREATER (_1,_3,_2))
# 667 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 117 "grammar/kappaParser.mly"
 (Ast.SMALLER (_1,_3,_2))
# 676 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 119 "grammar/kappaParser.mly"
 (Ast.EQUAL (_1,_3,_2))
# 685 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 121 "grammar/kappaParser.mly"
 (Ast.NOT (_2,_1))
# 693 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 123 "grammar/kappaParser.mly"
 (Ast.TRUE _1)
# 700 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 125 "grammar/kappaParser.mly"
 (Ast.FALSE _1)
# 707 "grammar/kappaParser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'modif_expr) in
    Obj.repr(
# 130 "grammar/kappaParser.mly"
 (_2)
# 714 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'multiple_mixture) in
    Obj.repr(
# 132 "grammar/kappaParser.mly"
 (let (alg,mix) = _2 in Ast.INTRO (alg,mix,_1))
# 722 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    Obj.repr(
# 134 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Malformed perturbation instruction, I was expecting '$(ADD) alg_expression kappa_expression'"))
# 729 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'multiple_mixture) in
    Obj.repr(
# 136 "grammar/kappaParser.mly"
 (let (alg,mix) = _2 in Ast.DELETE (alg,mix,_1))
# 737 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string*Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 138 "grammar/kappaParser.mly"
 (let lab,pos_lab = _1 in Ast.UPDATE (lab,pos_lab,_3,_2))
# 746 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 140 "grammar/kappaParser.mly"
 (Ast.SNAPSHOT _1)
# 753 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 142 "grammar/kappaParser.mly"
 (Ast.STOP _1)
# 760 "grammar/kappaParser.ml"
               : 'modif_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "grammar/kappaParser.mly"
          (1)
# 766 "grammar/kappaParser.ml"
               : 'multiple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*Misc.position) in
    Obj.repr(
# 147 "grammar/kappaParser.mly"
      (let int,_=_1 in int)
# 773 "grammar/kappaParser.ml"
               : 'multiple))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "grammar/kappaParser.mly"
 ({Ast.lbl_nme = None ; Ast.lbl_ref = None})
# 779 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Misc.position) in
    Obj.repr(
# 154 "grammar/kappaParser.mly"
 (let lab,pos = _1 in {Ast.lbl_nme=Some (lab,pos) ; Ast.lbl_ref = None})
# 786 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string*Misc.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string*Misc.position) in
    Obj.repr(
# 156 "grammar/kappaParser.mly"
 (let ref,pos = _2 and lab,pos' = _4 in {Ast.lbl_nme=Some (lab,pos') ; Ast.lbl_ref = Some (ref,pos)})
# 795 "grammar/kappaParser.ml"
               : 'rule_label))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "grammar/kappaParser.mly"
 (Ast.EMPTY_MIX)
# 801 "grammar/kappaParser.ml"
               : 'mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 163 "grammar/kappaParser.mly"
 (_1)
# 808 "grammar/kappaParser.ml"
               : 'mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rule_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'mixture) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mixture) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'rate) in
    Obj.repr(
# 168 "grammar/kappaParser.mly"
 (let (k2,k1) = _6 in 
		(_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=k2; Ast.k_un=k1})
	)
# 821 "grammar/kappaParser.ml"
               : 'rule_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'rule_label) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'mixture) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arrow) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'mixture) in
    Obj.repr(
# 172 "grammar/kappaParser.mly"
 ((_1,{Ast.lhs=_2; Ast.arrow=_3; Ast.rhs=_4; Ast.k_def=(Ast.FLOAT (1.0,Misc.no_pos)); Ast.k_un=None}))
# 831 "grammar/kappaParser.ml"
               : 'rule_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "grammar/kappaParser.mly"
 (Ast.RAR)
# 837 "grammar/kappaParser.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 179 "grammar/kappaParser.mly"
 (Ast.RAR_NOPOLY _1)
# 844 "grammar/kappaParser.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 184 "grammar/kappaParser.mly"
 (Ast.INFINITY _1)
# 851 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float*Misc.position) in
    Obj.repr(
# 186 "grammar/kappaParser.mly"
 (let f,pos = _1 in Ast.FLOAT (f,pos))
# 858 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*Misc.position) in
    Obj.repr(
# 188 "grammar/kappaParser.mly"
 (let i,pos = _1 in Ast.FLOAT (float_of_int i,pos))
# 865 "grammar/kappaParser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Misc.position) in
    Obj.repr(
# 193 "grammar/kappaParser.mly"
 (Ast.OBS_VAR _1)
# 872 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 195 "grammar/kappaParser.mly"
 (Ast.TIME_VAR _1)
# 879 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 197 "grammar/kappaParser.mly"
 (Ast.EVENT_VAR _1)
# 886 "grammar/kappaParser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'alg_expr) in
    Obj.repr(
# 202 "grammar/kappaParser.mly"
 (_2)
# 893 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 204 "grammar/kappaParser.mly"
 (_1)
# 900 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 206 "grammar/kappaParser.mly"
 (_1)
# 907 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 208 "grammar/kappaParser.mly"
 (Ast.MULT (_1,_3,_2))
# 916 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 210 "grammar/kappaParser.mly"
 (Ast.SUM (_1,_3,_2))
# 925 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 212 "grammar/kappaParser.mly"
 (Ast.DIV (_1,_3,_2))
# 934 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 214 "grammar/kappaParser.mly"
 (Ast.MINUS (_1,_3,_2))
# 943 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 216 "grammar/kappaParser.mly"
 (Ast.POW (_1,_3,_2))
# 952 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 218 "grammar/kappaParser.mly"
 (Ast.MODULO (_1,_3,_2))
# 961 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 220 "grammar/kappaParser.mly"
 (Ast.EXP (_2,_1))
# 969 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 222 "grammar/kappaParser.mly"
 (Ast.SINUS (_2,_1))
# 977 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 224 "grammar/kappaParser.mly"
 (Ast.COSINUS (_2,_1))
# 985 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 226 "grammar/kappaParser.mly"
 (Ast.ABS (_2,_1))
# 993 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 228 "grammar/kappaParser.mly"
 (Ast.SQRT (_2,_1))
# 1001 "grammar/kappaParser.ml"
               : 'alg_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'alg_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 233 "grammar/kappaParser.mly"
 ((_1,Some _3))
# 1009 "grammar/kappaParser.ml"
               : 'rate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'alg_expr) in
    Obj.repr(
# 235 "grammar/kappaParser.mly"
 ((_1,None))
# 1016 "grammar/kappaParser.ml"
               : 'rate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'alg_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 240 "grammar/kappaParser.mly"
 ((_1,_2))
# 1024 "grammar/kappaParser.ml"
               : 'multiple_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 242 "grammar/kappaParser.mly"
 ((Ast.FLOAT (1.,Misc.no_pos),_1))
# 1031 "grammar/kappaParser.ml"
               : 'multiple_mixture))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'non_empty_mixture) in
    Obj.repr(
# 247 "grammar/kappaParser.mly"
 (_2)
# 1038 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 249 "grammar/kappaParser.mly"
 (Ast.COMMA (_1,_3))
# 1046 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 251 "grammar/kappaParser.mly"
 (Ast.DOT (-1,_1,_3))
# 1054 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 253 "grammar/kappaParser.mly"
 (Ast.DOT (_2,_1,_3))
# 1063 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 255 "grammar/kappaParser.mly"
 (Ast.PLUS (-1,_1,_3))
# 1072 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agent_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'non_empty_mixture) in
    Obj.repr(
# 257 "grammar/kappaParser.mly"
 (Ast.PLUS (_2,_1,_3))
# 1081 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'agent_expression) in
    Obj.repr(
# 259 "grammar/kappaParser.mly"
 (Ast.COMMA(_1,Ast.EMPTY_MIX))
# 1088 "grammar/kappaParser.ml"
               : 'non_empty_mixture))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string*Misc.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'interface_expression) in
    Obj.repr(
# 264 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id; Ast.ag_intf=_3; Ast.ag_pos=pos})
# 1096 "grammar/kappaParser.ml"
               : 'agent_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Misc.position) in
    Obj.repr(
# 266 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.ag_nme=id;Ast.ag_intf=Ast.EMPTY_INTF;Ast.ag_pos=pos})
# 1103 "grammar/kappaParser.ml"
               : 'agent_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "grammar/kappaParser.mly"
 (Ast.EMPTY_INTF)
# 1109 "grammar/kappaParser.ml"
               : 'interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ne_interface_expression) in
    Obj.repr(
# 273 "grammar/kappaParser.mly"
 (_1)
# 1116 "grammar/kappaParser.ml"
               : 'interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'port_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ne_interface_expression) in
    Obj.repr(
# 278 "grammar/kappaParser.mly"
 (Ast.PORT_SEP(_1,_3))
# 1124 "grammar/kappaParser.ml"
               : 'ne_interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'port_expression) in
    Obj.repr(
# 280 "grammar/kappaParser.mly"
 (Ast.PORT_SEP(_1,Ast.EMPTY_INTF))
# 1131 "grammar/kappaParser.ml"
               : 'ne_interface_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string*Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'internal_state) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'link_state) in
    Obj.repr(
# 285 "grammar/kappaParser.mly"
 (let (id,pos) = _1 in {Ast.port_nme=id; Ast.port_int=_2; Ast.port_lnk=_3; Ast.port_pos=pos})
# 1140 "grammar/kappaParser.ml"
               : 'port_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 289 "grammar/kappaParser.mly"
          ([])
# 1146 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Misc.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'internal_state) in
    Obj.repr(
# 291 "grammar/kappaParser.mly"
 (let m,pos = _1 in m::_2)
# 1154 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 293 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid internal state"))
# 1160 "grammar/kappaParser.ml"
               : 'internal_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 298 "grammar/kappaParser.mly"
 (Ast.FREE)
# 1166 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int*Misc.position) in
    Obj.repr(
# 300 "grammar/kappaParser.mly"
 (Ast.LNK_VALUE _2)
# 1173 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 302 "grammar/kappaParser.mly"
 (Ast.LNK_SOME _2)
# 1180 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string*Misc.position) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string*Misc.position) in
    Obj.repr(
# 304 "grammar/kappaParser.mly"
 (Ast.LNK_TYPE (_2,_4))
# 1188 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Misc.position) in
    Obj.repr(
# 306 "grammar/kappaParser.mly"
 (Ast.LNK_ANY _1)
# 1195 "grammar/kappaParser.ml"
               : 'link_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 308 "grammar/kappaParser.mly"
 (raise (ExceptionDefn.Syntax_Error "Invalid link state"))
# 1201 "grammar/kappaParser.ml"
               : 'link_state))
(* Entry line *)
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
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
