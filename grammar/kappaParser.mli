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

val start_rule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
