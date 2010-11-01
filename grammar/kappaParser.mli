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

val line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
