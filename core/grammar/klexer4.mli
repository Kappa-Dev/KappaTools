val model :
  Lexing.lexbuf -> Ast.parsing_instruction list * string Locality.annot list

val compile :
  Format.formatter ->
  (Ast.agent, Ast.mixture, Ast.mixture, string, Ast.rule) Ast.compil ->
  string ->
  Ast.parsing_compil

val token : Lexing.lexbuf -> Kparser4.token
