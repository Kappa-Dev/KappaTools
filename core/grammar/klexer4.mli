val model :
  Lexing.lexbuf -> Ast.parsing_instruction list * string Loc.annoted list

val compile :
  Format.formatter ->
  rules_in_ws:int list ->
  removed_rules:int list ->
  ( Ast.agent,
    Ast.agent_sig,
    Ast.mixture,
    Ast.mixture,
    string,
    Ast.rule )
  Ast.compil ->
  string ->
  Ast.parsing_compil

val token : Lexing.lexbuf -> Kparser4.token
