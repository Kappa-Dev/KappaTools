val model :
  Lexing.lexbuf -> Ast.parsing_instruction list * string Loc.annoted list

val compile :
  Format.formatter ->
  all_rules_in_ws:bool ->
  rules_in_ws:int list ->
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
