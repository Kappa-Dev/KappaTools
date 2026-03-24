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
  string (** file *) ->
  Ast.parsing_compil

val position : Lexing.lexbuf -> string * int * int
val space_chars : char list
val reset_eof : Lexing.lexbuf -> unit
val token : Lexing.lexbuf -> KappaParser.token
