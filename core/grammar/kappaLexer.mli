val compile :
  Format.formatter ->
  (Ast.agent, Ast.mixture, Ast.mixture, string, Ast.rule) Ast.compil ->
  string (** file *) ->
  Ast.parsing_compil

val position : Lexing.lexbuf -> string * int * int
val space_chars : char list
val reset_eof : Lexing.lexbuf -> unit
val token : Lexing.lexbuf -> KappaParser.token
