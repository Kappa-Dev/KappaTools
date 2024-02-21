(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

{
open Kparser4

 let keyword_or_id =
 let keywords = Hashtbl.create 15 in
 let () = Hashtbl.add keywords "do" DO in
 let () = Hashtbl.add keywords "repeat" REPEAT in
 let () = Hashtbl.add keywords "INF" INFINITY in
 let () = Hashtbl.add keywords "inf" INFINITY in
 let () = Hashtbl.add keywords "alarm" ALARM in
 fun x ->
 try Hashtbl.find keywords x with Not_found -> ID x

}

let eol = '\r'? '\n'
let blank = [' ' '\t']
let integer = ['0'-'9']+
let real =
  (['0'-'9']+ ['e' 'E'] ['+' '-']? ['0'-'9']+) |
  (((['0'-'9']+ '.' ['0'-'9']*) | ('.' ['0'-'9']+))
    (['e' 'E'] ['+' '-']? ['0'-'9']+)?)
let id = ['_' '~'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '-' '+']+ |
         ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '~' '-' '+']*

rule token = parse
  | '/' '/' ([^'\r''\n']* as s) (eol | eof) {Lexing.new_line lexbuf; COMMENT s}
  | '/' '*' {COMMENT (inline_comment [] lexbuf)}
  | eol { Lexing.new_line lexbuf; NEWLINE }
  | blank + as x { SPACE x }
  | '#' { SHARP }
  | "&&" { AND }
  | "||" { OR }
  | "<->" { LRAR }
  | "<-" {LAR}
  | "->" { RAR }
  | "<>" { DIFF }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '@' { AT }
  | ',' { COMMA }
  | '\\' {BACKSLASH}
  | '(' { OP_PAR }
  | ')' { CL_PAR }
  | '[' { OP_BRA }
  | ']' { CL_BRA }
  | '{' { OP_CUR }
  | '}' { CL_CUR }
  | '|' { PIPE }
  | '.' { DOT }
  | '_' { UNDERSCORE }
  | '+' { PLUS }
  | '*' { MULT }
  | '-' { MINUS }
  | '^' { POW }
  | '/' { DIV }
  | '<' { SMALLER }
  | '>' { GREATER }
  | '=' { EQUAL }
  | integer as n { INT (int_of_string n) }
  | real as f { FLOAT (float_of_string f) }
  | '\'' ([^'\n' '\'']+ as x) '\''{ LABEL(x) }
  | '\"' ([^'\n' '\"']* as x) '\"'{ STRING(x) }
  | '\'' ([^'\n' '\'']+ as s) (eof | '\n')
    { raise (ExceptionDefn.Syntax_Error
      ("Unterminated label: "^s,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))}
  | '\"' ([^'\n' '\"']+ as s) (eof | '\n')
    { raise (ExceptionDefn.Syntax_Error
      ("Unterminated string: "^s,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))}
  | id as str { keyword_or_id str }
  | '%' (id as lab) ':' {
    match lab with
    | "agent" -> SIGNATURE
    | "init" -> INIT
    | "var" -> LET
    | "plot" -> PLOT
    | "mod" -> PERT
    | "obs" -> OBS
    | "def" -> CONFIG
    | "token" -> TOKEN
    | _ as s -> raise (ExceptionDefn.Syntax_Error
      ("Unknown directive: "^s,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))
    }
  | '[' blank* '?' blank* ']' { THEN }
  | '[' blank* ':' blank* ']' { ELSE }
  | '[' blank* (id as lab) blank* ']' {
    match lab with
    | "E" -> EVENT
    | "E-" -> NULL_EVENT
    | "T" -> TIME
    | "Tsim" -> CPUTIME
    | "log" -> LOG
    | "sin" -> SINUS
    | "cos" -> COSINUS
    | "tan" -> TAN
    | "exp" -> EXPONENT
    | "int" -> ABS
    | "mod" -> MOD
    | "sqrt" -> SQRT
    | "true" -> TRUE
    | "false" -> FALSE
    | "pi" -> FLOAT (3.14159265)
    | "max" -> MAX
    | "min" -> MIN
    | "Emax" -> EMAX
    | "Tmax" -> TMAX
    | "not" -> NOT
    | _ as s -> raise (ExceptionDefn.Syntax_Error
      ("Unknown primitive: "^s,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))
    }
  | '$' (id as s) {
    match s with
    | "APPLY" -> APPLY
    | "DEL" -> DELETE
    | "ADD" -> INTRO
    | "SNAPSHOT" -> SNAPSHOT
    | "STOP" -> STOP
    | "DIN" -> FLUX
    | "TRACK" -> TRACK
    | "UPDATE" -> ASSIGN
    | "PRINT" -> PRINTF
    | "PRINTF" -> PRINTF
    | "PLOTENTRY" -> PLOTENTRY
    | "RUN" -> RUN
    | "SPECIES_OF" -> SPECIES_OF
    | s -> raise (ExceptionDefn.Syntax_Error
      ("Unknown intervention: "^s,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))
    }
  | eof { lexbuf.Lexing.lex_eof_reached <- true; EOF }
  | _ as c { raise (ExceptionDefn.Syntax_Error
      ("Unknown character: "^String.make 1 c,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf))) }

and inline_comment acc = parse
  | ([^'\n' '*' '\"' '/'] *) as x { inline_comment (x::acc) lexbuf }
  | ('/' '/' [^'\r''\n']* as x) eol
    { Lexing.new_line lexbuf; inline_comment (x::acc) lexbuf }
  | eol as x { Lexing.new_line lexbuf; inline_comment (x::acc) lexbuf }
  | ('*' [^'/' '\n' '\"']) as x { inline_comment (x::acc) lexbuf }
  | ('/' [^'*' '\n' '\"']) as x { inline_comment (x::acc) lexbuf }
  | '*' '/' { String.concat "" (List.rev acc) }
  | eof { String.concat "" (List.rev acc) }
  | '*' '\n' { Lexing.new_line lexbuf; inline_comment ("*\n"::acc) lexbuf }
  | (('*' | '/')? '\"' [^'\n' '\"']+ '\"') as x
    { inline_comment (x::acc) lexbuf }
  | (('*' | '/')? '\"' [^'\n' '\"']+ (eof | '\n')) as x
    { raise (ExceptionDefn.Syntax_Error
      ("Unterminated string in comment: "^x,
       Loc.of_pos (Lexing.lexeme_start_p lexbuf)
         (Lexing.lexeme_end_p lexbuf)))}
  | '/' '*'
    { inline_comment ("*/"::(inline_comment ["/*"] lexbuf):: acc) lexbuf }
  | '/' '\n' { Lexing.new_line lexbuf; inline_comment ("/\n"::acc) lexbuf }

and recovery = parse
  | [^'\r''\n']* (eol | eof) {Lexing.new_line lexbuf}

{
  let rec aux_model err lex =
  try let out = Kparser4.model token lex in (out, List.rev err)
  with ExceptionDefn.Syntax_Error e ->
    let () = recovery lex in
    aux_model (e::err) lex

  let model lex = aux_model [] lex

  let compile logger compil fic =
    let d = open_in fic in
    let lexbuf = Lexing.from_channel d in
    let () = lexbuf.Lexing.lex_curr_p <-
      {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fic} in
    let compil = { compil with Ast.filenames = fic :: compil.Ast.filenames } in
    let () = Format.fprintf logger "Parsing %s...@." fic in
    let (out,err) = model lexbuf in
    let () = Format.fprintf logger "done@." in
    let () = match err with
      | [] -> ()
      | (msg,pos)::_ ->
        let () = Pp.error Format.pp_print_string (msg,pos) in
        exit 3 in
    let () = close_in d in
    Cst.append_to_ast_compil out compil
}
