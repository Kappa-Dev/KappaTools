{
 open Lexing
 open KappaParser
 open ExceptionDefn

 let reach_eof lexbuf = 
   lexbuf.lex_eof_reached <- true 

 let reset_eof lexbuf = 
   lexbuf.lex_eof_reached <- false 

 let incr_line lexbuf = 
   let pos = lexbuf.lex_curr_p in
     lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum+1 ; pos_bol = pos.pos_cnum}

 let return_error lexbuf msg = 
	let pos = lexbuf.lex_curr_p in
		let line = pos.pos_lnum in
			let loc = Printf.sprintf "line %d, character %d:" line (pos.pos_cnum - pos.pos_bol) in
			let full_msg = Printf.sprintf "Error (%s) %s %s" pos.pos_fname loc msg 
			in
				Printf.eprintf "%s\n" full_msg ; exit 1 
			
 let position lexbuf = 
	let pos = lexbuf.lex_curr_p in
		(pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
}

let blank = [' ' '\t']
let integer = (['0'-'9']+)
let real = 
  (((['0'-'9'] | ['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+)) ((['e' 'E'] ['+' '-'] ['0'-'9']+) | (['e' 'E'] ['0'-'9']+))) 
  | ((['0'-'9']+ '.' ['0'-'9']*) | (['0'-'9']* '.' ['0'-'9']+))   
let id = (['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*)
let internal_state = '~' (['0'-'9' 'a'-'z' 'A'-'Z']+)
let dot_radius = '.' '{' (['0'-'9']+) '}'
let plus_radius = '+' '{' (['0'-'9']+) '}'
let pert = '$' id

rule token = parse
    | "\\\n" {incr_line lexbuf ; token lexbuf}
		| "do" {let pos = position lexbuf in DO pos}
		| "until" {let pos = position lexbuf in UNTIL pos}
		| ":=" {let pos = position lexbuf in SET pos}
		| "&&" {let pos = position lexbuf in AND pos}
		| "||" {let pos = position lexbuf in OR pos}
    | "->" {KAPPA_RAR}
		| "->!" {let pos = position lexbuf in KAPPA_NOPOLY pos}
		| pert as s {let pos = position lexbuf in
									match s with  
						 			| "$DEL" -> (DELETE pos)
									| "$ADD" -> (INTRO pos)
									| "$SNAPSHOT" -> (SNAPSHOT pos) 
									| "$STOP" -> (STOP pos) 
									| s -> return_error lexbuf ("Perturbation effect \""^s^"\" is not defined")
					 			}  
		| '[' {let lab = read_label "" [']'] lexbuf in 
						let pos = position lexbuf in 
							match lab with
								| "E" -> EVENT pos
								| "T" -> TIME pos
								| "log" -> LOG pos
								| "sin" -> SINUS pos
								| "cos" -> COSINUS pos
								| "tan" -> TAN pos
								| "exp" -> EXPONENT pos
								| "int" -> ABS pos
								| "mod" -> MODULO pos
								| "sqrt" -> SQRT pos
								| "inf" -> INFINITY pos
								| "true" -> TRUE pos
								| "false" -> FALSE pos
								| "not" -> NOT pos
								| "pi" -> FLOAT (3.14159265,pos)
								| "emax" -> EMAX pos
								| "tmax" -> TMAX pos
								| _ as s -> return_error lexbuf ("Symbol \""^s^"\" is not defined")
						}  
		| '|' {PIPE}
    | '\n' {incr_line lexbuf ; NEWLINE}
		| '\r' {incr_line lexbuf ; NEWLINE}
    | '#' {comment lexbuf}
    | integer as n {let pos = position lexbuf in INT(int_of_string n,pos)}
    | real as f {let pos = position lexbuf in FLOAT(float_of_string f,pos)}
    | '\'' {let lab = read_label "" ['\''] lexbuf in let pos = position lexbuf in LABEL(lab,pos)}
    | id as str {let pos = position lexbuf in ID(str,pos)}
    | '@' {AT}
    | ',' {COMMA}
    | '(' {OP_PAR}
    | ')' {CL_PAR}
		| '.' {DOT}
		| '+' {let pos = position lexbuf in PLUS pos}
		| '*' {let pos = position lexbuf in MULT pos}
		| '-' {let pos = position lexbuf in MINUS pos}
		| '^' {let pos = position lexbuf in POW pos}
		| '/' {let pos = position lexbuf in DIV pos} 
		| '<' {let pos = position lexbuf in SMALLER pos}
		| '>' {let pos = position lexbuf in GREATER pos}
		| '=' {let pos = position lexbuf in EQUAL pos}
		| '%' {let lab = read_label "" [':'] lexbuf in 
						let pos = position lexbuf in 
							match lab with
								| "agent" -> (SIGNATURE pos)
								| "init" -> (INIT pos)
								| "var" -> (LET pos)
								| "plot" -> (PLOT pos)
								| "mod" -> (PERT pos)
								| "ref" -> (REF pos)
								| "obs" -> (OBS pos)
								| _ as s -> return_error lexbuf ("Instruction \""^s^"\" not recognized")
					 } 
		| dot_radius as s { let i = String.index s '{' in 
													let j = String.index s '}' in 
														let r = String.sub s (i+1) (j-i-1) in 
															try DOT_RADIUS (int_of_string r) with 
																| Failure _ -> return_error lexbuf (Printf.sprintf "Invalid radius")
										   }
		| plus_radius as s {let i = String.index s '{' in 
													let j = String.index s '}' in 
														let r = String.sub s (i+1) (j-i-1) in 
															try PLUS_RADIUS (int_of_string r) with 
																| Failure _ -> return_error lexbuf (Printf.sprintf "Invalid radius")
										   }
    | '!' {KAPPA_LNK}
    | internal_state as s {let i = String.index s '~' in 
			                     	 let r = String.sub s (i+1) (String.length s-i-1) in
																let pos = position lexbuf in 
																	KAPPA_MRK (r,pos)
													 }
    | '?' {let pos = position lexbuf in (KAPPA_WLD pos)}
    | '_' {let pos = position lexbuf in (KAPPA_SEMI pos)}
    | blank  {token lexbuf}
    | eof {reach_eof lexbuf; EOF}
    | _ as c {return_error lexbuf (Printf.sprintf "invalid use of character %c" c)}

  and read_label acc char_list = parse
    | eof {acc}
    | "\\\n" {incr_line lexbuf ; read_label acc char_list lexbuf}
    | _ as c {if List.mem c char_list then acc else read_label (Printf.sprintf "%s%c" acc c) char_list lexbuf}

  and comment = parse
    | '\n' {incr_line lexbuf ; NEWLINE}
    | "\\\n" {incr_line lexbuf ; comment lexbuf} 
    | eof {EOF}
    | _ {comment lexbuf}

{   
  let compile fic =
		let d = open_in fic in
		Parameter.openInDescriptors := d::(!Parameter.openInDescriptors) ;
		let lexbuf = Lexing.from_channel d in
		lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fic} ;
		try
			Debug.tag (Printf.sprintf "Parsing %s..." fic) ;
	   	KappaParser.start_rule token lexbuf ; Debug.tag "done" ; close_in d ;
			Parameter.openInDescriptors := List.tl (!Parameter.openInDescriptors)
 		with 
 			| Syntax_Error msg -> 
				(close_in d ; 
				Parameter.openInDescriptors := List.tl (!Parameter.openInDescriptors) ; 
				return_error lexbuf msg
				) 
			| exn -> (Printexc.print_backtrace stderr ; raise exn)
}
