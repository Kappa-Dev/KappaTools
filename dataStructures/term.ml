type 'a with_pos = 'a * (Lexing.position * Lexing.position)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR |
		    PROD_EVENT_VAR

let bin_alg_op_to_string () = function
  | MULT -> "*"
  | SUM -> "+"
  | DIV -> "/"
  | MINUS -> "-"
  | POW -> "^"
  | MODULO -> "[mod]"
  | MIN -> "[min]"
  | MAX -> "[max]"

let print_bin_alg_op f op =
  Printf.fprintf f "%s" (bin_alg_op_to_string () op)

let un_alg_op_to_string () = function
  | COSINUS -> "[cos]"
  | SINUS -> "[sin]"
  | TAN -> "[tan]"
  | EXP -> "[exp]"
  | SQRT -> "[sqrt]"
  | INT -> "[int]"
  | LOG -> "[log]"
  | UMINUS -> "-"

let print_un_alg_op f op =
  Printf.fprintf f "%s" (un_alg_op_to_string () op)

let state_alg_op_to_string () = function
  | CPUTIME -> "[Tsim]"
  | TIME_VAR -> "[T]"
  | EVENT_VAR -> "[E]"
  | NULL_EVENT_VAR -> "[E-]"
  | PROD_EVENT_VAR -> "[E+]"

let print_state_alg_op f op =
  Printf.fprintf f "%s" (state_alg_op_to_string () op)
