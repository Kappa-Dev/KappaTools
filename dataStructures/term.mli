type 'a with_pos = 'a * (Lexing.position * Lexing.position)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR |
		    PROD_EVENT_VAR

val bin_alg_op_to_string : unit -> bin_alg_op -> string
val print_bin_alg_op : out_channel -> bin_alg_op -> unit
val un_alg_op_to_string : unit -> un_alg_op -> string
val print_un_alg_op : out_channel -> un_alg_op -> unit
val state_alg_op_to_string : unit -> state_alg_op -> string
val print_state_alg_op : out_channel -> state_alg_op -> unit
