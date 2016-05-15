(** Compiled algebraic expression *)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR
		    | TMAX_VAR | EMAX_VAR | PLOTNUM
type bool_op = AND | OR
type compare_op = GREATER | SMALLER | EQUAL | DIFF

(** {6 Printers} *)

val print_bin_alg_op : Format.formatter -> bin_alg_op -> unit
val print_un_alg_op : Format.formatter -> un_alg_op -> unit
val print_state_alg_op : Format.formatter -> state_alg_op -> unit
val print_bool_op : Format.formatter -> bool_op -> unit
val print_compare_op : Format.formatter -> compare_op -> unit

(** {6 Dependencies management} *)

type rev_dep = ALG of int | RULE of int | PERT of int
module DepSet : SetMap.Set with type elt = rev_dep
val print_rev_dep : Format.formatter -> rev_dep -> unit
