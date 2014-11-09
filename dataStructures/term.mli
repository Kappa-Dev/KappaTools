(** Compiled representation of Kappa programs *)

type 'a with_pos = 'a * (Lexing.position * Lexing.position)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR |
		    PROD_EVENT_VAR
type bool_op = AND | OR
type compare_op = GREATER | SMALLER | EQUAL | DIFF

(** {6 Printers} *)

val bin_alg_op_to_string : unit -> bin_alg_op -> string
val print_bin_alg_op : Format.formatter -> bin_alg_op -> unit
val un_alg_op_to_string : unit -> un_alg_op -> string
val print_un_alg_op : Format.formatter -> un_alg_op -> unit
val state_alg_op_to_string : unit -> state_alg_op -> string
val print_state_alg_op : Format.formatter -> state_alg_op -> unit
val bool_op_to_string : unit -> bool_op -> string
val print_bool_op : Format.formatter -> bool_op -> unit
val compare_op_to_string : unit -> compare_op -> string
val print_compare_op : Format.formatter -> compare_op -> unit

(** {6 Dependencies management} *)

type dep_type = ALG of int | KAPPA of int | TOK of int | EVENT |
		TIME | RULE of int | PERT of int | ABORT of int
module DepMap : Map.S with type key = dep_type
module DepSet : Set.S with type elt = dep_type

val dep_of_state_alg_op : state_alg_op -> dep_type
val dep_to_string : unit -> dep_type -> string

val with_dummy_pos : 'a -> 'a with_pos
val has_dummy_pos : 'a with_pos -> bool

val print_dep_type : Format.formatter -> dep_type -> unit
