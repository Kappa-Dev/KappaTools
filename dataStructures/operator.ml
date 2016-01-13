type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR
type bool_op = AND | OR
type compare_op = GREATER | SMALLER | EQUAL | DIFF

type rev_dep = ALG of int | RULE of int | PERT of int
module DepSetMap = SetMap.Make (struct type t = rev_dep let compare = compare end)
module DepSet = DepSetMap.Set

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
  Format.fprintf f "%s" (bin_alg_op_to_string () op)

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
  Format.fprintf f "%s" (un_alg_op_to_string () op)

let state_alg_op_to_string () = function
  | CPUTIME -> "[Tsim]"
  | TIME_VAR -> "[T]"
  | EVENT_VAR -> "[E]"
  | NULL_EVENT_VAR -> "[E-]"

let print_state_alg_op f op =
  Format.fprintf f "%s" (state_alg_op_to_string () op)

let bool_op_to_string () = function
  | AND -> "&&"
  | OR -> "||"

let print_bool_op f op =
  Format.fprintf f "%s" (bool_op_to_string () op)

let compare_op_to_string () = function
  | GREATER -> ">"
  | SMALLER -> "<"
  | EQUAL -> "="
  | DIFF -> "!="

let print_compare_op f op =
  Format.fprintf f "%s" (compare_op_to_string () op)

let print_rev_dep f = function
  | RULE id ->
     Format.fprintf f "rate_of_rule [%i]" id
  (*"rate of rule '%a'" (Environment.print_rule env) id*)
  | ALG id ->
     Format.fprintf f "algebraic variable [%i]" id
  (*"variable '%a'" (Environment.print_alg env) id*)
  | PERT id -> Format.fprintf f "perturbation [%i]" id
