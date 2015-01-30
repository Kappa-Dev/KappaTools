type 'a with_pos = 'a * (Lexing.position * Lexing.position)
type 'a maybe_pos = ?pos:(Lexing.position * Lexing.position) -> 'a

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS
type state_alg_op = CPUTIME | TIME_VAR | EVENT_VAR | NULL_EVENT_VAR |
		    PROD_EVENT_VAR
type bool_op = AND | OR
type compare_op = GREATER | SMALLER | EQUAL | DIFF

type dep_type = ALG of int | KAPPA of int | TOK of int | EVENT |
		TIME | RULE of int | PERT of int | ABORT of int
module DepMap = Map.Make (struct type t = dep_type let compare = compare end)
module DepSet = Set.Make (struct type t = dep_type let compare = compare end)

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
  | PROD_EVENT_VAR -> "[E+]"

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

let dep_to_string () = function
  | TOK i -> "TOK("^(string_of_int i)^")"
  | ALG i -> "ALG("^(string_of_int i)^")"
  | KAPPA i -> "KAPPA("^(string_of_int i)^")"
  | EVENT -> "EVENT"
  | TIME -> "TIME"
  | RULE i -> "RULE("^(string_of_int i)^")"
  | PERT i -> "PERT("^(string_of_int i)^")"
  | ABORT i -> "ABORT("^(string_of_int i)^")"

let dep_of_state_alg_op = function
  | CPUTIME | EVENT_VAR | NULL_EVENT_VAR | PROD_EVENT_VAR -> EVENT
  | TIME_VAR -> TIME

let with_dummy_pos x = (x, (Lexing.dummy_pos, Lexing.dummy_pos))
let has_dummy_pos (_,(b_pos,e_pos)) =
  b_pos = Lexing.dummy_pos &&
    (e_pos = Lexing.dummy_pos || failwith "half dummy_pos")

let print_dep_type f = function
  | RULE id ->
     Format.fprintf f "rate_of_rule [%i]" id
  (*"rate of rule '%a'" (Environment.print_rule env) id*)
  | ALG id ->
     Format.fprintf f "algebraic variable [%i]" id
  (*"variable '%a'" (Environment.print_alg env) id*)
  | TOK id ->
     Format.fprintf f "token [%i]" id
  | KAPPA id ->
     Format.fprintf f "kappa_mixture [%i]" id
  | EVENT -> Format.fprintf f "event"
  | TIME -> Format.fprintf f "time"
  | PERT id -> Format.fprintf f "perturbation [%i]" id
  | ABORT id -> Format.fprintf f "ABORT(%i)" id
