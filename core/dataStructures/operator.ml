(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type bin_alg_op = MULT | SUM | DIV | MINUS | POW | MODULO | MIN | MAX
type un_alg_op = LOG | SQRT | EXP | SINUS | COSINUS | TAN | INT | UMINUS

type state_alg_op =
  | CPUTIME
  | TIME_VAR
  | EVENT_VAR
  | NULL_EVENT_VAR
  | TMAX_VAR
  | EMAX_VAR

type bin_bool_op = AND | OR
type un_bool_op = NOT
type compare_op = GREATER | SMALLER | EQUAL | DIFF
type rev_dep = ALG of int | RULE of int | MODIF of int

let bin_alg_op_to_string = function
  | MULT -> "*"
  | SUM -> "+"
  | DIV -> "/"
  | MINUS -> "-"
  | POW -> "^"
  | MODULO -> "[mod]"
  | MIN -> "[min]"
  | MAX -> "[max]"

let bin_alg_op_is_prefix = function
  | MAX | MIN -> true
  | MULT | SUM | DIV | MINUS | POW | MODULO -> false

let print_bin_alg_op fx x fy y f op =
  if bin_alg_op_is_prefix op then
    Format.fprintf f "%s (%a) (%a)" (bin_alg_op_to_string op) fx x fy y
  else
    Format.fprintf f "(%a %s %a)" fx x (bin_alg_op_to_string op) fy y

let bin_alg_op_to_json op = `String (bin_alg_op_to_string op)

let bin_alg_op_of_json = function
  | `String "*" -> MULT
  | `String "+" -> SUM
  | `String "/" -> DIV
  | `String "-" -> MINUS
  | `String "^" -> POW
  | `String "[mod]" -> MODULO
  | `String "[min]" -> MIN
  | `String "[max]" -> MAX
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect bin_alg_op", x))

let un_alg_op_to_string = function
  | COSINUS -> "[cos]"
  | SINUS -> "[sin]"
  | TAN -> "[tan]"
  | EXP -> "[exp]"
  | SQRT -> "[sqrt]"
  | INT -> "[int]"
  | LOG -> "[log]"
  | UMINUS -> "-"

let print_un_alg_op f op = Format.pp_print_string f (un_alg_op_to_string op)
let un_alg_op_to_json op = `String (un_alg_op_to_string op)

let un_alg_op_of_json = function
  | `String "[cos]" -> COSINUS
  | `String "[sin]" -> SINUS
  | `String "[tan]" -> TAN
  | `String "[exp]" -> EXP
  | `String "[sqrt]" -> SQRT
  | `String "[int]" -> INT
  | `String "[log]" -> LOG
  | `String "-" -> UMINUS
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect un_alg_op", x))

let state_alg_op_to_string = function
  | CPUTIME -> "[Tsim]"
  | TIME_VAR -> "[T]"
  | EVENT_VAR -> "[E]"
  | NULL_EVENT_VAR -> "[E-]"
  | TMAX_VAR -> "[Tmax]"
  | EMAX_VAR -> "[Emax]"

let print_state_alg_op f op =
  Format.pp_print_string f (state_alg_op_to_string op)

let state_alg_op_to_json op = `String (state_alg_op_to_string op)

let state_alg_op_of_json = function
  | `String "[Tsim]" -> CPUTIME
  | `String "[T]" -> TIME_VAR
  | `String "[E]" -> EVENT_VAR
  | `String "[E-]" -> NULL_EVENT_VAR
  | `String "[Tmax]" -> TMAX_VAR
  | `String "[Emax]" -> EMAX_VAR
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect state_alg_op", x))

let bin_bool_op_to_string = function
  | AND -> "&&"
  | OR -> "||"

let print_bin_bool_op f op = Format.pp_print_string f (bin_bool_op_to_string op)
let bin_bool_op_to_json op = `String (bin_bool_op_to_string op)

let bin_bool_op_of_json = function
  | `String "&&" -> AND
  | `String "||" -> OR
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect boolean op", x))

let un_bool_op_to_string = function
  | NOT -> "[not]"

let print_un_bool_op f op = Format.pp_print_string f (un_bool_op_to_string op)
let un_bool_op_to_json op = `String (un_bool_op_to_string op)

let un_bool_op_of_json = function
  | `String "[not]" -> NOT
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect boolean op", x))

let compare_op_to_string = function
  | GREATER -> ">"
  | SMALLER -> "<"
  | EQUAL -> "="
  | DIFF -> "!="

let print_compare_op f op = Format.pp_print_string f (compare_op_to_string op)
let compare_op_to_json op = `String (compare_op_to_string op)

let compare_op_of_json = function
  | `String ">" -> GREATER
  | `String "<" -> SMALLER
  | `String "=" -> EQUAL
  | `String "!=" -> DIFF
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect compare_op", x))

let print_rev_dep f = function
  | RULE id -> Format.fprintf f "rate_of_rule [%i]" id
  (*"rate of rule '%a'" (Model.print_rule env) id*)
  | ALG id -> Format.fprintf f "algebraic variable [%i]" id
  (*"variable '%a'" (Model.print_alg env) id*)
  | MODIF id -> Format.fprintf f "intervention [%i]" id

let rev_dep_to_yojson = function
  | RULE id -> `List [ `String "RULE"; `Int id ]
  | ALG id -> `List [ `String "ALG"; `Int id ]
  | MODIF id -> `List [ `String "MODIF"; `Int id ]

let rev_dep_of_yojson = function
  | `List [ `String "RULE"; `Int id ] -> RULE id
  | `List [ `String "ALG"; `Int id ] -> ALG id
  | `List [ `String "MODIF"; `Int id ] -> MODIF id
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect rev_dep", x))

module DepSetMap = SetMap.Make (struct
  type t = rev_dep

  let compare = compare
  let print = print_rev_dep
end)

module DepSet = DepSetMap.Set

let depset_to_yojson x =
  `List (DepSet.fold (fun x a -> rev_dep_to_yojson x :: a) x [])

let depset_of_yojson = function
  | `Null -> DepSet.empty
  | `List l ->
    List.fold_left
      (fun acc x -> DepSet.add (rev_dep_of_yojson x) acc)
      DepSet.empty l
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid depset", x))
