type 'a formula =
  | P of 'a
  | NOT of 'a formula
  | IMPLY of 'a formula * 'a formula
  | AND of 'a formula * 'a formula
  | OR of 'a formula * 'a formula
  | False
  | True

val simplify : 'a formula -> 'a formula
val print_formula : 'a -> (string -> 'a -> 'a) -> string formula -> 'a
val formula_to_json : ('a -> Yojson.Basic.t) -> 'a formula -> Yojson.Basic.t
val formula_of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a formula

val convert_p :
  ('a -> 'error -> 'error * 'b) -> 'error -> 'a formula -> 'error * 'b formula

val get_list_of_predicates : (string * 'a) formula -> string list
