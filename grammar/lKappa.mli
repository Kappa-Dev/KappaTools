type switching =
  | Linked of int Location.annot | Freed | Maintained | Erased

type rule_internal =
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int
type rule_agent =
  { ra_type: int;
    ra_erased: bool;
    ra_ports: ((int,int*int) Ast.link Location.annot * switching) array;
    ra_ints: rule_internal array;
    ra_syntax: (((int,int*int) Ast.link Location.annot * switching) array *
		  rule_internal array) option;
  }

val print_rule_mixture :
  Signature.s -> Format.formatter -> rule_agent list -> unit
val annotate_lhs_with_diff :
  Signature.s -> Ast.mixture -> Ast.mixture ->
  rule_agent list * (int * (Raw_mixture.agent * Location.t) list)
