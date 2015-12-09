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

type rule_mixture = rule_agent list

type rule =
  { r_mix: rule_mixture;
    r_created: Raw_mixture.t;
    r_rm_tokens :
      ((rule_mixture,int) Ast.ast_alg_expr Location.annot * int) list;
    r_add_tokens :
      ((rule_mixture,int) Ast.ast_alg_expr Location.annot * int) list;
    r_rate : (rule_mixture,int) Ast.ast_alg_expr Location.annot;
    r_rate_absolute : bool;
    r_un_rate : (rule_mixture,int) Ast.ast_alg_expr Location.annot option;
  }

val to_erased : rule_mixture -> rule_mixture
val to_maintained : rule_mixture -> rule_mixture
val to_raw_mixture : Signature.s -> rule_mixture -> Raw_mixture.t
val copy_rule_agent : rule_agent -> rule_agent

val print_rule_mixture :
  Signature.s -> Format.formatter -> rule_agent list -> unit
val print_rule :
  ltypes:bool -> rates:bool -> Signature.s ->
  (Format.formatter -> int -> unit) -> (Format.formatter -> int -> unit) ->
  Format.formatter -> rule -> unit
val annotate_lhs_with_diff :
  Signature.s -> Ast.mixture -> Ast.mixture ->
  rule_agent list * (Raw_mixture.agent * Location.t) list
val compil_of_ast :
  (string * Nbr.t) list ->
  (Ast.agent, Ast.mixture, Mods.StringMap.elt, Ast.rule) Ast.compil ->
  Signature.s * unit NamedDecls.t *
    (Ast.agent, rule_agent list, int, rule) Ast.compil
