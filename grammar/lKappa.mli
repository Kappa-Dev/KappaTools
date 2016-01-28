(** Intermediate representation of model on wich sanity has been checked *)

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
(** A representation of 'left-hand-side' agent that stores how
 everything is transformed. In an observable (a mixture in an alg_expr),
 everything is [Maintained] (represented by [I_VAL_CHANGED (i,i)] for
 internal states).

The field ra_syntax represents how the user describe the agent
before compilation. Therefore, [compil_of_ast] in this module
generates rule_agent where ra_syntax is [Some (Array.copy ra_ports,
Array.copy ra_ints)]. *)

type rule_mixture = rule_agent list

val to_erased : rule_mixture -> rule_mixture
val to_maintained : rule_mixture -> rule_mixture
val to_raw_mixture : Signature.s -> rule_mixture -> Raw_mixture.t
val copy_rule_agent : rule_agent -> rule_agent
val print_rule_mixture :
  Signature.s -> Format.formatter -> rule_agent list -> unit

type rule =
  { r_mix: rule_mixture;
    r_created: Raw_mixture.t;
    r_rm_tokens :
      ((rule_mixture,int) Ast.ast_alg_expr Location.annot * int) list;
    r_add_tokens :
      ((rule_mixture,int) Ast.ast_alg_expr Location.annot * int) list;
    r_rate : (rule_mixture,int) Ast.ast_alg_expr Location.annot;
    r_un_rate : ((rule_mixture,int) Ast.ast_alg_expr Location.annot
		 * int Location.annot option) option;
  }

val print_rule :
  ltypes:bool -> rates:bool -> Signature.s ->
  (Format.formatter -> int -> unit) -> (Format.formatter -> int -> unit) ->
  Format.formatter -> rule -> unit

val compil_of_ast :
  (string * Nbr.t) list ->
  (Ast.agent, Ast.mixture, string, Ast.rule) Ast.compil ->
  Signature.s * unit NamedDecls.t *
    (Ast.agent, rule_agent list, int, rule) Ast.compil
(** [compil_of_ast variable_overwrite ast]

@return the signature of agent, the signature of tokens and an
Ast.compil where identifiers are integers and not string, syntactic
sugar on rules are expansed (syntaxtic sugar on mixture are not)

This function sorts out longest prefix convention as well as ensure a
lot of sanity on mixtures:
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice

The sanity checks on rates consists in ensuring that
- either absolute or unary rates are provided;
- if the algebraic expression of the rate contains a mixture then a new variable
 is declared called rulelabel_un_rate; it is necessary in the update phase.*)
