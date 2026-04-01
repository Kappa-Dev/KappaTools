type ('rule, 'init) summary_file
type ('rule, 'init) summary

type diff_elt = {
  new_elt: int list;
  removed_elt: int list;
  pos_renaming: (Loc.t * Loc.t) list;
}

type diff = { diff_rules: diff_elt; diff_init: diff_elt }

type new_indexs = {
  next_rule: Ckappa_sig.c_rule_id;
  next_init: int;
  next_nsites: Ckappa_sig.c_site_name;
  next_nr_predicates: Ckappa_sig.c_guard_parameter;
  next_agent: Ckappa_sig.c_agent_name;
  former_dual:
    (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .t
    option;
}

val starting_new_elt : new_indexs

val summarize_from_ast :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ast.parsing_compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * ( Ast.rule Ast.compil_rule,
      (Ast.mixture, Ast.mixture, string) Ast.init_statement )
    summary

val summarize_from_ckappa :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.c_compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * (Ckappa_sig.enriched_rule, Ckappa_sig.enriched_init) summary

val summarize_from_cckappa :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * (Cckappa_sig.enriched_rule, Cckappa_sig.enriched_init) summary

val dump_summary :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a, 'b) summary ->
  unit

val is_new_rule :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a, 'b) summary ->
  filename:string ->
  rule:string ->
  Exception_without_parameter.exceptions_caught_and_uncaught * bool

val is_new_init_state :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a, 'b) summary ->
  filename:string ->
  init_state:string ->
  Exception_without_parameter.exceptions_caught_and_uncaught * bool

val diff :
  'rule Loc.diff_pos ->
  'init Loc.diff_pos ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  before:('rule, 'init) summary ->
  filename:string ->
  after:('rule, 'init) summary_file ->
  Exception_without_parameter.exceptions_caught_and_uncaught * diff

val dump_diff :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  diff ->
  Exception_without_parameter.exceptions_caught_and_uncaught

val get_file :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  filename:string ->
  ('rule, 'init) summary ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * ('rule, 'init) summary_file

val renaming_of_diff : diff -> Loc.t -> Loc.t option
val cut : diff -> Ast.parsing_compil -> Ast.parsing_compil

val get_new_indexs :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught * new_indexs

val fuse :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * Cckappa_sig.kappa_handler
  * Cckappa_sig.compil

val update_ast :
  Ast.parsing_compil ->
  Ast.parsing_compil ->
  Ast.parsing_compil * Ast.parsing_compil
