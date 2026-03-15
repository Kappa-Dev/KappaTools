type ('rule, 'init) summary_file 
type ('rule, 'init) summary 
type diff_elt = {new_elt: int list; removed_elt: int list}
type diff = 
 {
    diff_rules: diff_elt ; 
    diff_init: diff_elt ; 
 }

val summarize_from_ast:  
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ast.parsing_compil -> 
  Exception_without_parameter.exceptions_caught_and_uncaught
  * (Ast.rule Ast.compil_rule,
             (Ast.mixture, Ast.mixture, string) Ast.init_statement) summary 
  
val summarize_from_ckappa: 
Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ckappa_sig.c_compil -> 
  Exception_without_parameter.exceptions_caught_and_uncaught
  *   (Ckappa_sig.enriched_rule, Ckappa_sig.enriched_init) summary 

val summarize_from_cckappa: 
Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Cckappa_sig.compil -> 
  Exception_without_parameter.exceptions_caught_and_uncaught
  * (Cckappa_sig.enriched_rule, Cckappa_sig.enriched_init)  summary 


val dump_summary: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a,'b) summary -> unit 

val is_new_rule: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a, 'b) summary ->  
  filename:string -> 
  rule:string ->   
  Exception_without_parameter.exceptions_caught_and_uncaught * bool 
 
val is_new_init_state: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ('a, 'b) summary ->  
  filename:string -> 
  init_state:string -> 
  Exception_without_parameter.exceptions_caught_and_uncaught * bool 

val diff: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  before:('a,'b) summary -> 
  filename:string -> 
  after:('a,'b) summary_file -> 
  Exception_without_parameter.exceptions_caught_and_uncaught * diff

