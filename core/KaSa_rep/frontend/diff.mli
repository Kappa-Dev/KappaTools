type summary_file 
type summary 
type diff_elt = {new_elt: int list; removed_elt: int list}
type diff = 
 {
    diff_rules: diff_elt ; 
    diff_init: diff_elt ; 
 }

val summarize:  
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Ast.parsing_compil -> 
  Exception_without_parameter.exceptions_caught_and_uncaught
  * summary 
  
val dump_summary: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  summary -> unit 

val is_new_rule: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  summary ->  
  filename:string -> 
  rule:string ->   
  Exception_without_parameter.exceptions_caught_and_uncaught * bool 
 
val is_new_init_state: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  summary ->  
  filename:string -> 
  init_state:string -> 
  Exception_without_parameter.exceptions_caught_and_uncaught * bool 

val diff: 
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  before:summary -> 
  filename:string -> 
  after:summary_file -> 
  Exception_without_parameter.exceptions_caught_and_uncaught * diff

