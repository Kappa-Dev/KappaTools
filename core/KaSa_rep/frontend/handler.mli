val local_trace : bool

val get_label_of_var_txt :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_variable ->
  Exception_without_parameter.method_handler * string

val get_label_of_var_dot :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_variable ->
  Exception_without_parameter.method_handler * string

val get_label_of_rule_txt :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_rule ->
  Exception_without_parameter.method_handler * (string * Loc.t) option

val get_label_of_rule_dot :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_rule ->
  Exception_without_parameter.method_handler * (string * Loc.t) option

val print_site_contact_map : ('a, 'a, 'a) Ckappa_sig.site_type -> 'a

val print_rule_txt :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Ckappa_sig.c_rule_id ->
  string ->
  string ->
  Ckappa_sig.mixture Ckappa_sig.rule ->
  Exception_without_parameter.method_handler

val print_var_txt :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Ckappa_sig.c_rule_id ->
  string ->
  string ->
  (Ckappa_sig.mixture, string) Alg_expr.e ->
  Exception_without_parameter.method_handler

val print_rule_dot :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  string ->
  string ->
  Ckappa_sig.mixture Ckappa_sig.rule ->
  Exception_without_parameter.method_handler

val print_var_dot :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a ->
  string ->
  string ->
  (Ckappa_sig.mixture, string) Alg_expr.e ->
  Exception_without_parameter.method_handler

val print_rule_or_var :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  (Remanent_parameters_sig.parameters ->
  'a ->
  Ckappa_sig.c_rule_id ->
  string ->
  string ->
  Ckappa_sig.mixture Ckappa_sig.rule ->
  Exception_without_parameter.method_handler) ->
  (Remanent_parameters_sig.parameters ->
  'b ->
  Ckappa_sig.c_rule_id ->
  'c ->
  string ->
  (Ckappa_sig.mixture, string) Alg_expr.e ->
  Exception_without_parameter.method_handler) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_rule ->
  'a * string Loc.annoted option) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.enriched_variable ->
  'b * 'c) ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * bool * unit

val has_a_binding_state :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * bool

val id_of_binding_type :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_state

val state_list :
  Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception_without_parameter.method_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_state list

val last_site_of_agent :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_site_name

val last_state_of_site :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_state

val translate_agent :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Exception_without_parameter.method_handler * string

val string_of_agent :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Exception_without_parameter.method_handler * string

val is_counter :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * bool

val is_internal_site :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * bool

val is_binding_site :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * bool

val print_labels :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.Labels.label_set_couple ->
  Exception_without_parameter.method_handler

val string_of_site :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  ?state:Ckappa_sig.c_state ->
  ?add_parentheses:bool ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * string

val string_of_site_in_file_name :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * string

val string_of_site_update_views :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * string

val string_of_site_contact_map :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * string

val string_of_site_in_natural_language :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler * string

val string_of_state :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_state ->
  Exception_without_parameter.method_handler * string

val string_of_state_fully_deciphered :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_state ->
  Exception_without_parameter.method_handler * string

val string_of_rule :
  ?with_rule:bool ->
  ?with_rule_name:bool ->
  ?with_rule_id:bool ->
  ?with_loc:bool ->
  ?with_ast:bool ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * string

val string_of_rule_or_var :
  ?with_rule:bool ->
  ?with_rule_name:bool ->
  ?with_rule_id:bool ->
  ?with_loc:bool ->
  ?with_ast:bool ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * string

val convert_id_refined :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler
  * (Public_data.rule, Public_data.var) Public_data.influence_node

val convert_id_short :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler
  * (int, int) Public_data.influence_node

val pos_of_var :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * Loc.t

val pos_of_rule :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * Loc.t

val hide : Public_data.rule -> Public_data.rule

val info_of_agent :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Cckappa_sig.compil ->
  Quark_type.agent_quark ->
  Exception_without_parameter.method_handler
  * (string * Loc.t list * Quark_type.agent_quark)

val info_of_rule :
  Remanent_parameters_sig.parameters ->
  ?with_rates:bool ->
  ?original:bool ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler
  * (string
    * Loc.t
    * Public_data.rule_direction
    * string
    * Ckappa_sig.c_rule_id)

val has_no_label :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * bool

val is_reverse :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.compil ->
  Ckappa_sig.c_rule_id ->
  Exception_without_parameter.method_handler * bool

val complementary_interface :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name list ->
  Exception_without_parameter.method_handler * Ckappa_sig.c_site_name list

val dual :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_state ->
  Exception_without_parameter.method_handler
  * (Quark_type.agent_quark * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
    option

val translate_state :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_state ->
  Exception_without_parameter.method_handler
  * Ckappa_sig.Dictionary_of_States.value

val translate_site :
  ?ml_pos:(string * int * int * int) option ->
  ?ka_pos:Loc.t option ->
  ?message:string ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark ->
  Ckappa_sig.c_site_name ->
  Exception_without_parameter.method_handler
  * Ckappa_sig.Dictionary_of_sites.value

val nagents :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  Quark_type.agent_quark

val nvars :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  int

val nrules :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Cckappa_sig.kappa_handler ->
  int
