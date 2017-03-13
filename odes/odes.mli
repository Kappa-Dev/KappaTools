(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Mar 13 2017>
*)
module Make(I:Ode_interface_sig.Interface) :
sig
  type ode_var_id
  type ('a,'b) network
  type enriched_rule
  type rule_id
  type connected_component_id

  val get_compil :
    rate_convention:Remanent_parameters_sig.rate_convention ->
    show_reactions:bool -> count:Ode_args.count ->
    compute_jacobian:bool -> Run_cli_args.t -> I.compil

  val init: I.compil -> (ode_var_id,Ode_loggers_sig.ode_var_id) network

  val network_from_compil:
    ignore_obs:bool ->
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id,Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id,Ode_loggers_sig.ode_var_id) network

  val get_reactions:
    ('a,'b) network ->
    (ode_var_id list * ode_var_id list *
     (('a,'b) Alg_expr.e Locality.annot *
      ode_var_id Locality.annot) list  * I.rule) list

  val export_network:
    command_line:string -> command_line_quotes:string ->
    ?data_file:string -> ?init_t:float -> max_t:float -> ?plot_period:float
    -> ?compute_jacobian:bool ->
    Loggers.t -> Loggers.t -> I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id)  network ->
    unit

  val get_comment: enriched_rule -> string

  val get_rule_id_with_mode: enriched_rule -> rule_id * Rule_modes.arity * Rule_modes.direction

  val get_rule : enriched_rule -> I.rule

  val get_lhs : enriched_rule -> I.pattern

  val get_lhs_cc :
    enriched_rule ->
    (connected_component_id * I.connected_component) list

  val get_divide_rate_by : enriched_rule -> int

(*initial states*)
  (*val translate_species :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    I.chemical_species ->
    I.chemical_species list * ('a, 'b) network ->
    (I.chemical_species list * ('a, 'b) network) * ode_var_id

  val translate_canonic_species :
    I.compil ->
    I.canonic_species ->
    I.chemical_species ->
    I.chemical_species list * ('a, 'b) network ->
    (I.chemical_species list * ('a, 'b) network) * ode_var_id

  val species_of_initial_state :
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    I.init ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network
    * Pattern.cc list*)

  (*val initial_network :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id,Ode_loggers_sig.ode_var_id) network ->
    I.chemical_species list ->
    enriched_rule list ->
    I.chemical_species list *
    (ode_var_id, Ode_loggers_sig.ode_var_id) network*)

  (*  val compute_reactions :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    I.rule list ->
    I.chemical_species list ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val convert_initial_state :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (I.connected_component array list, Ode_loggers_sig.ode_var_id)
      Alg_expr.e * I.rule * Locality.t ->
    ('a, 'b) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
      Locality.annot *
    (('a, 'b) network * ode_var_id list)

  val species_of_species_id:
    ('a,'b) network -> ode_var_id -> I.chemical_species * int*)

  (*rules*)

  val compute_symmetries_from_model:
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    Remanent_state.contact_map ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val print_symmetries:
    Remanent_parameters_sig.parameters ->
    I.compil -> (ode_var_id, Ode_loggers_sig.ode_var_id) network
    -> unit

  val clean_symmetries:
    ('a,'b) network -> ('a,'b) network
end
