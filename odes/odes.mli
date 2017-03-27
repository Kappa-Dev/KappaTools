(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Mar 27 2017>
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
    ?data_file:string -> ?init_t:float -> max_t:float -> ?plot_period:float ->
    ?compute_jacobian:bool -> ?show_time_advance:bool ->
    ?nonnegative:bool -> ?initial_step:float -> 
    Remanent_parameters_sig.parameters ->
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

  (*rules*)


  val compute_symmetries_from_model:
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    Remanent_state.contact_map ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network
  val set_to_backward_symmetries_from_model:
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val set_to_forward_symmetries_from_model:
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val print_symmetries:
    Remanent_parameters_sig.parameters ->
    I.compil -> (ode_var_id, Ode_loggers_sig.ode_var_id) network
    -> unit

end
