(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Dec 19 2018>
*)
module Make (I : Symmetry_interface_sig.Interface) : sig
  type ode_var_id
  type ('a, 'b) network
  type enriched_rule
  type rule_id
  type connected_component_id

  val get_preprocessed_ast : Run_cli_args.t -> I.preprocessed_ast
  val get_ast : Run_cli_args.t -> I.ast
  val to_ast : I.ast -> Ast.parsing_compil
  val preprocess : Run_cli_args.t -> I.ast -> I.preprocessed_ast

  val get_compil :
    debugMode:bool ->
    dotnet:bool ->
    ?bwd_bisim:LKappa_group_action.bwd_bisim_info ->
    rule_rate_convention:Remanent_parameters_sig.rate_convention ->
    ?reaction_rate_convention:Remanent_parameters_sig.rate_convention ->
    show_reactions:bool ->
    count:Ode_args.count ->
    internal_meaning:Ode_args.count ->
    compute_jacobian:bool ->
    Run_cli_args.t ->
    I.preprocessed_ast ->
    I.compil

  val init : I.compil -> (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val reset :
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val network_from_compil :
    ?max_size:int ->
    smash_reactions:bool ->
    ignore_obs:bool ->
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val get_reactions :
    ('a, 'b) network ->
    ((ode_var_id list
     * ode_var_id list
     * ode_var_id Locality.annot list
     * I.rule)
    * int)
    list

  val export_network :
    command_line:string ->
    command_line_quotes:string ->
    ?data_file:string ->
    ?init_t:float ->
    max_t:float ->
    ?plot_period:float ->
    ?compute_jacobian:bool ->
    ?propagate_constants:bool ->
    ?show_time_advance:bool ->
    ?nonnegative:bool ->
    ?initial_step:float ->
    ?max_step:float ->
    ?abstol:float ->
    ?reltol:float ->
    Remanent_parameters_sig.parameters ->
    Ode_loggers_sig.t ->
    Ode_loggers_sig.t ->
    Loggers.t ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val get_comment : enriched_rule -> string

  val get_rule_id_with_mode :
    enriched_rule -> rule_id * Rule_modes.arity * Rule_modes.direction

  val get_rule : enriched_rule -> I.rule
  val get_lhs : enriched_rule -> I.pattern

  val get_lhs_cc :
    enriched_rule -> (connected_component_id * I.connected_component) list

  val get_divide_rate_by : enriched_rule -> int

  (*rules*)

  val compute_symmetries_from_model :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    Public_data.contact_map ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val set_to_backward_symmetries_from_model :
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val set_to_forward_symmetries_from_model :
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network

  val print_symmetries :
    Remanent_parameters_sig.parameters ->
    I.compil ->
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    unit

  val get_data :
    (ode_var_id, Ode_loggers_sig.ode_var_id) network -> int * int * int

  val init_bwd_bisim_info :
    (ode_var_id, Ode_loggers_sig.ode_var_id) network ->
    LKappa_group_action.bwd_bisim_info option
end
