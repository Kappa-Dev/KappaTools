(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Feb 18 2017>
*)
module Make(I:Ode_interface_sig.Interface) :
sig
  type ode_var_id
  type ('a,'b) network
  type enriched_rule
  type rule_id
  type connected_component_id

  val get_compil :
    rate_convention:Ode_args.rate_convention ->
    show_reactions:bool -> count:Ode_args.count ->
    compute_jacobian:bool -> Run_cli_args.t -> I.compil

  val network_from_compil:
    ignore_obs:bool -> I.compil -> (int,int) network

  val get_reactions:
    ('a,'b) network ->
    (ode_var_id list * ode_var_id list *
     (('a,'b) Alg_expr.e Locality.annot *
      ode_var_id  Locality.annot) list  * I.rule) list

  val export_network:
    command_line:string -> command_line_quotes:string ->
    ?data_file:string -> ?init_t:float -> max_t:float -> ?plot_period:float ->
    Loggers.t -> Loggers.t -> I.compil -> (int,int) network -> unit

  val species_of_species_id:
    (int,int) network -> ode_var_id -> I.chemical_species * int

  val get_comment: enriched_rule -> string

  val get_rule_id_with_mode: enriched_rule -> rule_id * I.arity * I.direction

  val get_rule : enriched_rule -> I.rule

  val get_lhs : enriched_rule -> I.pattern

  val get_lhs_cc :
    enriched_rule -> (connected_component_id * I.connected_component) list

  val get_divide_rate_by : enriched_rule -> int

  val compute_symmetries_from_syntactic_rules :
    Loggers.t ->
    I.compil ->
    I.nauto_in_rules_cache * unit

  (*val compute_symmetries_from_syntactic_rules : Loggers.t ->
    I.compil -> I.nauto_in_rules_cache * unit*)

end
