(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Aug 18 2016>
*)
module Make(I:Ode_interface_sig.Interface) :
sig
  type ode_var_id
  type ('a,'b) network

  val get_compil :
    rate_convention:Ode_args.rate_convention ->
    show_reactions:bool -> count:Ode_args.count ->
    compute_jacobian:bool -> Run_cli_args.t -> I.compil

  val network_from_compil: I.compil -> (int,int) network

  val get_reactions:
    ('a,'b) network ->
    (ode_var_id list * ode_var_id list *
     (('a,'b) Alg_expr.e Location.annot *
      ode_var_id  Location.annot) list  * I.rule) list

  val export_network:
    command_line:string ->
    command_line_quotes:string ->
    data_file:string ->
    init_t:float -> max_t:float -> plot_period:float ->
    Loggers.t -> I.compil -> (int,int) network -> unit

  val species_of_species_id:
    (int,int) network -> ode_var_id -> (I.chemical_species * int)
end
