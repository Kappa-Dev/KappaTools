(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Aug 18 2016>
*)
module Make(I:Ode_interface_sig.Interface) :
sig
  type ode_var_id
  type 'a network

  val get_compil : rate_convention:Ode_args.rate_convention -> Common_args.t -> Run_cli_args.t -> I.compil

  val network_from_compil:
    I.compil -> int network

  val get_reactions:
    'a network ->
    (ode_var_id list * ode_var_id list *
     ('a Alg_expr.e Location.annot *
      ode_var_id  Location.annot) list  * I.rule) list

  val export_network:
    command_line:string ->
    command_line_quotes:string ->
    data_file:string ->
    init_t:float ->
    max_t:float ->
    nb_points:int -> Loggers.t -> I.compil -> int network -> unit

  val species_of_species_id: int network -> ode_var_id -> I.chemical_species
end
