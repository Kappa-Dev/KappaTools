(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Jul 29 2016>
*)

module Make(I:Ode_interface.Interface) :
sig
  type ode_var_id
  type 'a network

  val get_compil : Common_args.t -> Run_cli_args.t ->
    Environment.t * Primitives.contact_map *
    (I.connected_component array list Alg_expr.e * I.rule * Location.t) list

  val network_from_compil:
    Environment.t -> Primitives.contact_map ->
    (I.connected_component array list Alg_expr.e * I.rule * Location.t) list ->
    int network

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
    nb_points:int -> Loggers.t -> Environment.t -> int network -> unit

  val species_of_species_id: int network -> ode_var_id -> I.chemical_species
end
