(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Jul 29 2016>
*)

module Make(I:Ode_interface.Interface) :
sig
  type ode_var_id
  type network
  val get_compil: string list -> I.compil

  val network_from_compil: I.compil -> network

  val get_reactions:
    network ->
    (ode_var_id list * ode_var_id list *
     ((I.pattern, string) Ast.ast_alg_expr Location.annot *
      ode_var_id  Location.annot) list  * I.rule) list

  val export_network:
    command_line:string ->
    command_line_quotes:string ->
    data_file:string ->
    init_t:float ->
    max_t:float ->
    nb_points:int -> Loggers.t -> I.compil -> network -> unit

  val species_of_species_id: network -> ode_var_id -> I.chemical_species
end
