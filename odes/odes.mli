(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Jul 25 2016>
*)

module Make(I:Ode_interface.Interface) :
sig
  type ode_var_id
  type network
  val get_input_files: unit -> string list
  val get_compil: string list -> I.compil
  val get_m_output_file: I.compil -> string

  val network_from_compil: I.compil -> network

  val get_reactions:
    network ->
    (ode_var_id list * ode_var_id list *
     ((I.pattern, string) Ast.ast_alg_expr Location.annot *
      ode_var_id  Location.annot) list  * I.rule) list

  val export_network: Loggers.t -> I.compil -> network -> unit

  val species_of_species_id: network -> ode_var_id -> I.chemical_species
end
