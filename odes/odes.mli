  module Make(I:Ode_interface.Interface) :
  sig
    type ode_var_id = int
    type network
    val get_compil: unit -> I.compil
    val convert_var_def:
      'a * (I.pattern, 'b) Ast.ast_alg_expr Location.annot ->
      network ->
      'a * (ode_var_id, 'b) Ast.ast_alg_expr Location.annot
    val convert_initial_state:
    'a * (I.pattern, 'b) Ast.ast_alg_expr Location.annot *
              (I.mixture, 'c) Ast.init_t ->
              network ->
              'a * (ode_var_id, 'b) Ast.ast_alg_expr Location.annot *
              (ode_var_id, 'c) Ast.ast_alg_expr

    val network_from_compil: I.compil -> network
    val species_of_species_id: network -> int -> I.chemical_species
    val get_reactions:
      network ->
      (ode_var_id list * ode_var_id list *
       ((I.connected_component, string) Ast.ast_alg_expr Location.annot *
        ode_var_id  Location.annot) list  * I.rule) list

  end
