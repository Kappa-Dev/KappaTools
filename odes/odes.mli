module type Interface =
sig
  type mixture              (* not necessarily connected, fully specified *)
  type chemical_species     (* connected, fully specified *)
  type pattern              (* not necessarity connected, maybe partially specified *)
  type connected_component  (* connected, maybe partially specified *)

  val dummy_chemical_species: chemical_species
  val print_chemical_species: Format.formatter -> chemical_species -> unit

  type connected_component_id
  val print_connected_component_id: Format.formatter -> connected_component_id -> unit

  val nbr_automorphisms_in_chemical_species: chemical_species -> int
  val nbr_automorphisms_in_pattern: pattern -> int

  val canonic_form: chemical_species -> chemical_species

  val connected_components_of_patterns:
    pattern -> (connected_component_id * connected_component) list

  val connected_components_of_mixture:
    mixture -> chemical_species list

  type embedding (* the domain is connected *)
  type embedding_forest (* the domain may be not connected *)
  val lift_embedding: embedding -> embedding_forest
  val find_embeddings: connected_component -> chemical_species -> embedding list
  val find_embeddings_unary_binary:
    pattern -> chemical_species -> embedding_forest list
  val disjoint_union:
    (connected_component * embedding * chemical_species) list ->
    pattern * embedding_forest * mixture

  type rule
  type rule_id

  val binary_rule_that_can_be_applied_in_a_unary_context: rule -> bool
  val lhs: rule -> pattern

  val print_rule_id: Format.formatter -> rule_id -> unit
  val rule_id: rule -> rule_id

  val apply: rule -> embedding_forest -> mixture  -> mixture
  val lift_species: chemical_species -> mixture

end


  module Make(I:Interface) :
  sig
    type species_id = int
    type network

    val compute_reactions: I.rule list -> I.chemical_species list -> network
    val convert_var_def:
      'a * (I.connected_component, 'b) Ast.ast_alg_expr Location.annot ->
      network ->
      'a * (species_id, 'b) Ast.ast_alg_expr Location.annot
    val convert_initial_state:
    'a * (I.connected_component, 'b) Ast.ast_alg_expr Location.annot * (I.mixture, 'c) Ast.init_t ->
    network ->
    'a * (species_id, 'b) Ast.ast_alg_expr Location.annot * (species_id, 'd) Ast.ast_alg_expr
    val species_of_species_id: network -> int -> I.chemical_species
    val get_reactions: network -> (species_id list * species_id list * I.rule) list
end

val dummy: unit -> unit
