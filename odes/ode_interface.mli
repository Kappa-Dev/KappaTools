module type Interface =
sig
  type mixture              (* not necessarily connected, fully specified *)
  type chemical_species     (* connected, fully specified *)
  type canonic_species     (* chemical species in canonic form *)
  type pattern              (* not necessarity connected, maybe partially specified *)
  type connected_component  (* connected, maybe partially specified *)


  val dummy_chemical_species: chemical_species
  val dummy_canonic_species: canonic_species


  val print_chemical_species: Format.formatter -> chemical_species -> unit
  val print_canonic_species: Format.formatter -> canonic_species -> unit
  type connected_component_id
  val print_connected_component_id: Format.formatter -> connected_component_id -> unit

  val do_we_divide_rates_by_n_auto_in_lhs: bool 
  val nbr_automorphisms_in_chemical_species: chemical_species -> int
  val nbr_automorphisms_in_pattern: pattern -> int

  val canonic_form: chemical_species -> canonic_species

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
  type direction = Direct | Reverse
  type arity = Usual | Unary
  type rule_mode = direction * arity
  type rule_id = int
  type rule_id_with_mode = rule_id * rule_mode

  val valid_modes: rule -> rule_mode list
  val lhs: rule -> rule_mode -> pattern
  val token_vector:
    rule -> rule_mode -> ((connected_component,string) Ast.ast_alg_expr Location.annot *  string Location.annot) list
  val print_rule_id: Format.formatter -> rule_id -> unit
  val rate: rule -> rule_mode -> (pattern,string) Ast.ast_alg_expr Location.annot option

  val apply: rule -> rule_mode -> embedding_forest -> mixture  -> mixture
  val lift_species: chemical_species -> mixture


  type compil
  val get_compil: string list -> compil
  val get_rules: compil -> rule list
  val get_initial_state: compil ->
    (string Location.annot option *
     (pattern,string) Ast.ast_alg_expr Location.annot *
     (mixture,string) Ast.init_t Location.annot) list
  val get_variables: compil -> (pattern,string) Ast.variable_def list

  val get_t_init: compil -> float option
  val get_t_end: compil -> float option
  val get_n_points: compil -> int option
  val get_files: unit -> string list
  val get_m_output_file: compil -> string
  val get_data_output_file: compil -> string
end
