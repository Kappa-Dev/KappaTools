(** Network/ODE generation
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jul 29 2016>
*)

module type Interface =
sig
  type mixture              (* not necessarily connected, fully specified *)
  type chemical_species     (* connected, fully specified *)
  type canonic_species      (* chemical species in canonic form *)
  type pattern              (* not necessarity connected, maybe partially specified *)
  type connected_component  (* connected, maybe partially specified *)


  val dummy_chemical_species: Signature.s -> chemical_species

  val compare_connected_component :
    connected_component -> connected_component -> int
  val print_connected_component :
    ?sigs:Signature.s -> Format.formatter -> connected_component -> unit

  val print_chemical_species:
    ?sigs:Signature.s -> Format.formatter -> chemical_species -> unit
  val print_canonic_species:
    ?sigs:Signature.s -> Format.formatter -> canonic_species -> unit

  val do_we_divide_rates_by_n_auto_in_lhs: bool
  val nbr_automorphisms_in_chemical_species: chemical_species -> int
  val nbr_automorphisms_in_pattern: pattern -> int

  val canonic_form: chemical_species -> canonic_species

  val connected_components_of_patterns: pattern -> connected_component list

  val connected_components_of_mixture:
    Signature.s -> Primitives.contact_map -> mixture -> chemical_species list

  type embedding (* the domain is connected *)
  type embedding_forest (* the domain may be not connected *)
  val lift_embedding: embedding -> embedding_forest
  val find_embeddings: connected_component -> chemical_species -> embedding list
  val find_embeddings_unary_binary:
    pattern -> chemical_species -> embedding_forest list
  val disjoint_union:
    Signature.s ->
    (connected_component * embedding * chemical_species) list ->
    pattern * embedding_forest * mixture

  type rule
  type arity = Usual | Unary
  type rule_id = int
  type rule_id_with_mode = rule_id * arity

  val valid_modes: rule -> arity list
  val lhs: rule -> pattern
  val token_vector:
    rule ->
    (connected_component array list Alg_expr.e Location.annot * int) list
  val print_rule_id: Format.formatter -> rule_id -> unit
  val rate:
    rule -> arity ->
    connected_component array list Alg_expr.e Location.annot option

  val apply: Signature.s -> rule -> embedding_forest -> mixture  -> mixture
  val lift_species: Signature.s -> chemical_species -> mixture

  val get_compil:
    Common_args.t -> Run_cli_args.t ->
    Environment.t * Primitives.contact_map *
    (connected_component array list Alg_expr.e * rule * Location.t) list
  val get_rules: Environment.t -> rule list
  val get_variables:
    Environment.t ->
    (string Location.annot *
     connected_component array list Alg_expr.e Location.annot) array
  val get_obs: Environment.t -> connected_component array list Alg_expr.e list

  val get_obs_titles: Environment.t -> string list

end
