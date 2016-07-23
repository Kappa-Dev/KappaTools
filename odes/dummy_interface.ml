module Interface =
struct
  type mixture = Ast.mixture          (* not necessarily connected, fully specified *)
  type chemical_species = Ast.mixture   (* connected, fully specified *)
  type canonic_species = int (* chemical species in canonic form *)
  type pattern = Ast.mixture           (* not necessarity connected, maybe partially specified *)
  type connected_component = Ast.mixture (* connected, maybe partially specified *)

  let dummy_chemical_species = []
  let dummy_canonic_species = 0

  let print_chemical_species _ _ = ()
  let print_canonic_species _ _ = ()

  type connected_component_id = int
  let print_connected_component_id log = Format.fprintf log "%i"

  let nbr_automorphisms_in_chemical_species _ = 1
  let nbr_automorphisms_in_pattern _ = 1

  let canonic_form _ = 1

  let connected_components_of_patterns _ =[0,[]]

  let connected_components_of_mixture _ = [[]]

  type embedding = (int * int) list (* the domain is connected *)
  type embedding_forest = embedding (* the domain may be not connected *)
  let lift_embedding x = x
  let find_embeddings _ _ = [[1,1]]

  let find_embeddings_unary_binary _ _ = [[1,1]]

  let disjoint_union _ = [],[1,1],[]

  type rule = (string Location.annot option * Ast.rule Location.annot)
  type rule_id = int

  let binary_rule_that_can_be_applied_in_a_unary_context _ = false
  let lhs _ = []

  let token_vector _ = []

  let print_rule_id log = Format.fprintf log "%i"
  let rule_id _ = 1

  let apply _ _ _ = []
  let lift_species _ = []


  type compil = (Ast.agent, Ast.mixture, string, Ast.rule) Ast.compil
  let get_compil () =
    let files = !FileNames.input in
    List.fold_left
      (KappaLexer.compile Format.std_formatter) Ast.empty_compil
      files
  let get_rules compil = compil.Ast.rules
  let get_initial_state compil = compil.Ast.init
  let get_variables compil = compil.Ast.variables


end
