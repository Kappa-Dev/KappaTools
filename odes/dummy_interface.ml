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
  type direction = Direct | Reverse
  type arity = Usual | Unary
  type rule_mode = direction * arity
  type rule_id_with_mode = rule_id * rule_mode

  let add x y list  =
    match y with
    | None -> list
    | Some _ -> x::list

  let valid_modes rule =
    let rule = fst (snd rule) in
    (Direct,Usual)::
    (add (Direct,Unary) rule.Ast.k_un
      (add (Reverse,Usual) rule.Ast.k_op
         (add (Reverse,Unary) rule.Ast.k_op_un [])))

  let lift extended_rate =
    match extended_rate
    with
    | None -> None
    | Some (a,_) -> Some a

  let rate rule mode =
    let rule = fst (snd rule) in
    match
      mode
    with
    | Direct,Usual -> Some rule.Ast.k_def
    | Direct,Unary -> lift rule.Ast.k_un
    | Reverse,Usual -> rule.Ast.k_op
    | Reverse,Unary -> lift rule.Ast.k_op_un

  let lhs (_,(a,_)) (dir,_) =
    match dir with
    | Direct -> a.Ast.lhs
    | Reverse -> a.Ast.rhs

  let token_vector (_,(a,_)) (dir,_) =
    let add,remove  =
      match dir with
      | Direct -> a.Ast.add_token,a.Ast.rm_token
      | Reverse -> a.Ast.rm_token,a.Ast.add_token
    in
    List.fold_left
      (fun token_vector (a,b) ->
         (Location.dummy_annot (Ast.UN_ALG_OP(Operator.UMINUS,a)),b)::token_vector)
      add remove


  let print_rule_id log = Format.fprintf log "%i"


  let apply _ _ _ _ = []
  let lift_species _ = []


  type compil = (Ast.agent, Ast.mixture, string, Ast.rule) Ast.compil
  let get_compil files  =
    List.fold_left
      (KappaLexer.compile Format.std_formatter) Ast.empty_compil
      files
  let get_rules compil = compil.Ast.rules
  let get_initial_state compil = compil.Ast.init
  let get_variables compil = compil.Ast.variables

  let get_t_init _compil = Some 0.
  let get_t_end _compil = Some 1.
  let get_n_points _compil = Some 1000
  let get_files () = ["essai.ka"]
  let get_m_output_file _compil = "ode.m"
  let get_data_output_file _compil = "data.out"
end
