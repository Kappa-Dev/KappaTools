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
  type rule_id

  val binary_rule_that_can_be_applied_in_a_unary_context: rule -> bool
  val lhs: rule -> pattern
  val token_vector:
    rule -> ((connected_component,string) Ast.ast_alg_expr Location.annot *  string Location.annot) list
  val print_rule_id: Format.formatter -> rule_id -> unit
  val rule_id: rule -> rule_id

  val apply: rule -> embedding_forest -> mixture  -> mixture
  val lift_species: chemical_species -> mixture

end


module Make(I:Interface) =
struct

  module SpeciesSetMap =
    SetMap.Make
      (struct
        type t = I.chemical_species
        let compare = compare
        let print = I.print_chemical_species
      end)
  module SpeciesSet = SpeciesSetMap.Set
  module SpeciesMap = SpeciesSetMap.Map


  module Store =
    SetMap.Make
      (struct
        type t = I.rule_id * I.connected_component_id
        let compare = compare
        let print a (r,cc) =
          let () = Format.fprintf a "Component_wise:(%a,%a)" I.print_rule_id r I.print_connected_component_id cc  in
          let () = I.print_rule_id a r in
          let () = I.print_connected_component_id a cc in
          ()
      end)

  module StoreMap = Store.Map

  type id = int
  type var_id = id
  type intro_coef_id = id
  type rule_coef_id = id
  type decl_id = id
  let fst_id = 0
  let next_id id = id + 1

  type ode_var = Nembed of I.canonic_species | Token of string | Dummy
  type lhs_decl = Init_decl | Var_decl of string | Init_value of ode_var

  module VarSetMap =
    SetMap.Make
      (struct
        type t = ode_var
        let compare = compare
        let print log x =
          match x with
          | Nembed x -> I.print_canonic_species log x
          | Token x -> Format.fprintf log "%s" x
          | Dummy -> ()
      end)
  module VarSet = VarSetMap.Set
  module VarMap = VarSetMap.Map

  type network =
    {
      variables : VarSet.t ;
      species_tab: I.chemical_species Mods.DynArray.t ;
      vars_tab: ode_var Mods.DynArray.t ;
      id_of_var: id VarMap.t ;
      reactions: (id list * id list * ((I.connected_component,string) Ast.ast_alg_expr Location.annot * id Location.annot) list * I.rule) list ;
      fresh_var_id: id ;
      declarations: (ode_var * (I.chemical_species,string) Ast.ast_alg_expr Location.annot) Mods.DynArray.t ;
      fresh_rule_coef_id: id;
    }

  let fold_left_swap f a b =
    List.fold_left
      (fun a b -> f b a)
      b a

  let init () =
    {
      reactions = [] ;
      variables = VarSet.empty ;
      vars_tab = Mods.DynArray.create 0 Dummy ;
      id_of_var = VarMap.empty ;
      species_tab = Mods.DynArray.create 0 I.dummy_chemical_species ;
      fresh_var_id = fst_id ;
      fresh_rule_coef_id = fst_id ;
      declarations = Mods.DynArray.create 0 (Dummy, Location.dummy_annot (Ast.CONST Nbr.zero)) ;
    }

  let is_known_variable variable network =
    VarSet.mem variable network.variables

  let add_new_var var network =
    let () = Mods.DynArray.set network.vars_tab network.fresh_var_id var in
    { network
      with
        variables = VarSet.add var network.variables ;
        id_of_var = VarMap.add var network.fresh_var_id network.id_of_var ;
        fresh_var_id = next_id network.fresh_var_id
    }, network.fresh_var_id

  let add_new_canonic_species canonic species network =
    let () = Mods.DynArray.set network.species_tab network.fresh_var_id species in
    add_new_var (Nembed canonic) network

  let add_new_token token network =
    add_new_var (Token token) network

  let enrich_rule rule =
    let lhs = I.lhs rule in
    let lhs_cc = I.connected_components_of_patterns lhs in
    (rule,lhs,lhs_cc)

  let add_embedding key embed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    StoreMap.add key (embed::old_list) store

  let add_embedding_list key lembed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    let new_list =
      fold_left_swap (fun a b -> a::b)
        lembed
        old_list
    in
    StoreMap.add key new_list store

  let translate_canonic_species canonic species remanent =
    let id_opt = VarMap.find_option (Nembed canonic) (snd remanent).id_of_var in
    match
      id_opt
    with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_canonic_species canonic species network in
      (species::to_be_visited,
       network), id
    | Some i -> remanent,i

  let translate_species species remanent =
    translate_canonic_species (I.canonic_form species) species remanent

  let translate_token token remanent =
    let id_opt = VarMap.find_option (Token token) (snd remanent).id_of_var in
    match id_opt with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_token token network in
      (to_be_visited, network), id
    | Some i -> remanent, i

  (*  let petrify_canonic_species = translate_canonic_species*)
  let petrify_species species =
    translate_canonic_species (I.canonic_form species) species
  let petrify_species_list l remanent =
    fold_left_swap
      (fun species (remanent,l) ->
         let remanent, i =
           petrify_species species remanent
         in
         remanent,(i::l))
      l
      (remanent,[])

  let petrify_mixture mixture =
    petrify_species_list (I.connected_components_of_mixture mixture)

  let add_to_prefix_list connected_component key prefix_list store acc =
    let list_embeddings =
      StoreMap.find_default [] key store
    in
    List.fold_left
      (fun new_list prefix ->
         List.fold_left
           (fun new_list (embedding,chemical_species) ->
              ((connected_component,embedding,chemical_species)::prefix)::new_list)
           new_list
           list_embeddings
      )
      acc prefix_list

  let add_reaction rule embedding_forest mixture remanent =
    let remanent, reactants = petrify_mixture mixture remanent in
    let products = I.apply rule embedding_forest mixture in
    let tokens = I.token_vector rule in
    let remanent, products = petrify_mixture products remanent in
    let remanent, tokens =
      List.fold_left
        (fun (remanent, tokens) (a,(b,c)) ->
           let remanent, id = translate_token b remanent in
           remanent,(a,(id,c))::tokens)
        (remanent,[])
        tokens
    in
    let to_be_visited, network = remanent in
    let network =
      {
        network
        with reactions = (List.rev reactants, List.rev products, List.rev tokens, rule)::network.reactions
      }
    in
    to_be_visited, network

  let initial_network initial_states =
    List.fold_left
      (fun remanent species -> fst (translate_species species remanent))
      ([], init ())
      initial_states

  let compute_reactions rules initial_states =
    (* Let us annotate the rules with cc decomposition *)
    let rules = List.rev_map enrich_rule (List.rev rules) in
    let to_be_visited, network = initial_network initial_states in
    let store = StoreMap.empty in
    (* store maps each cc in the lhs of a rule to the list of embedding between this cc and a pattern in set\to_be_visited *)
    let rec aux to_be_visited network store =
      match
        to_be_visited
      with
      | []   -> network
      | new_species::to_be_visited ->
        (* add in store the embeddings from cc of lhs to new_species,
           for unary application of binary rule, the dictionary of species is updated, and the reaction entered directly *)
        let store, to_be_visited, network  =
          List.fold_left
            (fun
              (store_old_embeddings, to_be_visited, network)  (rule,lhs,lhs_cc)->
              let rule_id = I.rule_id rule in
              (* regular application of tules, we store the embeddings*)
              let store_new_embeddings =
                List.fold_left
                  (fun store (cc_id, cc) ->
                     let lembed = I.find_embeddings cc new_species in
                     add_embedding_list
                       (rule_id,cc_id)
                       (List.rev_map (fun a -> a,new_species) (List.rev lembed))
                       store
                  )
                  StoreMap.empty
                  lhs_cc
              in
              let (),store_all_embeddings =
                StoreMap.map2_with_logs
                  (fun _ a _ _ _ -> a)
                  ()
                  ()
                  (fun _ _ b -> (),b)
                  (fun _ _ b -> (),b)
                  (fun _ _ b c ->
                     (),List.fold_left
                       (fun list elt -> elt::list)
                       b c)
                  store_old_embeddings
                  store_new_embeddings
              in
              (* compute the embedding betwen lhs and tuple of species that contain at least one occurence of new_species *)
              let _,new_embedding_list =
                List.fold_left
                  (fun (partial_emb_list,partial_emb_list_with_new_species) (cc_id,cc) ->
                     (* First case, we complete with an embedding towards the new_species *)
                     let partial_emb_list =
                       add_to_prefix_list cc (rule_id,cc_id) partial_emb_list store_old_embeddings []
                     in
                     let partial_emb_list_with_new_species =
                       add_to_prefix_list cc (rule_id,cc_id)
                         partial_emb_list
                         store_new_embeddings
                         (add_to_prefix_list cc (rule_id,cc_id) partial_emb_list_with_new_species
                            store_all_embeddings [])
                     in
                     partial_emb_list, partial_emb_list_with_new_species
                  )
                  ([[]],[[]])
                  lhs_cc
              in
              (* compute the corresponding rhs, and put the new species in the working list, and store the corrsponding reactions *)
              let to_be_visited, network =
                List.fold_left
                  (fun remanent list ->
                     let _,embed,mixture = I.disjoint_union list in
                     add_reaction rule embed mixture remanent)
                  (to_be_visited,network)
                  new_embedding_list
              in
              (* unary application of binary rules *)
              let to_be_visited, network =
                if I.binary_rule_that_can_be_applied_in_a_unary_context rule
                then
                  begin
                    let lembed = I.find_embeddings_unary_binary lhs new_species in
                    fold_left_swap
                      (fun embed ->
                         add_reaction rule embed
                           (I.lift_species new_species))
                      lembed
                      (to_be_visited, network)
                  end
                else
                  to_be_visited, network
              in
              store_all_embeddings, to_be_visited, network
            )
            (store, to_be_visited, network)
            rules
        in
        aux to_be_visited network store
    in
    aux to_be_visited network store

  let translate_species species network =
    snd (translate_species species ([],network))

  let convert_cc connected_component network =
    VarMap.fold
      (fun vars id alg ->
         match vars with
         | Nembed _ ->
           begin
             let species = Mods.DynArray.get network.species_tab id in
             let n_embs =
               List.length
                 (I.find_embeddings connected_component species)
             in
             if n_embs = 0
             then
               alg
             else
               let species = Ast.KAPPA_INSTANCE id in
               let term =
                 if n_embs = 1
                 then
                   species
                 else
                   Ast.BIN_ALG_OP
                     (
                       Operator.MULT,
                       Location.dummy_annot (Ast.CONST (Nbr.I n_embs)),
                       Location.dummy_annot species)
               in
               if alg = Ast.CONST (Nbr.I 0) then term
               else
                 Ast.BIN_ALG_OP
                   (
                     Operator.SUM,
                     Location.dummy_annot alg,
                     Location.dummy_annot term)
           end
         | Token _ | Dummy -> alg

      )
      network.id_of_var
      ((Ast.CONST (Nbr.I 0)))

  let rec convert_alg_expr alg network =
    match
      alg
    with
    | Ast.BIN_ALG_OP (op, arg1, arg2 ),loc ->
      Ast.BIN_ALG_OP (op, convert_alg_expr arg1 network, convert_alg_expr arg2 network),loc
    | Ast.UN_ALG_OP (op, arg),loc ->
      Ast.UN_ALG_OP (op, convert_alg_expr arg network),loc
    | Ast.KAPPA_INSTANCE connected_component, loc ->
      convert_cc connected_component network, loc
    | Ast.TOKEN_ID a, loc -> Ast.TOKEN_ID a, loc
    | Ast.OBS_VAR a, loc -> Ast.OBS_VAR a, loc
    | Ast.CONST a , loc -> Ast.CONST a, loc
    | Ast.STATE_ALG_OP op,loc ->
      Ast.STATE_ALG_OP op,loc

  let convert_var_def variable_def network =
    let a,b = variable_def in
    a,convert_alg_expr b network

  let convert_initial_state intro network =
    let a,b,c = intro in
    a,
    convert_alg_expr b network,
    match
      c
    with
    | Ast.INIT_MIX m ->
      begin
        let cc = I.connected_components_of_mixture m in
        let list =
          List.rev_map
            (fun x -> translate_species x network,
                      I.nbr_automorphisms_in_chemical_species x)
            cc
        in
        match
          list
        with
        | [] ->
          Ast.CONST (Nbr.zero)
        | h::t ->
          let term (singleton, auto) =
            let species = Ast.KAPPA_INSTANCE singleton in
            if auto = 1
            then
              species
            else
              Ast.BIN_ALG_OP
                (Operator.MULT,
                 Location.dummy_annot (Ast.CONST (Nbr.I auto)),
                 Location.dummy_annot species) in
          let rec aux tail expr =
            match tail
            with
            | [] -> expr
            | h::t ->
              aux t
                (Ast.BIN_ALG_OP
                   (Operator.SUM,
                    Location.dummy_annot expr,
                    Location.dummy_annot (term h)))
          in aux t (term h)

      end
    | Ast.INIT_TOK token ->
      Ast.TOKEN_ID token





  let species_of_species_id network =
    (fun i -> Mods.DynArray.get network.species_tab i)
  let get_reactions network = network.reactions


end



let dummy () = ()
let _ = Ode_loggers.initialize
