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

    type species_id = int
    let fst_species_id = 0
    let next_species_id id = id + 1
    type network =
      {
        species : SpeciesSet.t ;
        species_tab: I.chemical_species Mods.DynArray.t ;
        id_of_species: species_id SpeciesMap.t ;
        reactions: (species_id list * species_id list * I.rule) list ;
        fresh_species_id: species_id ;
      }

    let fold_left_swap f a b =
      List.fold_left
        (fun a b -> f b a)
        b a

    let init () =
      {
        species = SpeciesSet.empty ;
        id_of_species = SpeciesMap.empty ;
        species_tab = Mods.DynArray.create 0 I.dummy_chemical_species ;
        reactions = [] ;
        fresh_species_id = fst_species_id
      }

    let is_known_species species network =
      SpeciesSet.mem species network.species

    let add_new_species species network =
      let () = Mods.DynArray.set network.species_tab network.fresh_species_id species in
      { network
        with
          species = SpeciesSet.add species network.species ;
          id_of_species = SpeciesMap.add species network.fresh_species_id network.id_of_species ;
          fresh_species_id = next_species_id network.fresh_species_id
      },
      network.fresh_species_id

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

    let translate_canonic_species species remanent =
      let id_opt = SpeciesMap.find_option species (snd remanent).id_of_species in
      match
        id_opt
      with
      | None ->
        let to_be_visited, network = remanent in
        let network, id = add_new_species species network in
        (species::to_be_visited,
         network), id
      | Some i -> remanent,i

    let translate_species species remanent =
      translate_canonic_species (I.canonic_form species) remanent

    let petrify_canonic_species = translate_canonic_species
    let petrify_species species =
      translate_canonic_species (I.canonic_form species)
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
      let remanent, products = petrify_mixture products remanent in
      let to_be_visited, network = remanent in
      let network =
        {
          network
          with reactions = (List.rev reactants, List.rev products, rule)::network.reactions
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
              (fun (store_old_embeddings, to_be_visited, network)  (rule,lhs,lhs_cc)->
                 let rule_id = I.rule_id rule in
                 (* regular application of tules, we store the embeddings*)                 let store_new_embeddings =
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

      let species_of_species_id network =
        (fun i -> Mods.DynArray.get network.species_tab i)
      let get_reactions network = network.reactions

  end



let dummy () = ()
