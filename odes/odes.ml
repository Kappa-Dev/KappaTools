module type Interface =
sig
  type pattern
  type canonic_pattern
  val print_canonic_pattern: Format.formatter -> canonic_pattern -> unit

  type connected_component_id
  val print_connected_component_id: Format.formatter -> connected_component_id -> unit

  val nbr_automorphisms: pattern -> int
  val canonic_form: pattern -> canonic_pattern
  val connected_components:
    pattern -> (connected_component_id * pattern) list

  type embedding

  val find_embeddings: pattern -> pattern -> embedding list
  val disjoint_union:
    (pattern * embedding * pattern) list ->
    pattern * embedding * pattern

  type rule

  val binary_rule_that_can_be_applied_in_a_unary_context: rule -> bool
  val lhs: rule -> pattern
  type rule_id
  val print_rule_id: Format.formatter -> rule_id -> unit
  val rule_id: rule -> rule_id

  val apply: rule -> embedding -> pattern -> pattern


end


module Make(I:Interface) =
struct

  module PatternSetMap =
    SetMap.Make
      (struct
        type t = I.canonic_pattern
        let compare = compare
        let print = I.print_canonic_pattern
      end)
  module PatternSet = PatternSetMap.Set

  type store_key =
    | Component_wise of I.rule_id * I.connected_component_id
    | Global of I.rule_id

  module Store =
    SetMap.Make
      (struct
        type t = store_key
        let compare = compare
        let print a b =
          match
            b
          with
          | Component_wise (r,cc) ->
            let () = Format.fprintf a "Component_wise:(%a,%a)" I.print_rule_id r I.print_connected_component_id cc  in
            let () = I.print_rule_id a r in
            let () = I.print_connected_component_id a cc in
            ()
          | Global r ->
            Format.fprintf a "Global:%a"
              I.print_rule_id r
      end)
  module StoreMap = Store.Map

  let enrich_rule rule =
    let lhs = I.lhs rule in
    let lhs_cc = I.connected_components lhs in
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
      List.fold_left
        (fun list embed -> embed::list)
        old_list
        lembed
    in
    StoreMap.add key new_list store

  let check_if_a_pattern_is_new pattern (to_be_visited,set) =
    let canonic = I.canonic_form pattern in
    if PatternSet.mem canonic set
    then
      to_be_visited,set
    else
      pattern::to_be_visited,
      PatternSet.add canonic set

  let compute_reactions rules initial_states =
    (* Let us annotate the rules with cc decomposition *)
    let rules =
      List.rev_map
        enrich_rule
        (List.rev rules)
    in
    let set = PatternSet.empty in
    let to_be_visited = [] in
    let to_be_visited,set =
      List.fold_left
        (fun remanent pattern ->
           check_if_a_pattern_is_new pattern remanent)
        (to_be_visited,set)
        initial_states
    in
    let store = StoreMap.empty in
    (* set is the set of pattern in cannonic forms *)
    (* store maps each cc in the lhs of a rule to the list of embedding between this cc and a pattern in set\to_be_visited, and for unary applciation of rules, the list of the embedding between the whole lhs and a pattern in set\to_be_visited*)
    let rec aux to_be_visited set store =
      match
        to_be_visited
      with
      | []   -> set,store
      | new_species::to_be_visited ->
        (* add in store the embeddings from cc of lhs to new_species
           as well as the embeddings from lhs to a new species for unary application of a binary application *)
        let store' =
          List.fold_left
            (fun store (rule,lhs,lhs_cc)->
               let rule_id = I.rule_id rule in
               let store =
                 List.fold_left
                   (fun store (cc_id, cc) ->
                      let lembed = I.find_embeddings cc new_species in
                           add_embedding_list
                             (Component_wise (rule_id,cc_id))
                             lembed
                             store
                   )
                   store
                   lhs_cc
               in
               let store =
                 if I.binary_rule_that_can_be_applied_in_a_unary_context rule
                 then
                   begin
                     let lembed = I.find_embeddings lhs new_species in
                     add_embedding_list (Global rule_id) lembed store
                   end
                 else
                   store
               in
               store
            )
            store
            rules
        in
        (* compute the list of embeeding from lhs to new_species
           for unary application of binary rules *)
        let new_embedding_list =
          List.fold_left
            (fun embedding_list (rule,lhs,_lhs_cc) ->
            if I.binary_rule_that_can_be_applied_in_a_unary_context rule
            then
              begin
                let lembed = I.find_embeddings lhs new_species in
                List.fold_left
                  (fun list embed -> (rule,embed,new_species)::list)
                  embedding_list
                  lembed
              end
            else
              embedding_list
            )
            []
            rules
        in
        (* compute the embedding betwen lhs and tuple of species that contain
           at least one occurence of new_species *)
        let new_embedding_list = (* TODO *)
          new_embedding_list
        in
        (* compute the corresponding rhs, and put the new species in the working list *)
        let to_be_visited,set =
          List.fold_left
            (fun working_list  (rule,embed,mixture) ->
               let refined_rhs = I.apply rule embed mixture in
               let cc_rhs = I.connected_components refined_rhs in
               List.fold_left
                 (fun working_list (_cc_id,pattern) ->
                    check_if_a_pattern_is_new pattern working_list)
                 working_list
                 cc_rhs
            )
            (to_be_visited,set)
            new_embedding_list
        in
        aux to_be_visited set store'
    in
    aux to_be_visited set store

end

let dummy () = ()
