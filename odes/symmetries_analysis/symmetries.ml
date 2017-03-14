(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Mar 14 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(******************************************************************)
(*TYPE*)
(******************************************************************)

type contact_map =
  ((string list) * (string*string) list)
    Mods.StringSetMap.Map.t Mods.StringSetMap.Map.t


type partitioned_contact_map =
  string Symmetries_sig.site_partition Mods.StringSetMap.Map.t

(*internal states * binding states*)
type lkappa_partitioned_contact_map =
  int Symmetries_sig.site_partition array

type symmetries = lkappa_partitioned_contact_map

(***************************************************************)
(*PARTITION THE CONTACT MAP*)
(***************************************************************)

let add_gen add find_option k data map =
  let old =
    match
      find_option k map
    with
    | None -> []
    | Some l -> l
  in
  add k (data::old) map

let partition_gen
    empty add find_option fold sort empty_range cache hash int_of_hash f map =
  let map =
    fst
      (Mods.StringMap.fold
         (fun key data (inverse,cache) ->
            let range = f data in
            if range = empty_range
            then inverse, cache
            else
              let sorted_range = sort range in
              let cache, hash = hash cache sorted_range in
              let inverse =
                add_gen
                  add find_option (int_of_hash hash) key inverse
              in
              inverse, cache
         ) map (empty, cache))
  in
  List.rev (fold (fun _ l sol -> (List.rev l)::sol) map [])

let partition cache hash int_of_hash f map =
  partition_gen
    Mods.IntMap.empty
    Mods.IntMap.add
    Mods.IntMap.find_option
    Mods.IntMap.fold
    (List.sort compare)
    []
    cache hash int_of_hash f map

let partition_pair cache hash int_of_hash f map =
  partition_gen
    Mods.Int2Map.empty
    Mods.Int2Map.add
    Mods.Int2Map.find_option
    Mods.Int2Map.fold
    (fun (a,b) ->
       List.sort compare a,
       List.sort compare b)
    ([],[])
    cache hash int_of_hash f map

module State:  SetMap.OrderedType
  with type t = string
  =
struct
  type t = string
  let compare = compare
  let print f s = Format.fprintf f "%s" s
end

module StateList = Hashed_list.Make (State)

module BindingType: SetMap.OrderedType
  with type t = string * string
 =
struct
  type t = string*string
  let compare = compare
  let print f (s1,s2) = Format.fprintf f "%s.%s" s1 s2
end

module BindingTypeList = Hashed_list.Make (BindingType)

let collect_partitioned_contact_map contact_map =
  Mods.StringMap.map
    (fun sitemap ->
       let cache1 = StateList.init () in
       let cache2 = BindingTypeList.init () in
       let (internal_state_partition: string list list) =
         partition
           cache1
           StateList.hash
           StateList.int_of_hashed_list
           fst
           sitemap
       in
       let (binding_state_partition: string list list) =
         partition
           cache2
           BindingTypeList.hash
           BindingTypeList.int_of_hashed_list
           snd
           sitemap
       in
       let full_state_partition =
         partition_pair
           (cache1,cache2)
           (fun
             (cache1,cache2)
             (l1,l2) ->
              let cache1,a1 = StateList.hash cache1 l1 in
              let cache2,a2 = BindingTypeList.hash cache2 l2 in
              (cache1,cache2),(a1,a2))
           (fun (a,b) ->
              StateList.int_of_hashed_list a,
              BindingTypeList.int_of_hashed_list b)
           (fun x->x)
           sitemap
       in
       {
         Symmetries_sig.over_internal_states = internal_state_partition ;
         Symmetries_sig.over_binding_states = binding_state_partition ;
         Symmetries_sig.over_full_states = full_state_partition ;
       }
    ) contact_map

(*****************************************************************)
(*PRINT*)
(*****************************************************************)

  let print_partitioned_contact_map parameters partitioned_contact_map =
    let log = Remanent_parameters.get_logger parameters in
    Mods.StringMap.iter
      (fun agent partition ->
         Symmetries_sig.print
           log
           (fun agent fmt site ->
              Loggers.fprintf log "%s" site)
           (fun fmt agent ->
              Loggers.fprintf log "%s" agent)
           agent
           partition
      ) partitioned_contact_map

let print_partitioned_contact_map_in_lkappa logger env partitioned_contact_map =
  let signature = Model.signatures env in
  Array.iteri
    (fun agent_id partition ->
       Symmetries_sig.print
         logger
         (Signature.print_site signature)
         (Signature.print_agent signature)
         agent_id
         partition
    ) partitioned_contact_map

let print_contact_map parameters contact_map =
  let log = Remanent_parameters.get_logger parameters in
  Mods.StringMap.iter
    (fun agent sitemap ->
       let () = Loggers.fprintf log "agent:%s\n" agent in
       Mods.StringMap.iter
         (fun site (l1,l2) ->
         let () = Loggers.fprintf log "  site:%s\n" site in
         let () =
           if l1 <> []
           then
             let () = Loggers.fprintf log "internal_states:" in
             let () = List.iter (Loggers.fprintf log "%s;") l1 in
             let () = Loggers.print_newline log in ()
         in
         let () =
           if l2 <> []
           then
             let () = Loggers.fprintf log "binding_states:" in
             let () =
               List.iter (fun (s1,s2) ->
                   Loggers.fprintf log "%s.%s;" s1 s2) l2
             in
             let () = Loggers.print_newline log in ()
         in ()) sitemap) contact_map

(****************************************************************)

let translate_list l agent_interface =
  List.rev_map
    (fun equ_class ->
       List.rev_map
         (fun site_string ->
            Signature.num_of_site
              (Locality.dummy_annot site_string)
              agent_interface)
         (List.rev equ_class))
    (List.rev l)

let translate_to_lkappa_representation env partitioned_contact_map =
  let signature = Model.signatures env in
  let nagents = Signature.size signature in
  let array = Array.make nagents Symmetries_sig.empty in
  let () =
    Mods.StringMap.iter
      (fun agent_string partition ->
         let ag_id =
           Signature.num_of_agent
             (Locality.dummy_annot agent_string)
             signature
         in
         let interface = Signature.get signature ag_id in
         let partition =
           Symmetries_sig.map
            (fun site_string  ->
              Signature.num_of_site
                (Locality.dummy_annot site_string)
                interface)
            partition
         in
         array.(ag_id) <- partition)
      partitioned_contact_map
  in
  array

let partition_pair cache p l =
  let rec part cache yes no = function
    | [] -> cache, (List.rev yes, List.rev no)
    | x :: l ->
      let cache, b = p cache x in
      if b
      then part
          cache
          (x :: yes)
          no
          l
      else part cache yes (x :: no) l in
  part cache [] [] l

let refine_class cache p l result =
  let rec aux cache to_do classes =
    match to_do with
    | [] -> cache, classes
    | h::tail ->
      let cache, (newclass, others) =
        partition_pair cache (fun cache -> p cache h) tail
      in
      aux cache others ((h::newclass) :: classes)
  in
  aux cache l result

let refine_class cache p l =
  if l <> [] then
    List.fold_left
      (fun (cache, result) l ->
         let cache, result =
           refine_class cache p l result
         in
         cache, result
      ) (cache, []) l
  else (cache, [])

let refine_partitioned_contact_map_in_lkappa_representation
    cache
    p_internal_state
    p_binding_state
    p_both
    partitioned_contact_map =
  Tools.array_fold_lefti
    (fun agent_type cache partition ->
       let over_binding_states = partition.Symmetries_sig.over_binding_states in
       let over_internal_states = partition.Symmetries_sig.over_internal_states
       in
       let over_full_states = partition.Symmetries_sig.over_full_states in
       let cache, a =
        refine_class
          cache
          (fun cache -> p_internal_state cache agent_type)
          over_internal_states
      in
      let cache, b =
        refine_class
          cache
          (fun cache -> p_binding_state cache agent_type)
          over_binding_states
      in
      let cache, c =
        refine_class
          cache
          (fun cache -> p_both cache agent_type)
          over_full_states
      in
      let () =
        partitioned_contact_map.(agent_type) <-
          {
            Symmetries_sig.over_internal_states = a ;
            Symmetries_sig.over_binding_states = b ;
            Symmetries_sig.over_full_states = c
          }
      in
      cache
    ) cache partitioned_contact_map, partitioned_contact_map

(*****************************************************************)
(*DETECT SYMMETRIES*)
(*****************************************************************)

let max_hash h1 h2 =
  if compare h1 h2 >= 0
  then h1
  else h2

let max_hashes hash_list =
  let rec aux tail best =
    match tail with
    | [] -> best
    | head :: tail -> aux tail (max_hash best head)
  in aux hash_list LKappa_auto.RuleCache.empty

let build_array_for_symmetries_gen size_hash_plus_1 hashed_list =
  let to_be_checked = Array.make size_hash_plus_1 false in
  let counter = Array.make size_hash_plus_1 0 in
  let correct = Array.make size_hash_plus_1 1 in
  to_be_checked, counter, correct

let build_array_for_symmetries hashed_list =
  let max_hash = max_hashes hashed_list in
  let size_hash_plus_1 =
    (LKappa_auto.RuleCache.int_of_hashed_list max_hash) + 1
  in
  let to_be_checked, counter, correct =
    build_array_for_symmetries_gen size_hash_plus_1 hashed_list
  in
  let rate =
    Array.make size_hash_plus_1 Rule_modes.RuleModeMap.empty
  in
  to_be_checked, counter, rate, correct

(******************************************************************)

let check_invariance_gen
    p ?parameters ?env ~to_be_checked ~counter ~correct ~rates
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list *
                          LKappa.rule) list)
    cache agent_type site1 site2 =
  let rec aux hash_and_rule_list (cache, to_be_checked, counter) =
    match hash_and_rule_list with
    | [] -> (cache, to_be_checked, counter), true
    | (hash, rule) :: tail ->
      let id = LKappa_auto.RuleCache.int_of_hashed_list hash in
      if
        to_be_checked.(id)
      then
        let (cache, counter, to_be_checked), b =
          p ?parameters ?env ~agent_type ~site1 ~site2 rule ~correct
            rates cache ~counter to_be_checked
        in
        if b then
          aux tail (cache, to_be_checked, counter)
        else
          (cache, to_be_checked, counter), false
      else
        aux tail (cache, to_be_checked, counter)
  in
  aux hash_and_rule_list (cache, to_be_checked, counter)

let check_invariance_internal_states
    ~correct ~rates ?parameters ?env
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list *
                          LKappa.rule) list)
    (cache, to_be_checked, counter)
    agent_type site1 site2 =
  check_invariance_gen
    LKappa_group_action.check_orbit_internal_state_permutation
    ?parameters ?env
    ~to_be_checked ~counter ~correct ~rates
    hash_and_rule_list cache agent_type site1 site2

let check_invariance_binding_states
    ~correct ~rates ?parameters ?env
    hash_and_rule_list
    (cache, to_be_checked, counter)
    agent_type site1 site2 =
  check_invariance_gen
    LKappa_group_action.check_orbit_binding_state_permutation
    ?parameters ?env
    ~to_be_checked ~counter ~correct ~rates
    hash_and_rule_list cache agent_type site1 site2

let check_invariance_both
    ~correct ~rates ?parameters ?env
    hash_and_rule_list
    (cache, to_be_checked, counter)
    agent_type site1 site2 =
  check_invariance_gen
    LKappa_group_action.check_orbit_full_permutation
    ?parameters ?env
    ~to_be_checked ~counter ~correct ~rates
    hash_and_rule_list cache agent_type site1 site2

let print_symmetries_for_rules parameters env contact_map
    partitioned_contact_map partitioned_contact_map_in_lkappa
    refined_partitioned_contact_map =
  let () =
    if Remanent_parameters.get_trace parameters
    then
      let logger = Remanent_parameters.get_logger parameters in
      let () = Loggers.fprintf logger "Contact map" in
      let () = Loggers.print_newline logger in
      let () = print_contact_map parameters contact_map in
      let () = Loggers.fprintf logger "Partitioned contact map" in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map parameters partitioned_contact_map in
      let () = Loggers.fprintf logger
          "Partitioned contact map (LKAPPA)"
      in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map_in_lkappa logger env
          partitioned_contact_map_in_lkappa
      in
      let () = Loggers.fprintf logger "With predicate (LKAPPA)" in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map_in_lkappa
          logger env
          refined_partitioned_contact_map
      in
      ()
    else
      ()
  in
  ()

let print_symmetries_for_init parameters env
    partitioned_contact_map partitioned_contact_map_in_lkappa
    refined_partitioned_contact_map =
  let () =
    if Remanent_parameters.get_trace parameters
    then
      let logger = Remanent_parameters.get_logger parameters in
      let () = Loggers.fprintf logger "Partitioned contact map initial" in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map parameters partitioned_contact_map in
      let () = Loggers.fprintf logger
          "Partitioned contact map (LKAPPA) initial"
      in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map_in_lkappa logger env
          partitioned_contact_map_in_lkappa
      in
      let () = Loggers.fprintf logger "With predicate (LKAPPA) initial" in
      let () = Loggers.print_newline logger in
      let () =
        print_partitioned_contact_map_in_lkappa
          logger env
          refined_partitioned_contact_map
      in
      ()
    else
      ()
  in
  ()

let detect_symmetries_gen parameters env cache
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list *
                          LKappa.rule) list)
    arrays
    (contact_map:(string list * (string * string) list)
         Mods.StringMap.t Mods.StringMap.t) =
  (*-------------------------------------------------------------*)
  (*PARTITION A CONTACT MAP RETURN A LIST OF LIST OF SITES*)
  let partitioned_contact_map =
    collect_partitioned_contact_map contact_map
  in
  (*-------------------------------------------------------------*)
  (*PARTITION A CONTACT MAP RETURN A LIST OF LIST OF SITES WITH A
    PREDICATE*)
  let partitioned_contact_map_in_lkappa =
    translate_to_lkappa_representation env partitioned_contact_map
  in
  let p' = Array.copy partitioned_contact_map_in_lkappa in
  let to_be_checked, counter, rates, correct = arrays in
  let (cache, _, _), refined_partitioned_contact_map =
    let parameters, env = Some parameters, Some env in
    refine_partitioned_contact_map_in_lkappa_representation
      (cache, to_be_checked, counter)
      (check_invariance_internal_states
         ?parameters
         ?env ~correct ~rates hash_and_rule_list)
      (check_invariance_binding_states
         ?parameters
         ?env ~correct ~rates hash_and_rule_list)
      (check_invariance_both
         ?parameters
         ?env ~correct ~rates hash_and_rule_list)
      p'
  in
  let refined_partitioned_contact_map =
    Array.map Symmetries_sig.clean refined_partitioned_contact_map
  in
  cache, partitioned_contact_map, partitioned_contact_map_in_lkappa,
  refined_partitioned_contact_map

let detect_symmetries_for_rules parameters env cache
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list *
                          LKappa.rule) list)
    arrays
    (contact_map:(string list * (string * string) list)
         Mods.StringMap.t Mods.StringMap.t) =
  let cache, partitioned_contact_map, partitioned_contact_map_in_lkappa,
      refined_partitioned_contact_map =
    detect_symmetries_gen parameters env cache
      hash_and_rule_list
      arrays
      contact_map
  in
  let () =
    print_symmetries_for_rules parameters env contact_map
      partitioned_contact_map partitioned_contact_map_in_lkappa
      refined_partitioned_contact_map
  in
  cache, refined_partitioned_contact_map

let detect_symmetries_for_init parameters env cache
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list *
                          LKappa.rule) list)
    arrays
    (contact_map:(string list * (string * string) list)
         Mods.StringMap.t Mods.StringMap.t) =
  let cache, partitioned_contact_map, partitioned_contact_map_in_lkappa,
      refine_partitioned_contact_map =
    detect_symmetries_gen parameters env cache
      hash_and_rule_list
      arrays
      contact_map
  in
  let () =
    print_symmetries_for_init parameters env
      partitioned_contact_map partitioned_contact_map_in_lkappa
      refine_partitioned_contact_map
  in
  cache, refine_partitioned_contact_map

(******************************************************)

module Cc =
struct
  type t = Pattern.cc
  let compare = compare
  let print _ _ = ()
end

module CcSetMap = SetMap.Make(Cc)

module CcMap = CcSetMap.Map

type cache = Pattern.cc CcMap.t

let empty_cache () = CcMap.empty

let representant ?parameters signature cache rule_cache preenv_cache symmetries
    species =
  match CcMap.find_option species cache with
  | Some species -> cache, rule_cache, preenv_cache, species
  | None ->
    let rule_cache, preenv_cache, species' =
      Pattern_group_action.normalize
        ?parameters
        signature
        rule_cache
        preenv_cache
        symmetries
        species
    in
    let cache  = CcMap.add species species' cache in
    cache, rule_cache, preenv_cache, species'

let print_symmetries parameters env symmetries =
  let log = Remanent_parameters.get_logger parameters in
  let () = Loggers.fprintf log "Symmetries:" in
  let () = Loggers.print_newline  log in
  print_partitioned_contact_map_in_lkappa log env symmetries
