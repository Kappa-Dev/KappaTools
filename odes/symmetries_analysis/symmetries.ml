(**
   * symmetries.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Mar 06 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(***************************************************************************)
(*TYPE*)
(***************************************************************************)
type contact_map =
  ((string list) * (string*string) list)
    Mods.StringSetMap.Map.t Mods.StringSetMap.Map.t

type partitioned_contact_map =
  ((string list list) * (string list list))
    Mods.StringSetMap.Map.t

type lkappa_partitioned_contact_map =
  ((int list list) * (int list list))
    array

type symmetries = lkappa_partitioned_contact_map

(***************************************************************)
(*PARTITION THE CONTACT MAP*)
(***************************************************************)

let add k data map =
  let old =
    match
      Mods.IntMap.find_option k map
    with
    | None -> []
    | Some l -> l
  in
  Mods.IntMap.add k (data::old) map

let partition cache hash int_of_hash f map =
  let map =
    fst
      (
        Mods.StringMap.fold
          (fun key data (inverse,cache) ->
             let range = f data in
             if range = []
             then inverse,cache
             else
               let sorted_range = List.sort compare range in
               let cache, hash = hash cache sorted_range in
               let inverse = add (int_of_hash hash) key inverse in
               inverse,cache
          )
          map
          (Mods.IntMap.empty,
           cache))
  in
  List.rev
    (Mods.IntMap.fold
       (fun _ l sol -> l::sol)
       map
       [])

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
       let internal_state_partition =
         partition cache1 StateList.hash
           StateList.int_of_hashed_list fst sitemap
       in
       let binding_state_partition =
         partition
           cache2 BindingTypeList.hash BindingTypeList.int_of_hashed_list snd
           sitemap
       in
       internal_state_partition,
       binding_state_partition
    )
    contact_map

(****************************************************************)

let refine_class p l result =
  let rec aux to_do classes =
    match to_do with
    | [] -> classes
    | h::_ ->
      let newclass, others =
        List.partition (p h) to_do
      in
      aux others (newclass :: classes)
  in
  aux l result

let refine_class p l =
  List.fold_left
      (fun result l ->
        refine_class p l result
      ) [] l

let collect_partitioned_with_predicate
    p_internal_state
    p_binding_state
    partitioned_contact_map =
    Mods.StringMap.mapi
      (fun agent_type
        (internal_sites_partition,binding_sites_partition) ->
          refine_class (p_internal_state agent_type)
            internal_sites_partition,
         refine_class (p_binding_state agent_type)
           binding_sites_partition
      ) partitioned_contact_map

(*****************************************************************)
(*PRINT*)
(*****************************************************************)

let print_l parameters l =
  if l <> []
  then
    List.iter
      (fun equ_class ->
      let () =
        List.iter
          (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s,") equ_class
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ())
      l
  else Loggers.print_newline (Remanent_parameters.get_logger parameters)

let print_partitioned_contact_map parameters partitioned_contact_map =
  Mods.StringMap.iter
    (fun agent (l1,l2) ->
       let () =
         Loggers.fprintf
           (Remanent_parameters.get_logger parameters)
           "Agent: %s"
           agent
       in
       let () =
         Loggers.print_newline
           (Remanent_parameters.get_logger parameters)
       in
       let () =
         Loggers.fprintf
           (Remanent_parameters.get_logger parameters) "internal sites:"
       in
       let () =
         Loggers.print_newline
           (Remanent_parameters.get_logger parameters)
       in
       let () =
         print_l parameters l1
       in
       let () =
         Loggers.fprintf
           (Remanent_parameters.get_logger parameters) "binding sites:"
       in
       let () =
         Loggers.print_newline
           (Remanent_parameters.get_logger parameters)
       in
       let () =
         print_l parameters l2
       in ()
    ) partitioned_contact_map

let print_contact_map parameters contact_map =
  Mods.StringMap.iter
    (fun agent sitemap ->
       let () =
         Loggers.fprintf (Remanent_parameters.get_logger parameters)
           "agent:%s\n"
           agent
       in
       Mods.StringMap.iter
         (fun site (l1,l2) ->
         let () =
           Loggers.fprintf (Remanent_parameters.get_logger parameters)
             "  site:%s\n"
             site
         in
         let () =
           if l1<>[]
           then
             let () =
               Loggers.fprintf (Remanent_parameters.get_logger parameters)
                 "internal_states:"
             in
             let () =
               List.iter
                 (Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s;")
                 l1
             in
             let () =
               Loggers.print_newline (Remanent_parameters.get_logger parameters)
             in
             ()
         in
         let () =
           if l2<>[]
           then
             let () =
               Loggers.fprintf (Remanent_parameters.get_logger parameters)
                 "binding_states:"
             in
             let () =
               List.iter
                 (fun (s1,s2) ->
                    Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s.%s;" s1 s2)
                 l2
             in
             let () =
               Loggers.print_newline (Remanent_parameters.get_logger parameters)
             in
             ()
         in
         ())
         sitemap)
    contact_map



(****************************************************************)

let translate_list l agent_interface =
  List.rev_map
    (fun equ_class ->
       List.rev_map
         (fun site_string ->
            Signature.num_of_site
              (Locality.dummy_annot site_string)
              agent_interface)
         equ_class)
    (List.rev l)

let translate_to_lkappa_representation env partitioned_contact_map =
  let signature = Model.signatures env in
  let nagents = Signature.size signature in
  let array = Array.make nagents ([],[]) in
  let () =
    Mods.StringMap.iter
      (fun agent_string (l1,l2) ->
         let ag_id =
           Signature.num_of_agent
             (Locality.dummy_annot agent_string)
             signature
         in
         let interface = Signature.get signature ag_id in
         let l1 = translate_list l1 interface in
         let l2 = translate_list l2 interface in
         array.(ag_id)<-(l1,l2))
      partitioned_contact_map
  in
  array

let partition_pair cache p l =
  let rec part cache yes no = function
    | [] -> cache, (List.rev yes, List.rev no)
    | x :: l ->
      let cache, b = p cache x in
      if b
      then part cache (x :: yes) no l
      else part cache yes (x :: no) l in
  part cache [] [] l

let refine_class' cache p l result =
  let rec aux cache to_do classes =
    match to_do with
    | [] -> cache, classes
    | h::_ ->
      let cache, (newclass, others) =
        partition_pair cache (fun cache -> p cache h) to_do
      in
      aux cache others (newclass :: classes)
  in
  aux cache l result

let refine_class' cache p l =
  if l <> [] then
    List.fold_left
      (fun (cache, result) l ->
         let cache, result =
           refine_class' cache p l result
         in
         cache, result
      ) (cache, []) l
  else (cache, [])

let refine_partitioned_contact_map_in_lkappa_representation
    cache
    p_internal_state
    p_binding_state
    partitioned_contact_map
  =
  Tools.array_fold_lefti
    (fun
      agent_type
      cache
      (internal_sites_partition,binding_sites_partition)
      ->
      let cache, a =
        refine_class'
          cache
          (fun cache -> p_internal_state cache agent_type)
          internal_sites_partition
      in
      let cache, b =
        refine_class'
          cache
          (fun cache -> p_binding_state cache agent_type)
          binding_sites_partition
      in
      let () =
        partitioned_contact_map.(agent_type) <- (a,b)
      in
      cache
    )
    cache
    partitioned_contact_map,
  partitioned_contact_map

(******************************************************************)

let print_l logger fmt signature agent l =
  let () = Loggers.fprintf logger "{" in
  let size_1 =
    match l with [l] -> true
               | [] | _::_ -> false
  in

  let b =
    List.fold_left
      (fun b equ_class ->
         let () = if b then Loggers.fprintf logger "," in
         let () = if not size_1 then Loggers.print_newline logger in
         let () = if not size_1 then Loggers.fprintf logger "  " in
         let () = Loggers.fprintf logger "{" in
         let _ =
           List.fold_left
             (fun b site ->
                let () = if b then Loggers.fprintf logger "," in
                let () = Signature.print_site signature agent fmt site in
                true)
             false
             equ_class
         in
         let () = Loggers.fprintf logger "}" in
         true)
      false
      l
  in
  let () = if b && not size_1 then Loggers.print_newline logger in
  let () = Loggers.fprintf logger "}" in
  let () = Loggers.print_newline logger in
  ()

let print_partitioned_contact_map_in_lkappa
    logger env partitioned_contact_map =
  let signature = Model.signatures env in
  let fmt = Loggers.formatter_of_logger logger in
  match fmt with
  | None -> ()
  | Some fmt ->
    Array.iteri
      (fun agent (l1,l2) ->
         let () = Loggers.fprintf logger "Agent: " in
         let () = Signature.print_agent signature fmt agent in
         let () = Loggers.print_newline logger in
         let () = Loggers.fprintf logger "Partition of the sites for internal states:" in
         let () = Loggers.print_newline logger in
         let () = print_l logger fmt signature agent l1 in
         let () = Loggers.fprintf logger "Partition of the sites for bindings states:" in
         let () = Loggers.print_newline logger in
         let () = print_l logger fmt signature agent l2 in
         ()
      ) partitioned_contact_map

      (*****************************************************************)
      (*DETECT SYMMETRIES*)
      (*****************************************************************)


let check_invariance_gen
    p ~to_be_checked ~counter ~correct ~rates
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list * LKappa.rule) list) cache agent_type site1 site2 =
  let rec aux hash_and_rule_list (cache, to_be_checked, counter) =
    match hash_and_rule_list with
    | [] -> (cache, to_be_checked, counter), true
    | (hash,rule)::tail ->
      let id = LKappa_auto.RuleCache.int_of_hashed_list hash in
      if
        to_be_checked.(id)
      then
        let (cache, counter, to_be_checked), b =
          p ~agent_type ~site1 ~site2 rule ~correct rates cache ~counter
            to_be_checked
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
    ~correct ~rates (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list * LKappa.rule) list)
    (cache, to_be_checked, counter)
    agent_type site1 site2
  =
  check_invariance_gen
    LKappa_group_action.check_orbit_internal_state_permutation
    ~to_be_checked ~counter ~correct ~rates
    hash_and_rule_list cache agent_type site1 site2

let check_invariance_binding_states
    ~correct ~rates hash_and_rule_list
    (cache, to_be_checked, counter)
    agent_type site1 site2
      =
      check_invariance_gen
        LKappa_group_action.check_orbit_binding_state_permutation
        ~to_be_checked ~counter ~correct ~rates
        hash_and_rule_list cache agent_type site1 site2

let detect_symmetries parameters env cache
    (hash_and_rule_list: (LKappa_auto.RuleCache.hashed_list * LKappa.rule) list)
    arrays
    (contact_map:(string list * (string * string) list) Mods.StringMap.t Mods.StringMap.t) =
  (*-------------------------------------------------------------*)
  (*PARTITION A CONTACT MAP RETURN A LIST OF LIST OF SITES*)
  let partitioned_contact_map =
    collect_partitioned_contact_map contact_map
  in
  (*-------------------------------------------------------------*)
  (*PARTITION A CONTACT MAP RETURN A LIST OF LIST OF SITES WITH A PREDICATE*)
  let partitioned_contact_map_in_lkappa =
    translate_to_lkappa_representation env partitioned_contact_map
  in
  let p' = Array.copy partitioned_contact_map_in_lkappa in
  let to_be_checked, counter, rates, correct = arrays in
  let (cache,_,_), refined_partitioned_contact_map =
    refine_partitioned_contact_map_in_lkappa_representation
      (cache, to_be_checked, counter)
      (check_invariance_internal_states
         ~correct ~rates hash_and_rule_list)
      (check_invariance_binding_states
         ~correct ~rates hash_and_rule_list)
      p'
  in
  (*-------------------------------------------------------------*)
  (*PRINT*)
  let () =
    if Remanent_parameters.get_trace parameters
    then
      let logger = Remanent_parameters.get_logger parameters in
      let () =
        Loggers.fprintf logger "Contact map"
      in
      let () =
        Loggers.print_newline logger
      in
      let () =
        print_contact_map parameters contact_map
      in
      let () =
        Loggers.fprintf logger "Partitioned contact map"
      in
      let () =
        Loggers.print_newline logger
      in
      let () =
        print_partitioned_contact_map parameters partitioned_contact_map
      in
      let () =
        Loggers.fprintf logger "Partitioned contact map (LKAPPA)"
      in
      let () =
        Loggers.print_newline logger
      in
      let () =
        print_partitioned_contact_map_in_lkappa logger env  partitioned_contact_map_in_lkappa
      in
      let () =
        Loggers.fprintf logger "With predicate (LKAPPA)"
      in
      let () =
        Loggers.print_newline logger
      in
      let () =
        print_partitioned_contact_map_in_lkappa
          logger env
          refined_partitioned_contact_map
      in
      ()
    else
      ()
  in
  cache, refined_partitioned_contact_map

(***************************************************************************)

let max_hash h1 h2 =
  if compare h1 h2 >= 0
  then h1
  else h2

let max_hashes hash_list =
  let rec aux tail best =
    match
      tail
    with
    | [] -> best
    | head::tail ->
      aux tail (max_hash best head)
  in
  aux hash_list LKappa_auto.RuleCache.empty

let build_array_for_symmetries nbr_rule_list =
  let max_hash = max_hashes nbr_rule_list in
  let size_hash_plus_1 =
    (LKappa_auto.RuleCache.int_of_hashed_list max_hash) + 1
  in
  let to_be_checked = Array.make size_hash_plus_1 false in
  let counter = Array.make size_hash_plus_1 0 in
  let rate = Array.make size_hash_plus_1 Rule_modes.RuleModeMap.empty in
  let correct = Array.make size_hash_plus_1 1 in
  to_be_checked, counter, rate, correct

let print_line_stars log =
  Loggers.fprintf log "\n***************\n"

let print_hash log hash =
  Loggers.fprintf log " Hash-:%a\n"
    LKappa_auto.RuleCache.print hash;
  Loggers.print_newline log

let print_hash_list log hash_list =
  List.iter (fun hash ->
      let () = print_line_stars log in
      let () = print_hash log hash in
      let () = print_line_stars log in ()
    ) hash_list

let print_kinetic_rate_list ?env log fmt l =
  List.iter (fun rate_opt ->
      match rate_opt with
      | None -> ()
      | Some (alg, loc) ->
        (*let () = Locality.print fmt loc in*)
        let () = Loggers.fprintf log " Rate: " in
        let () = Kappa_printer.alg_expr ?env fmt alg in
        let () = Loggers.print_newline log in
        ()
    ) l

let print_map log map =
  let () = print_line_stars log in
  let () = Loggers.print_newline log in
  let () =
    LKappa_auto.CannonicMap.fold (fun cannonic (nocc, nauto) () ->
        let () = Loggers.fprintf log "nocc:%i - " nocc in
        let () = Loggers.fprintf log "nauto:%i - " nauto in
        let () = Loggers.fprintf log "cannonic:%a"
            LKappa_auto.CannonicCache.print cannonic
        in
        let () = Loggers.print_newline log in ()
      ) map ()
  in
  let () = print_line_stars log in
  ()

let print_divide_rule_rate_by log i =
  Loggers.fprintf log " Divide_rule_rate_by:%i\n" i

let print_cannonic_form_from_syntactic_rules
    ~compil log print_rule_name print_rule_id rule_id rule
    nbr_auto_in_lhs
    nbr_auto_in_rule rate_opt_list
  =
  match Loggers.formatter_of_logger log with
  | None -> ()
  | Some fmt ->
    (*let () = Loggers.fprintf log "rule: \n" in
      let () = I.print_rule ~compil fmt rule in*)
    let () = Loggers.fprintf log " rule_name: " in
    let () = print_rule_name ~compil fmt rule in
    let () = Loggers.print_newline log in
    let () = print_divide_rule_rate_by log nbr_auto_in_lhs in
    let () = print_kinetic_rate_list log fmt rate_opt_list in
    let () = Loggers.fprintf log " rule_id: " in
    let () = print_rule_id fmt rule_id in
    let () = Loggers.print_newline log in
    let () = print_hash log nbr_auto_in_rule in
    let () = Loggers.print_newline log in
    (*let () = print_list_of_agent_id log list_of_agent_id in*)
    let () = print_line_stars log in
    (*let () = Loggers.fprintf log " Gamma: " in
      let () = print_gamma_list log fmt gamma_list in
      let () = print_line_stars log in*)
    ()


module Cc =
struct
  type t = Pattern.cc
  let compare = compare
  let print _ _ = ()
end

module CcSetMap =
  SetMap.Make(Cc)

module CcMap = CcSetMap.Map

type cache = Pattern.cc CcMap.t

let empty_cache () = CcMap.empty

let representant ?parameters signature cache preenv_cache symmetries species =
  match
    CcMap.find_option species cache
  with
  | Some species -> cache, preenv_cache, species
  | None ->
    let preenv_cache, species' =
      Pattern_group_action.normalize_internal_states_in_raw_mixture
        ?parameters
        signature
        preenv_cache
        symmetries
        species
    in
    let cache  = CcMap.add species species' cache in
    cache, preenv_cache, species'

let print_symmetries parameters env symmetries =
  let () =
    Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "Symmetries:"
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  print_partitioned_contact_map_in_lkappa
    (Remanent_parameters.get_logger parameters)
    env symmetries
