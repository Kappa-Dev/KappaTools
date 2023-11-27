(**
   * LKappa_group_action.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Jun 13 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false

let do_print ?trace ?fmt ?(sigs : Signature.s option) f =
  match (local_trace, trace), fmt, sigs with
  | (true, _ | _, Some true), Some fmt, Some sigs -> f sigs fmt
  | (false, (Some false | None)), _, _ | _, None, _ | _, _, None -> ()

let binding_equal ((a_t, _), a_m) ((b_t, _), b_m) = a_t = b_t && a_m = b_m

let care_binding_regular ((test, _), mods) =
  (match mods with
  | LKappa.Maintained | LKappa.Erased -> false
  | LKappa.Freed | LKappa.Linked _ -> true)
  ||
  match test with
  | LKappa.LNK_VALUE _ | LKappa.LNK_FREE | LKappa.ANY_FREE | LKappa.LNK_SOME
  | LKappa.LNK_TYPE _ ->
    true
  | LKappa.LNK_ANY -> false

let care_internal_regular = function
  | LKappa.I_ANY | LKappa.I_ANY_ERASED -> false
  | LKappa.I_ANY_CHANGED _ | LKappa.I_VAL_CHANGED _ | LKappa.I_VAL_ERASED _ ->
    true

let may_swap_internal_state_regular ag_type site1 site2 ag =
  ag_type = ag.LKappa.ra_type
  && (care_internal_regular ag.LKappa.ra_ints.(site1)
     || care_internal_regular ag.LKappa.ra_ints.(site2))
  && ag.LKappa.ra_ints.(site1) <> ag.LKappa.ra_ints.(site2)

let may_swap_binding_state_regular ag_type site1 site2 ag =
  ag_type = ag.LKappa.ra_type
  && (care_binding_regular ag.LKappa.ra_ports.(site1)
     || care_binding_regular ag.LKappa.ra_ports.(site2))
  && not (binding_equal ag.LKappa.ra_ports.(site1) ag.LKappa.ra_ports.(site2))

let may_swap_full_regular ag_type site1 site2 ag =
  may_swap_binding_state_regular ag_type site1 site2 ag
  || may_swap_internal_state_regular ag_type site1 site2 ag

let may_swap_internal_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  && ag.Raw_mixture.a_ints.(site1) <> ag.Raw_mixture.a_ints.(site2)

let may_swap_binding_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  && ag.Raw_mixture.a_ports.(site1) <> ag.Raw_mixture.a_ports.(site2)

let may_swap_full_created ag_type site1 site2 ag =
  may_swap_internal_state_created ag_type site1 site2 ag
  || may_swap_binding_state_created ag_type site1 site2 ag

let of_rule rule = rule.LKappa.r_mix, rule.LKappa.r_created
let is_empty (rule_tail, created_tail) = rule_tail = [] && created_tail = []

let p_head ~fmt_err ~sigs p p_raw (rule_tail, created_tail) =
  match rule_tail, created_tail with
  | h :: t, _ -> p h, (t, created_tail)
  | _, h :: t -> p_raw h, (rule_tail, t)
  | [], [] ->
    let s1, i1, i2, i3 = __POS__ in
    let s = Format.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    let fmt = fmt_err in
    let trace = Some true in
    let () =
      do_print ?trace ?fmt ?sigs (fun _ fmt -> Format.fprintf fmt "%s@" s)
    in
    raise (invalid_arg s)

let apply_head ~fmt_err ~sigs sigma sigma_raw (rule_tail, created_tail) =
  match rule_tail, created_tail with
  | h :: t, _ ->
    let () = sigma h in
    t, created_tail
  | _, h :: t ->
    let () = sigma_raw h in
    rule_tail, t
  | [], [] ->
    let s1, i1, i2, i3 = __POS__ in
    let s = Format.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    let fmt = fmt_err in
    let trace = Some true in
    let () =
      do_print ?trace ?fmt ?sigs (fun _ fmt -> Format.fprintf fmt "%s@" s)
    in
    raise (invalid_arg s)

let apply_head_predicate ~fmt_err ~sigs f f_raw cache (rule_tail, created_tail)
    rule =
  match rule_tail, created_tail with
  | h :: _, _ -> f h rule cache
  | _, h :: _ -> f_raw h rule cache
  | [], [] ->
    let s1, i1, i2, i3 = __POS__ in
    let s = Format.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    let fmt = fmt_err in
    let trace = Some true in
    let () =
      do_print ?trace ?fmt ?sigs (fun _ fmt -> Format.fprintf fmt "%s@" s)
    in
    raise (invalid_arg s)

let shift ~fmt_err ~sigs tail = apply_head ~fmt_err ~sigs ignore ignore tail

(***************************************************************)

let filter_positions ~fmt_err ~sigs p p_raw rule =
  let rec aux pos_id rule_tail accu =
    if is_empty rule_tail then
      List.rev accu
    else (
      let b, rule_tail = p_head ~fmt_err ~sigs p p_raw rule_tail in
      if b then
        aux (pos_id + 1) rule_tail (pos_id :: accu)
      else
        aux (pos_id + 1) rule_tail accu
    )
  in
  aux 0 (of_rule rule) []

let potential_positions_for_swapping_internal_states ~fmt_err ~sigs agent_type
    site1 site2 rule : int list =
  filter_positions ~fmt_err ~sigs
    (may_swap_internal_state_regular agent_type site1 site2)
    (may_swap_internal_state_created agent_type site1 site2)
    rule

let potential_positions_for_swapping_binding_states ~fmt_err ~sigs agent_type
    site1 site2 rule =
  filter_positions ~fmt_err ~sigs
    (may_swap_binding_state_regular agent_type site1 site2)
    (may_swap_binding_state_created agent_type site1 site2)
    rule

let potential_positions_for_swapping_full ~fmt_err ~sigs agent_type site1 site2
    rule =
  filter_positions ~fmt_err ~sigs
    (may_swap_full_regular agent_type site1 site2)
    (may_swap_full_created agent_type site1 site2)
    rule

(******************************************************************)

let backtrack ~fmt_err ~sigs sigma_inv sigma_raw_inv counter positions rule =
  let rec aux ~fmt_err ~sigs agent_id rule_tail pos_id positions_tail =
    match positions_tail with
    | [] -> ()
    | pos_head :: _ when agent_id < pos_head ->
      let rule_tail = shift ~fmt_err ~sigs rule_tail in
      aux ~fmt_err ~sigs (agent_id + 1) rule_tail pos_id positions_tail
    | pos_head :: pos_tail when agent_id = pos_head ->
      let rule_tail =
        if counter.(pos_id) then
          apply_head ~fmt_err ~sigs sigma_inv sigma_raw_inv rule_tail
        else
          shift ~fmt_err ~sigs rule_tail
      in
      aux ~fmt_err ~sigs (agent_id + 1) rule_tail (pos_id + 1) pos_tail
    | _ :: pos_tail ->
      aux ~fmt_err ~sigs agent_id rule_tail (pos_id + 1) pos_tail
  in
  aux ~fmt_err ~sigs 0 (of_rule rule) 0 positions

(***************************************************************)
(*SYMMETRIES*)
(***************************************************************)

let for_all_elt_permutation ~fmt_err ~sigs (positions : int list)
    (f : LKappa.rule_agent -> LKappa.rule -> 'a -> 'a * bool)
    (f_raw : Raw_mixture.agent -> LKappa.rule -> 'a -> 'a * bool)
    (rule : LKappa.rule) (init : 'a) =
  let rec next ~fmt_err ~sigs agent_id rule_tail pos_id positions_tail accu =
    match positions_tail with
    | [] -> accu, true
    | pos_head :: _ when agent_id < pos_head ->
      let rule_tail = shift ~fmt_err ~sigs rule_tail in
      next ~fmt_err ~sigs (agent_id + 1) rule_tail pos_id positions_tail accu
    | pos_head :: pos_tail when agent_id = pos_head ->
      (match
         apply_head_predicate ~fmt_err ~sigs f f_raw accu rule_tail rule
       with
      | accu, false -> accu, false
      | accu, true ->
        let rule_tail = shift ~fmt_err ~sigs rule_tail in
        next ~fmt_err ~sigs (agent_id + 1) rule_tail (pos_id + 1) pos_tail accu)
    | _ :: _ (*when agent_id > pos_head*) ->
      let s1, i1, i2, i3 = __POS__ in
      let string = Format.sprintf "Internal bug: %s %i %i %i" s1 i1 i2 i3 in
      let fmt = fmt_err in
      let trace = Some true in
      let () =
        do_print ?trace ?fmt ?sigs (fun _ fmt ->
            Format.fprintf fmt "%s@ " string)
      in
      accu, false
  in
  next ~fmt_err ~sigs 0 (of_rule rule) 0 positions init

let for_all_over_orbit ~trace ~fmt ~fmt_err ~sigs (positions : int list)
    (sigma : LKappa.rule_agent -> unit) (sigma_inv : LKappa.rule_agent -> unit)
    (sigma_raw : Raw_mixture.agent -> unit)
    (sigma_raw_inv : Raw_mixture.agent -> unit)
    (f :
      trace:bool option ->
      fmt:Format.formatter option ->
      fmt_err:Format.formatter option ->
      sigs:Signature.s option ->
      LKappa.rule ->
      'a ->
      'a * bool) (rule : LKappa.rule) (init : 'a) : 'a * bool =
  let n = List.length positions in
  let counter = Array.make n false in
  let rec next ~trace ~fmt ~fmt_err ~sigs agent_id rule_tail pos_id
      positions_tail accu =
    match positions_tail with
    | [] -> f ~trace ~fmt ~fmt_err ~sigs rule accu
    | pos_head :: _ when agent_id < pos_head ->
      let rule_tail = shift ~fmt_err ~sigs rule_tail in
      next ~trace ~fmt ~fmt_err ~sigs (agent_id + 1) rule_tail pos_id
        positions_tail accu
    | pos_head :: pos_tail when agent_id = pos_head ->
      if counter.(pos_id) then (
        let () = counter.(pos_id) <- false in
        let rule_tail =
          apply_head ~fmt_err ~sigs sigma_inv sigma_raw_inv rule_tail
        in
        next ~trace ~fmt ~fmt_err ~sigs (agent_id + 1) rule_tail (pos_id + 1)
          pos_tail accu
      ) else (
        let () = counter.(pos_id) <- true in
        let _ = apply_head ~fmt_err ~sigs sigma sigma_raw rule_tail in
        let accu, b = f ~trace ~fmt ~fmt_err ~sigs rule accu in
        if b then
          next ~trace ~fmt ~fmt_err ~sigs 0
            (rule.LKappa.r_mix, rule.LKappa.r_created)
            0 positions accu
        else (
          let () =
            backtrack ~fmt_err ~sigs sigma_inv sigma_raw_inv counter positions
              rule
          in
          accu, false
        )
      )
    | _ :: _ (*when agent_id > pos_head*) ->
      let s1, i1, i2, i3 = __POS__ in
      let s = Format.sprintf "Internal bug: %s %i %i %i" s1 i1 i2 i3 in
      let fmt = fmt_err in
      let trace = Some true in
      let () =
        do_print ?trace ?fmt ?sigs (fun _ fmt -> Format.fprintf fmt "%s@" s)
      in
      let () =
        backtrack ~fmt_err ~sigs sigma_inv sigma_raw_inv counter positions rule
      in
      accu, false
  in
  next ~trace ~fmt ~fmt_err ~sigs 0 (of_rule rule) 0 positions init

exception False

(****************************************************************)
(*cannonic form for symmetries*)
(****************************************************************)

(** Swapping sites in regular (tested/modified/deleted) agents *)

let swap_binding_state_regular _ag_type site1 site2 ag =
  let tmp = ag.LKappa.ra_ports.(site1) in
  let () = ag.LKappa.ra_ports.(site1) <- ag.LKappa.ra_ports.(site2) in
  let () = ag.LKappa.ra_ports.(site2) <- tmp in
  ()

let swap_internal_state_regular _ag_type site1 site2 (ag : LKappa.rule_agent) =
  let tmp = ag.LKappa.ra_ints.(site1) in
  let () = ag.LKappa.ra_ints.(site1) <- ag.LKappa.ra_ints.(site2) in
  let () = ag.LKappa.ra_ints.(site2) <- tmp in
  ()

let swap_full_regular ag_type site1 site2 ag =
  let () = swap_internal_state_regular ag_type site1 site2 ag in
  swap_binding_state_regular ag_type site1 site2 ag

(** Swapping sites in created agents *)

let swap_binding_state_created _ag_type site1 site2 ag =
  let tmp = ag.Raw_mixture.a_ports.(site1) in
  let () = ag.Raw_mixture.a_ports.(site1) <- ag.Raw_mixture.a_ports.(site2) in
  let () = ag.Raw_mixture.a_ports.(site2) <- tmp in
  ()

let swap_internal_state_created _ag_type site1 site2 ag =
  let tmp = ag.Raw_mixture.a_ints.(site1) in
  let () = ag.Raw_mixture.a_ints.(site1) <- ag.Raw_mixture.a_ints.(site2) in
  let () = ag.Raw_mixture.a_ints.(site2) <- tmp in
  ()

let swap_full_created ag_type site1 site2 ag =
  let () = swap_internal_state_created ag_type site1 site2 ag in
  swap_binding_state_created ag_type site1 site2 ag

(******************************************************************)
(*fold over each element transformation*)

let fold_elt_pair f l accu =
  match l with
  | [] -> accu
  | h :: t -> List.fold_left (fun accu s2 -> f h s2 accu) accu t

let fold_one_kind_of_sym_over_an_agent sigma sigma_inv partition f agent_type
    agent rule accu =
  List.fold_left
    (fun accu equ_class ->
      fold_elt_pair
        (fun s1 s2 accu ->
          let () = sigma agent_type s1 s2 agent in
          let accu = f rule accu in
          let () = sigma_inv agent_type s1 s2 agent in
          accu)
        equ_class accu)
    accu partition

let fold_over_elt_transformation get_sym_internal_states get_sym_binding_states
    get_sym_full_states (rule : LKappa.rule) (f : LKappa.rule -> 'a -> 'a)
    (*acc*)
      (accu : 'a) : 'a =
  (*position is a list of agent*)
  let accu =
    List.fold_left
      (fun accu agent ->
        let agent_type = agent.LKappa.ra_type in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_internal_state_regular
            swap_internal_state_regular
            (get_sym_internal_states agent_type)
            f agent_type agent rule accu
        in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_binding_state_regular
            swap_binding_state_regular
            (get_sym_binding_states agent_type)
            f agent_type agent rule accu
        in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_full_regular swap_full_regular
            (get_sym_full_states agent_type)
            f agent_type agent rule accu
        in
        accu)
      accu rule.LKappa.r_mix
  in
  let accu =
    List.fold_left
      (fun accu (agent : Raw_mixture.agent) ->
        let agent_type = agent.Raw_mixture.a_type in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_internal_state_created
            swap_internal_state_created
            (get_sym_internal_states agent_type)
            f agent_type agent rule accu
        in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_binding_state_created
            swap_binding_state_created
            (get_sym_binding_states agent_type)
            f agent_type agent rule accu
        in
        let accu =
          fold_one_kind_of_sym_over_an_agent swap_full_created swap_full_created
            (get_sym_full_states agent_type)
            f agent_type agent rule accu
        in
        accu)
      accu rule.LKappa.r_created
  in
  accu

let copy_lkappa_rule rule =
  {
    rule with
    LKappa.r_mix =
      List.rev_map LKappa.copy_rule_agent (List.rev rule.LKappa.r_mix);
    r_created =
      List.rev_map Raw_mixture.copy_agent (List.rev rule.LKappa.r_created);
  }

let equiv_class cache seen rule ~partitions_internal_states
    ~partitions_binding_states ~partitions_full_states ~convention =
  let to_visit = [ rule ] in
  let rec aux cache to_visit seen visited =
    match to_visit with
    | [] -> cache, seen, visited
    | h :: q ->
      let cache, hashed_list = LKappa_auto.cannonic_form cache h in
      let hash = LKappa_auto.RuleCache.int_of_hashed_list hashed_list in
      if Mods.DynArray.get seen hash then
        aux cache q seen visited
      else (
        let visited = h :: visited in
        let () = Mods.DynArray.set seen hash true in
        let to_visit =
          fold_over_elt_transformation partitions_internal_states
            partitions_binding_states partitions_full_states h
            (fun rule list -> copy_lkappa_rule rule :: list)
            q
        in
        aux cache to_visit seen visited
      )
  in
  let cache, seen, equ_class = aux cache to_visit seen [] in
  let cache, equ_class =
    List.fold_left
      (fun (cache, list) elt ->
        let cache, nauto = LKappa_auto.nauto convention cache elt in
        cache, (elt, nauto) :: list)
      (cache, []) equ_class
  in
  cache, seen, equ_class

type bwd_bisim_info =
  int Symmetries_sig.site_partition array
  * bool Mods.DynArray.t
  * LKappa_auto.cache ref

let saturate_domain_with_symmetric_patterns ~debugMode ~compileModeOn env
    bwd_bisim_info ccs domain =
  let sigs = Model.signatures env in
  let contact_map = Model.contact_map env in
  let equivalence_relations, bool_array, cache_ref = bwd_bisim_info in
  let cache = !cache_ref in
  let partitions_internal_states i =
    equivalence_relations.(i).Symmetries_sig.over_internal_states
  in
  let partitions_binding_states i =
    equivalence_relations.(i).Symmetries_sig.over_binding_states
  in
  let partitions_full_states i =
    equivalence_relations.(i).Symmetries_sig.over_full_states
  in
  let domain, cache =
    List.fold_left
      (fun (domain, cache) cc_array ->
        Array.fold_left
          (fun (domain, cache) cc_id ->
            let cc =
              Pattern.Env.content (Pattern.Env.get (Model.domain env) cc_id)
            in
            let cache, _, equiv_class =
              equiv_class ~partitions_internal_states ~partitions_binding_states
                ~partitions_full_states
                ~convention:
                  Remanent_parameters_sig.Divide_by_nbr_of_autos_in_lhs cache
                bool_array
                (Patterns_extra.pattern_to_lkappa_rule ~sigs cc)
            in
            let domain =
              List.fold_left
                (fun domain (lkappa_rule, _) ->
                  let rule_mixture = lkappa_rule.LKappa.r_mix in
                  let domain, _ =
                    Pattern_compiler
                    .connected_components_sum_of_ambiguous_mixture ~debugMode
                      ~compileModeOn contact_map domain ?origin:None
                      rule_mixture
                  in
                  domain)
                domain equiv_class
            in
            domain, cache)
          (domain, cache) cc_array)
      (domain, cache) ccs
  in
  let () = cache_ref := cache in
  domain

let check_orbit ~trace ~fmt ~fmt_err ~sigs
    (get_positions, sigma, sigma_inv, sigma_raw, sigma_raw_inv) weight agent
    site1 site2 rule correct rates cache counter to_be_checked :
    (LKappa_auto.cache * int array * bool array) * bool =
  let () =
    do_print ?trace ?fmt ?sigs (fun sigs fmt ->
        let () = Format.fprintf fmt "Check an orbit@." in
        let () = Format.fprintf fmt "Permutation of the sites " in
        let () = Signature.print_site sigs agent fmt site1 in
        let () = Format.fprintf fmt " and " in
        let () = Signature.print_site sigs agent fmt site2 in
        let () = Format.fprintf fmt " in agent of type  " in
        let () = Signature.print_agent sigs fmt agent in
        let () = Format.pp_print_newline fmt () in
        let () = Format.fprintf fmt " rule:   " in
        let () =
          LKappa.print_rule ~noCounters:true ~full:true sigs
            (fun _ _ -> ())
            (fun _ _ -> ())
            fmt rule
        in
        let () = Format.pp_print_newline fmt () in
        ())
  in
  let size = Array.length to_be_checked in
  let accu = cache, [], counter, to_be_checked in
  let f ~trace ~fmt ~fmt_err ~sigs rule (cache, l, counter, to_be_checked) =
    let _ = fmt_err in
    let () =
      do_print ?trace ?fmt ?sigs (fun sigs fmt ->
          let () = Format.fprintf fmt " rule:   " in
          let () =
            LKappa.print_rule ~noCounters:true ~full:true sigs
              (fun _ _ -> ())
              (fun _ _ -> ())
              fmt rule
          in
          let () = Format.pp_print_newline fmt () in
          ())
    in
    let cache, hash = LKappa_auto.cannonic_form cache rule in
    let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
    if i < size && to_be_checked.(i) then (
      let () =
        do_print ?fmt ?trace ?sigs (fun _ fmt ->
            let () = Format.fprintf fmt "Existing rule" in
            Format.pp_print_newline fmt ())
      in
      let n = counter.(i) in
      let () = counter.(i) <- n + 1 in
      if n = 0 then
        (cache, hash :: l, counter, to_be_checked), true
      else
        (cache, l, counter, to_be_checked), true
    ) else (
      let () =
        do_print ?fmt ?trace ?sigs (fun _ fmt ->
            let () = Format.fprintf fmt "Unknown rule" in
            Format.pp_print_newline fmt ())
      in
      (cache, l, counter, to_be_checked), false
    )
  in
  let (cache, l, counter, to_be_checked), b =
    for_all_over_orbit ~trace ~fmt ~fmt_err ~sigs
      (get_positions ~fmt_err ~sigs agent site1 site2 rule)
      (sigma agent site1 site2)
      (sigma_inv agent site1 site2)
      (sigma_raw agent site1 site2)
      (sigma_raw_inv agent site1 site2)
      f rule accu
  in
  let get_weight hash =
    let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
    Rule_modes.RuleModeMap.map
      (fun rate ->
        weight ~correct:correct.(i) ~card_stabilizer:counter.(i) ~rate)
      rates.(i)
  in
  let rec aux w_ref l =
    match l with
    | [] -> true
    | h :: t ->
      if
        try
          let (), () =
            Rule_modes.RuleModeMap.monadic_fold2 () ()
              (fun () () _ w_ref w () ->
                match w_ref, w with
                | Some w_ref, Some w ->
                  if Affine_combinations.necessarily_equal w_ref w then
                    (), ()
                  else
                    raise False
                | None, _ | _, None -> raise False)
              (fun () () _ _ () -> raise False)
              (fun () () _ _ () -> raise False)
              w_ref (get_weight h) ()
          in
          true
        with False -> false
      then
        aux w_ref t
      else
        false
  in
  let good_rates =
    b
    &&
    match l with
    | [] -> true
    | h :: t -> aux (get_weight h) t
  in
  let () =
    List.iter
      (fun h ->
        let i = LKappa_auto.RuleCache.int_of_hashed_list h in
        counter.(i) <- 0;
        to_be_checked.(i) <- true)
      l
  in
  if good_rates then
    (cache, counter, to_be_checked), true
  else
    (cache, counter, to_be_checked), false

let weight ~correct ~card_stabilizer ~rate =
  Affine_combinations.div_scal rate (correct * card_stabilizer)

let check_orbit_internal_state_permutation ?trace ?fmt ?fmt_err ?sigs
    ~agent_type ~site1 ~site2 rule ~correct rates cache ~counter to_be_checked =
  check_orbit ~trace ~fmt ~fmt_err ~sigs
    ( potential_positions_for_swapping_internal_states,
      swap_internal_state_regular,
      swap_internal_state_regular,
      swap_internal_state_created,
      swap_internal_state_created )
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_orbit_binding_state_permutation ?trace ?fmt ?fmt_err ?sigs ~agent_type
    ~site1 ~site2 rule ~correct rates cache ~counter to_be_checked =
  check_orbit ~trace ~fmt ~fmt_err ~sigs
    ( potential_positions_for_swapping_binding_states,
      swap_binding_state_regular,
      swap_binding_state_regular,
      swap_binding_state_created,
      swap_binding_state_created )
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_orbit_full_permutation ?trace ?fmt ?fmt_err ?sigs ~agent_type ~site1
    ~site2 rule ~correct rates cache ~counter to_be_checked =
  check_orbit ~trace ~fmt ~fmt_err ~sigs
    ( potential_positions_for_swapping_full,
      swap_full_regular,
      swap_full_regular,
      swap_full_created,
      swap_full_created )
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_invariance ~trace ~fmt ~fmt_err ~sigs
    (get_positions, is_equal, is_equal_raw) agent_type site1 site2 rule cache =
  let _ = trace, fmt, sigs in
  for_all_elt_permutation ~fmt_err ~sigs
    (get_positions ~fmt_err ~sigs agent_type site1 site2 rule)
    is_equal is_equal_raw rule cache

let is_invariant_internal_states_permutation ?trace ?fmt ?fmt_err ?sigs
    ~agent_type ~site1 ~site2 rule cache =
  let _ = trace, fmt, sigs in
  let positions =
    potential_positions_for_swapping_internal_states ~fmt_err ~sigs agent_type
      site1 site2 rule
  in
  match positions with
  | [] -> cache, true
  | _ :: _ -> cache, false

let check_gen swap agent_type site1 site2 agent rule cache =
  let cache, hash = LKappa_auto.cannonic_form cache rule in
  let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
  let () = swap agent_type site1 site2 agent in
  let cache, hash' = LKappa_auto.cannonic_form cache rule in
  let i' = LKappa_auto.RuleCache.int_of_hashed_list hash' in
  let () = swap agent_type site1 site2 agent in
  cache, i = i'

let is_invariant_binding_states_permutation ?(trace : bool option)
    ?(fmt : Format.formatter option) ?fmt_err ?(sigs : Signature.s option)
    ~agent_type ~site1 ~site2 rule cache =
  check_invariance ~trace ~fmt ~fmt_err ~sigs
    ( potential_positions_for_swapping_binding_states,
      check_gen swap_binding_state_regular agent_type site1 site2,
      check_gen swap_binding_state_created agent_type site1 site2 )
    agent_type site1 site2 rule cache

let is_invariant_full_states_permutation ?trace ?fmt ?fmt_err ?sigs ~agent_type
    ~site1 ~site2 rule cache =
  let cache, b1 =
    is_invariant_internal_states_permutation ?trace ?fmt ?fmt_err ?sigs
      ~agent_type ~site1 ~site2 rule cache
  in
  if b1 then
    is_invariant_binding_states_permutation ?trace ?fmt ?fmt_err ?sigs
      ~agent_type ~site1 ~site2 rule cache
  else
    cache, false
