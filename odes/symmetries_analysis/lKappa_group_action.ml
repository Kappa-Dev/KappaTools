(**
   * LKappa_group_action.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <Jun 08 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let get_binding_precondition x = fst (fst x)

let get_binding_mod x = snd x

let binding_equal ((a_t,_),a_m) ((b_t,_),b_m) =
  a_t = b_t && a_m = b_m

let care_binding_regular ((test,_),mods) =
  (match mods with
   | LKappa.Maintained | LKappa.Erased -> false
   | LKappa.Freed | LKappa.Linked _ -> true)
  ||
  (match test with
   | Ast.LNK_VALUE _
   | Ast.LNK_FREE
   | Ast.ANY_FREE
   | Ast.LNK_SOME
   | Ast.LNK_TYPE _ -> true
   | Ast.LNK_ANY -> false
  )

let care_internal_regular =
    function
    | LKappa.I_ANY
    | LKappa.I_ANY_ERASED -> false
    | LKappa.I_ANY_CHANGED _
    | LKappa.I_VAL_CHANGED _
    | LKappa.I_VAL_ERASED _ -> true

let may_swap_internal_state_regular ag_type site1 site2 ag =
  ag_type = ag.LKappa.ra_type
  &&
  (
    care_internal_regular ag.LKappa.ra_ints.(site1)
   ||
    care_internal_regular ag.LKappa.ra_ints.(site2)
  )
  &&
  (ag.LKappa.ra_ints.(site1) <> ag.LKappa.ra_ints.(site2))

let may_swap_binding_state_regular ag_type site1 site2 ag =
  ag_type = ag.LKappa.ra_type
  &&
  (
    care_binding_regular ag.LKappa.ra_ports.(site1)
    ||
    care_binding_regular ag.LKappa.ra_ports.(site2)
  )
  &&
  (not (binding_equal ag.LKappa.ra_ports.(site1) ag.LKappa.ra_ports.(site2)))

let may_swap_full_regular ag_type site1 site2 ag =
  may_swap_binding_state_regular ag_type site1 site2 ag
  || may_swap_internal_state_regular ag_type site1 site2 ag

let may_swap_internal_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  &&
  (ag.Raw_mixture.a_ints.(site1) <> ag.Raw_mixture.a_ints.(site2))

let may_swap_binding_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  &&
  (ag.Raw_mixture.a_ports.(site1) <> ag.Raw_mixture.a_ports.(site2))

let may_swap_full_created ag_type site1 site2 ag =
  may_swap_internal_state_created ag_type site1 site2 ag
  || may_swap_binding_state_created ag_type site1 site2 ag

let of_rule rule = (rule.LKappa.r_mix, rule.LKappa.r_created)

let is_empty (rule_tail,created_tail) =
  rule_tail = [] && created_tail = []

let p_head p p_raw (rule_tail,created_tail) =
  match rule_tail, created_tail with
  | h :: t, _ -> p h, (t, created_tail)
  | _, h :: t -> p_raw h, (rule_tail, t)
  | [], [] ->
    let s1,i1,i2,i3 = __POS__ in
    let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    raise (invalid_arg s)

let apply_head sigma sigma_raw (rule_tail,created_tail) =
  match rule_tail, created_tail with
  | h::t, _ ->
    let () = sigma h in
    t, created_tail
  | _, h::t ->
    let () = sigma_raw h in
    rule_tail, t
  | [], [] ->
    let s1,i1,i2,i3 = __POS__ in
    let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    raise (invalid_arg s)

let apply_head_predicate f f_raw cache (rule_tail,created_tail) rule =
  match rule_tail, created_tail with
  | h::_, _ ->
    f h rule cache
  | _, h::_ ->
    f_raw h rule cache
  | [], [] ->
    let s1,i1,i2,i3 = __POS__ in
    let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    raise (invalid_arg s)

let shift tail = apply_head ignore ignore tail

(***************************************************************)

let filter_positions p p_raw rule =
  let rec aux pos_id rule_tail accu =
    if is_empty rule_tail
    then List.rev accu
    else
      let b, rule_tail = p_head p p_raw rule_tail in
      if b then
        aux (pos_id + 1) rule_tail (pos_id :: accu)
      else
        aux (pos_id + 1) rule_tail accu
  in
  aux 0 (of_rule rule) []

let potential_positions_for_swapping_internal_states
    agent_type site1 site2 rule : int list =
  filter_positions
    (may_swap_internal_state_regular agent_type site1 site2)
    (may_swap_internal_state_created agent_type site1 site2)
    rule

let potential_positions_for_swapping_binding_states
    agent_type site1 site2 rule =
  filter_positions
    (may_swap_binding_state_regular agent_type site1 site2)
    (may_swap_binding_state_created agent_type site1 site2)
    rule

let potential_positions_for_swapping_full
    agent_type site1 site2 rule =
  filter_positions
    (may_swap_full_regular agent_type site1 site2)
    (may_swap_full_created agent_type site1 site2)
    rule

(******************************************************************)

let backtrack sigma_inv sigma_raw_inv counter positions rule =
  let rec aux agent_id rule_tail pos_id positions_tail =
    match positions_tail with
    | [] -> ()
    | pos_head :: _
      when agent_id < pos_head ->
      let rule_tail = shift rule_tail in
      aux (agent_id + 1) rule_tail pos_id positions_tail
    | pos_head :: pos_tail
      when agent_id = pos_head ->
      begin
        let rule_tail =
          if
            counter.(pos_id)
          then
            apply_head sigma_inv sigma_raw_inv rule_tail
          else
          shift rule_tail
        in
        aux (agent_id + 1) rule_tail (pos_id + 1) pos_tail
      end
    | _ :: pos_tail ->
      aux agent_id rule_tail (pos_id + 1) pos_tail
  in
  aux 0 (of_rule rule) 0 positions

(***************************************************************)
(*SYMMETRIES*)
(***************************************************************)

let for_all_elt_permutation
    (positions:int list)
    (f:LKappa.rule_agent -> LKappa.rule -> 'a -> 'a * bool )
    (f_raw:Raw_mixture.agent -> LKappa.rule -> 'a -> 'a * bool)
    (rule:LKappa.rule)
    (init:'a)  =
  let rec next agent_id rule_tail pos_id positions_tail accu =
    match positions_tail with
    | [] -> accu, true
    | pos_head :: _
      when agent_id < pos_head ->
      let rule_tail = shift rule_tail in
      next (agent_id + 1) rule_tail pos_id positions_tail accu
    | pos_head :: pos_tail
      when agent_id = pos_head ->
      begin
        match apply_head_predicate
                f f_raw
                accu
                rule_tail rule
        with

        | accu, false -> accu, false

        | accu, true ->
          let rule_tail = shift rule_tail in
          next (agent_id + 1) rule_tail (pos_id + 1) pos_tail accu
      end
    | _::_
      (*when agent_id > pos_head*) ->
      let s1,i1,i2,i3 = __POS__ in
      let () =
        Format.fprintf Format.err_formatter
          "Internal bug: %s %i %i %i" s1 i1 i2 i3
      in
      accu, false
  in
  next 0 (of_rule rule) 0 positions init

let for_all_over_orbit
    (positions:int list)
    (sigma:LKappa.rule_agent -> unit)
    (sigma_inv:LKappa.rule_agent -> unit)
    (sigma_raw:Raw_mixture.agent -> unit)
    (sigma_raw_inv:Raw_mixture.agent -> unit)
    (f: LKappa.rule -> 'a -> 'a * bool)
    (rule:LKappa.rule)
    (init:'a) : 'a * bool =
    let n = List.length positions in
    let counter = Array.make n false in
    let rec next agent_id rule_tail pos_id positions_tail accu =
      match positions_tail with
      | [] -> f rule accu
      | pos_head :: _
        when agent_id < pos_head ->
        let rule_tail = shift rule_tail in
        next (agent_id + 1) rule_tail pos_id positions_tail accu
      | pos_head :: pos_tail
        when agent_id = pos_head ->
        begin
          if
            counter.(pos_id)
          then
            let () = counter.(pos_id) <- false in
            let rule_tail =
              apply_head sigma_inv sigma_raw_inv rule_tail
            in
            next (agent_id + 1) rule_tail (pos_id + 1) pos_tail accu
          else
            let () = counter.(pos_id) <- true in
            let _ = apply_head sigma sigma_raw rule_tail in
            let accu, b = f rule accu in
            if b
            then
              next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0
                positions accu
            else
              let () =
                backtrack sigma_inv sigma_raw_inv counter
                  positions rule
              in
              accu, false
        end
      | _::_
        (*when agent_id > pos_head*) ->
        let s1,i1,i2,i3 = __POS__ in
        let () =
          Format.fprintf Format.err_formatter
            "Internal bug: %s %i %i %i" s1 i1 i2 i3
        in
        let () =
          backtrack sigma_inv sigma_raw_inv counter positions rule
        in
        accu, false
    in
    next 0 (of_rule rule) 0 positions init

exception False

let do_print ?parameters ?sigs f =
  match parameters,sigs with
| Some parameters, Some sigs ->
  begin
    if Remanent_parameters.get_trace parameters
    then
      let logger = Remanent_parameters.get_logger parameters in
      let fmt_opt = Loggers.formatter_of_logger logger in
      match fmt_opt with
      | None -> ()
      | Some fmt -> f sigs logger fmt
  end
| None, _ | _, None  -> ()

(*
get_positions: (int -> int -> int -> LKappa.rule -> int list)
*)
let check_orbit
    ?parameters ?sigs
    (get_positions, sigma, sigma_inv, sigma_raw, sigma_raw_inv)
    weight agent site1 site2 rule correct rates cache counter
    to_be_checked : (LKappa_auto.cache * int array * bool array) * bool =
  let () =
    do_print ?parameters ?sigs
      (fun sigs logger fmt ->
          let () = Loggers.fprintf logger "Check an orbit" in
          let () = Loggers.print_newline logger in
          let () = Loggers.fprintf logger "Permutation of the sites " in
          let () = Signature.print_site sigs agent fmt site1 in
          let () = Loggers.fprintf logger " and " in
          let () = Signature.print_site sigs agent fmt site2 in
          let () = Loggers.fprintf logger " in agent of type  " in
          let () = Signature.print_agent sigs fmt agent in
          let () = Loggers.print_newline logger in
          let () = Loggers.fprintf logger " rule:   " in
          let () =
            LKappa.print_rule
              ~full:true
              sigs
              (fun _ _ -> ()) (fun _ _ -> ())
              fmt rule
          in
          let () = Loggers.print_newline logger in
          ()
      )
  in
  let size = Array.length to_be_checked in
  let accu = cache, [], counter, to_be_checked in
  let f rule (cache, l, counter, to_be_checked) =
    let () =
      do_print ?parameters ?sigs
        (fun sigs logger fmt ->
            let () = Loggers.fprintf logger " rule:   " in
            let () =
              LKappa.print_rule
                ~full:true
                sigs
                (fun _ _ -> ()) (fun _ _ -> ())
                fmt rule
            in
            let () = Loggers.print_newline logger in
            ())
    in
    let cache, hash = LKappa_auto.cannonic_form cache rule in
    let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
    if i < size && to_be_checked.(i)
    then
      begin
        let () =
          do_print ?parameters ?sigs
            (fun _ logger _ ->
               let () = Loggers.fprintf logger "Existing rule" in
               Loggers.print_newline logger)
        in
        let n = counter.(i) in
        let () = counter.(i) <- n+1 in
        if n = 0
        then
          (cache, hash::l, counter, to_be_checked), true
        else
          (cache, l, counter, to_be_checked), true
      end
    else
    let () =
      do_print ?parameters ?sigs
        (fun _ logger _ ->
           let () = Loggers.fprintf logger "Unknown rule" in
           Loggers.print_newline logger)
    in
      (cache, l, counter, to_be_checked), false
  in
  let (cache, l, counter, to_be_checked), b =
    for_all_over_orbit
      (get_positions agent site1 site2 rule)
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
         weight
           ~correct:(correct.(i))
           ~card_stabilizer:(counter.(i))
           ~rate)
      (rates.(i))
  in
  let rec aux w_ref l =
    match l with
    | [] -> true
    | h :: t ->
      if
        begin
          try
            let (), () =
              Rule_modes.RuleModeMap.monadic_fold2
                () ()
                (fun () () _ w_ref w () ->
                    match w_ref, w with
                        Some w_ref,Some w ->
                        if Affine_combinations.necessarily_equal w_ref w
                        then (),()
                        else raise False
                      | None,_ | _,None -> raise False)
                (fun () () _ _ () -> raise False)
                (fun () () _ _ () -> raise False)
                w_ref
                (get_weight h)
                ()
            in true
          with
          | False -> false
        end
      then
        aux w_ref t
      else
        false
  in
  let good_rates =
    b &&
    begin
      match l with
        | [] -> true
        | h :: t -> aux (get_weight h) t
    end
  in
  let () =
    List.iter
      (fun h ->
         let i = LKappa_auto.RuleCache.int_of_hashed_list h in
         counter.(i) <- 0; to_be_checked.(i) <- true) l
  in
  if good_rates then
    (cache, counter, to_be_checked), true
  else
    (cache, counter, to_be_checked), false

let weight ~correct ~card_stabilizer ~rate =
  Affine_combinations.div_scal
          rate (correct * card_stabilizer)

let check_orbit_internal_state_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2 rule ~correct rates cache
    ~counter to_be_checked =
  check_orbit
    ?parameters ?sigs
    (potential_positions_for_swapping_internal_states,
     LKappa_auto.swap_internal_state_regular,
     LKappa_auto.swap_internal_state_regular,
     LKappa_auto.swap_internal_state_created,
     LKappa_auto.swap_internal_state_created)
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_orbit_binding_state_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2 rule ~correct rates cache
    ~counter to_be_checked =
  check_orbit
    ?parameters ?sigs
    (potential_positions_for_swapping_binding_states,
     LKappa_auto.swap_binding_state_regular,
     LKappa_auto.swap_binding_state_regular,
     LKappa_auto.swap_binding_state_created,
     LKappa_auto.swap_binding_state_created)
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_orbit_full_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2 rule ~correct rates cache
    ~counter to_be_checked =
  check_orbit
    ?parameters ?sigs
    (potential_positions_for_swapping_full,
     LKappa_auto.swap_full_regular,
     LKappa_auto.swap_full_regular,
     LKappa_auto.swap_full_created,
     LKappa_auto.swap_full_created)
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_invariance
    ?parameters ?sigs
    (get_positions, is_equal, is_equal_raw)
    agent_type site1 site2 rule cache
  =
  let _ = parameters,sigs in
  for_all_elt_permutation
    (get_positions agent_type site1 site2 rule)
    is_equal
    is_equal_raw
    rule
    cache

let is_invariant_internal_states_permutation
    ?parameters
    ?sigs
    ~agent_type ~site1 ~site2 rule cache =
  let _ = sigs,parameters in
  let positions =
    potential_positions_for_swapping_internal_states
      agent_type site1 site2 rule
  in
  match positions with
  | [] -> cache, true
  | _::_ -> cache, false

let check_gen swap agent_type site1 site2 agent rule cache =
  let cache, hash = LKappa_auto.cannonic_form cache rule in
  let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
  let () =
    swap
      agent_type site1 site2
      agent
  in
  let cache, hash' = LKappa_auto.cannonic_form cache rule in
  let i' = LKappa_auto.RuleCache.int_of_hashed_list hash' in
  let () =
    swap
      agent_type site1 site2
      agent
  in
  cache, i=i'

let is_invariant_binding_states_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2
    rule cache =
  check_invariance
    ?parameters ?sigs
    (potential_positions_for_swapping_binding_states,
     (check_gen
        LKappa_auto.swap_binding_state_regular
        agent_type
        site1
        site2),
     (check_gen LKappa_auto.swap_binding_state_created agent_type site1 site2))
    agent_type site1 site2 rule cache

let is_invariant_full_states_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2
    rule cache =
  let cache, b1 =
    is_invariant_internal_states_permutation
      ?parameters ?sigs ~agent_type ~site1 ~site2
      rule cache
  in
  if b1 then
    is_invariant_binding_states_permutation
    ?parameters ?sigs ~agent_type ~site1 ~site2
    rule cache
  else
    cache, false
