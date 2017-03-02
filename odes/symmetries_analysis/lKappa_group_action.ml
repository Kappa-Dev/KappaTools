(** Swapping sites in regular (tested/modified/deleted) agents *)

let get_binding_precondition x = fst (fst x)
let get_binding_mod x = snd x

let care_binding_regular binding =
  (match get_binding_mod binding with
   | LKappa.Maintained | LKappa.Erased -> false
   | LKappa.Freed | LKappa.Linked _ -> true)
  ||
  (match get_binding_precondition binding with
   | Ast.LNK_VALUE _
   | Ast.FREE
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

let binding_equal a b =
  (get_binding_precondition a = get_binding_precondition b)
   &&
   (get_binding_mod a = get_binding_mod b)

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

let swap_internal_state_regular ag_type site1 site2 ag =
  let tmp = ag.LKappa.ra_ints.(site1) in
  let () = ag.LKappa.ra_ints.(site1) <- ag.LKappa.ra_ints.(site2) in
  let () = ag.LKappa.ra_ints.(site2) <- tmp in
  ()

let swap_binding_state_regular ag_type site1 site2 ag =
  let tmp = ag.LKappa.ra_ports.(site1) in
  let () = ag.LKappa.ra_ports.(site1) <- ag.LKappa.ra_ports.(site2) in
  let () = ag.LKappa.ra_ports.(site2) <- tmp in
  ()

(** Swapping sites in created agents *)

let may_swap_internal_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  &&
  (ag.Raw_mixture.a_ints.(site1) <> ag.Raw_mixture.a_ints.(site2))

let may_swap_binding_state_created ag_type site1 site2 ag =
  ag_type = ag.Raw_mixture.a_type
  &&
  (ag.Raw_mixture.a_ports.(site1) <>  ag.Raw_mixture.a_ports.(site2))

let swap_internal_state_created ag_type site1 site2 ag =
  let tmp = ag.Raw_mixture.a_ints.(site1) in
  let () = ag.Raw_mixture.a_ints.(site1) <- ag.Raw_mixture.a_ints.(site2) in
  let () = ag.Raw_mixture.a_ints.(site2) <- tmp in
  ()

let swap_binding_state_created ag_type site1 site2 ag =
  let tmp = ag.Raw_mixture.a_ports.(site1) in
  let () = ag.Raw_mixture.a_ports.(site1) <- ag.Raw_mixture.a_ports.(site2) in
  let () = ag.Raw_mixture.a_ports.(site2) <- tmp in
  ()

(*******************************************************************)

let of_rule rule = (rule.LKappa.r_mix,rule.LKappa.r_created)

let is_empty (rule_tail,created_tail) =
  rule_tail = [] && created_tail = []

let p_head p p_raw (rule_tail,created_tail) =
  match
    rule_tail, created_tail
  with
  | h::t,_ ->
    p h,(t,created_tail)
  | _,h::t ->
    p_raw h,(rule_tail, t)
  | [],[] ->
    let s1,i1,i2,i3 = __POS__ in
    let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    raise (invalid_arg s)

let apply_head sigma sigma_raw (rule_tail,created_tail) =
  match
    rule_tail, created_tail
  with
  | h::t,_ ->
    let () = sigma h in
    t,created_tail
  | _,h::t ->
    let () = sigma_raw h in
    rule_tail, t
  | [],[] ->
    let s1,i1,i2,i3 = __POS__ in
    let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
    raise (invalid_arg s)

let shift tail = apply_head ignore ignore tail

(***************************************************************)

let filter_positions p p_raw  rule =
  let rec aux pos_id rule_tail accu =
    if is_empty rule_tail
    then List.rev accu
    else
      let b,rule_tail = p_head p p_raw rule_tail in
      if b then
        aux (pos_id+1) rule_tail (pos_id::accu)
      else
        aux (pos_id+1) rule_tail accu
  in
  aux 0 (of_rule rule) []

let potential_positions_for_swapping_internal_states
    agent_type site1 site2 rule =
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

(******************************************************************)

let backtrack sigma_inv sigma_raw_inv counter positions rule =
  let rec aux agent_id rule_tail pos_id positions_tail =
    match positions_tail with
    | [] -> ()
    | pos_head::_
      when agent_id < pos_head ->
      let rule_tail = shift rule_tail in
      aux (agent_id+1) rule_tail pos_id positions_tail
    | pos_head::pos_tail
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
        aux (agent_id+1) rule_tail (pos_id+1) pos_tail
      end
    | _::pos_tail ->
      aux agent_id rule_tail (pos_id+1) pos_tail
  in
  aux 0 (of_rule rule) 0 positions

(***************************************************************)
(*SYMMETRIES*)
(***************************************************************)
  (*
  [Strongly symmetric model]:
  iff
  1. for any element i,j in I such that i different than j,
   we have ri different than rj;
  2. for any element i in I and any pair of permutation sigma in
  Gri , there exists an element j in I, such that:
  (a) sigma.ri = rj.
  (b) ki/[Li,Li] = kj/ [Lj ,Lj]
  where:
  i. Li is the left hand side of the rule ri,
  ii. Lj is the left hand side of the rule rj,
  iii. and for any site graph [E, E] denotes the number of
  automorphisms in the site graph E.
  *)



(***************************************************************)
(*We say that the rules have a symmetric action over the site x
  and the site y, whenever the the product between the number of
  automorphisms of the rule r and its kinetic rate k(r), divided by
  the number of automorphisms in the left hand side of the rule r,
  is the same for any pair of symmetric rules.*)


let for_all_over_orbit
    (positions:int list)
    (sigma:LKappa.rule_agent -> unit)
    (sigma_inv:LKappa.rule_agent -> unit)
    (sigma_raw:Raw_mixture.agent -> unit)
    (sigma_raw_inv:Raw_mixture.agent -> unit)
    (f:LKappa.rule -> 'a -> 'a * bool)
    (rule:LKappa.rule)
    (init:'a)  =
  let n = List.length positions in
  let counter = Array.make n false in
  let rec next agent_id rule_tail pos_id positions_tail accu =
    match positions_tail with
    | [] -> f rule accu
    | pos_head::_
      when agent_id < pos_head ->
      let rule_tail = shift rule_tail in
      next (agent_id+1) rule_tail pos_id positions_tail accu
    | pos_head::pos_tail
      when agent_id = pos_head ->
      begin
        if
          counter.(pos_id)
        then
          let () = counter.(pos_id)<-false in
          let rule_tail =
            apply_head sigma_inv sigma_raw_inv rule_tail
          in
          next (agent_id+1) rule_tail (pos_id+1) pos_tail accu
        else
          let () = counter.(pos_id)<-true in
          let _ = apply_head sigma sigma_raw rule_tail in
          let accu, b = f rule accu in
          if b
          then
            next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0
              positions accu
          else
            let () = backtrack sigma_inv sigma_raw_inv counter
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

let check_orbit
    (get_positions, sigma, sigma_inv, sigma_raw, sigma_raw_inv)
    weight agent site1 site2 rule correct rates cache counter
    to_be_checked =
  let size = Array.length to_be_checked in
  let accu = cache, [], counter, to_be_checked in
  let f rule (cache, l, counter, to_be_checked) =
    let cache, hash = LKappa_auto.cannonic_form cache rule in
    let i = LKappa_auto.RuleCache.int_of_hashed_list hash in
    if
      i<size
      && to_be_checked.(i)
    then
      begin
        let n = counter.(i) in
        let () = counter.(i) <- n+1 in
        if n = 0
        then
          (cache, hash::l, counter, to_be_checked), true
        else
          (cache, l, counter, to_be_checked), true
      end
    else
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
    | h::t ->
      if
        begin
          try
            let (),() =
              Rule_modes.RuleModeMap.monadic_fold2
                () ()
                (fun () () _ w_ref w () ->
                   if Alg_expr_extra.necessarily_equal w_ref w
                   then (),()
                   else raise False)
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
        | h::t ->
          aux (get_weight h) t
    end
  in
  let () =
    List.iter
      (fun h ->
         let i = LKappa_auto.RuleCache.int_of_hashed_list h in
         counter.(i)<-0; to_be_checked.(i)<-true) l
  in
  if good_rates then
    (cache,counter,to_be_checked), true
  else
    (cache, counter, to_be_checked), false

let weight ~correct ~card_stabilizer ~rate =
  Alg_expr_extra.get_corrected_rate
    (Alg_expr_extra.divide_expr_by_int
       rate
       (correct * card_stabilizer))

let check_orbit_internal_state_permutation
    ~agent_type ~site1 ~site2 rule ~correct rates cache ~counter
    to_be_checked =
check_orbit
  (potential_positions_for_swapping_internal_states,
   swap_internal_state_regular,
   swap_internal_state_regular,
   swap_internal_state_created,
   swap_internal_state_created)
  weight agent_type site1 site2 rule correct rates cache counter to_be_checked

let check_orbit_binding_state_permutation
    ~agent_type ~site1 ~site2 rule ~correct rates cache ~counter
    to_be_checked =
  check_orbit
    (potential_positions_for_swapping_binding_states,
     swap_binding_state_regular,
     swap_binding_state_regular,
     swap_binding_state_created,
     swap_binding_state_created)
    weight agent_type site1 site2 rule correct rates cache counter to_be_checked
