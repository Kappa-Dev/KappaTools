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

(******************************************************************************)


let is_empty (rule_tail,created_tail) =
  rule_tail = [] && created_tail = []

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
  in
  aux 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0 positions

let fold_over_orbit
    (positions:int list)
    (sigma:LKappa.rule_agent -> unit)
    (sigma_inv:LKappa.rule_agent -> unit)
    (sigma_raw:Raw_mixture.agent -> unit)
    (sigma_raw_inv:Raw_mixture.agent -> unit)
    (f:LKappa.rule -> 'a -> 'a option)
    (rule:LKappa.rule)
    (init:'a)  =
  let n = List.length positions in
  let counter = Array.make n false in
  let rec next agent_id rule_tail pos_id positions_tail residue =
    match positions_tail with
    | [] -> Some residue
    | pos_head::_
      when agent_id < pos_head ->
      let rule_tail = shift rule_tail in
      next (agent_id+1) rule_tail pos_id positions_tail residue
    | pos_head::pos_tail
      when agent_id = pos_head ->
      begin
        if
          counter.(pos_id)
        then
          let () = counter.(pos_id)<-false in
          let rule_tail = apply_head sigma_inv sigma_raw_inv rule_tail in
          next (agent_id+1) rule_tail (pos_id+1) pos_tail residue
        else
          let () = counter.(pos_id)<-true in
          let _ = apply_head sigma sigma_raw rule_tail in
          match f rule residue with
          | None -> None
          | Some residue ->
          next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0 positions residue
      end
    | pos_head::_
      when agent_id > pos_head ->
      let s1,i1,i2,i3 = __POS__ in
      let () =
        Format.fprintf Format.err_formatter
          "Internal bug: %s %i %i %i" s1 i1 i2 i3
      in
      let () = backtrack sigma_inv sigma_raw_inv counter positions rule in
      None
  in
  next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0 positions init
