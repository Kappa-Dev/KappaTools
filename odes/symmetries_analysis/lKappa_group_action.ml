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

let fold_over_orbit
    (positions:int list)
    (sigma:LKappa.rule_agent -> unit)
    (sigma_inv:LKappa.rule_agent -> unit)
    (sigma_raw:Raw_mixture.agent -> unit)
    (sigma_raw_inv:Raw_mixture.agent -> unit)
    (f:LKappa.rule -> 'a -> 'a)
    (rule:LKappa.rule)
    (init:'a)  =
  let n = List.length positions in
  let counter = Array.create n false in
  let rec next agent_id rule_tail pos_id positions_tail residue =
    match positions_tail with
    | [] -> residue
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
          let residue = f rule residue in
          next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0 positions residue
      end
    | pos_head::_
      when agent_id > pos_head ->
      let s1,i1,i2,i3 = __POS__ in
      let s = Printf.sprintf "%s %i %i %i" s1 i1 i2 i3 in
      raise (invalid_arg s)
  in
  next 0 (rule.LKappa.r_mix,rule.LKappa.r_created) 0 positions init
