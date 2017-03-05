let normalize_internal_states_in_agent equiv_class agent_ints =
  let equiv_class_rev = List.rev equiv_class in
  let asso =
    List.rev_map
      (fun x -> x, agent_ints.(x))
      equiv_class_rev
  in
  let asso =
    List.sort
      (fun (_,x) (_,y) -> compare x y)
      asso
  in
  let asso' =
    List.rev_map2
      (fun k (_,value) -> (k,value))
      equiv_class_rev
      asso
  in
  let () =
    List.iter
      (fun (k,value) -> agent_ints.(k)<-value)
      asso'
  in
  agent_ints

let normalize_internal_states_in_raw_mixture
    symmetries raw_mixture =
  let raw_mixture =
    List.rev_map
    (fun agent ->
       let agent_type = agent.Raw_mixture.a_type in
       let partition =
         try
           fst (symmetries.(agent_type))
         with
           _ -> []
       in
       let ag_ints =
         List.fold_left
           (fun agent_ints equiv_class ->
              normalize_internal_states_in_agent equiv_class agent_ints
           )
           agent.Raw_mixture.a_ints
           partition
       in
       {agent with Raw_mixture.a_ints = ag_ints})
    (List.rev raw_mixture)
  in
  raw_mixture
