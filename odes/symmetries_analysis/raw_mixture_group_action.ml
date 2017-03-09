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
              normalize_internal_states_in_agent equiv_class
                agent_ints
           )
           agent.Raw_mixture.a_ints
           partition
       in
       {
         agent with
         Raw_mixture.a_ints = ag_ints
       }
    ) (List.rev raw_mixture)
  in
  raw_mixture

(**************************************************************)
(*check the states of agent are the same in the initial states*)

let normalize_equal_internal_states_in_agent equiv_class
    agent_ints =
  (*reverse the equivalence classes*)
  let equiv_class_rev = List.rev equiv_class in
  let asso =
    List.rev_map (fun x -> x, agent_ints.(x)) equiv_class_rev
  in
  let asso =
    List.fold_left (fun current_list (x, y) ->
        match y with
        | None -> current_list
        | Some y ->
          if x = y
          then x :: current_list
          else current_list
      ) [] asso
  in
  let asso' =
    List.rev_map2
      (fun k value -> (k,value))
      equiv_class_rev
      asso
  in
  let () =
    List.iter
      (fun (k,value) -> agent_ints.(k)<-(Some value))
      asso'
  in
  agent_ints

let normalize_internal_states_in_raw_mixture_init symmetries
    init_raw_mixture =
  let raw_mixture =
    List.rev_map (fun agent ->
        let agent_type = agent.Raw_mixture.a_type in
        (*get the internal states in the symmetries*)
        let partition =
          try fst (symmetries.(agent_type)) with _ -> []
        in
        (*get all the internal states of each equiv_class
        that have their states are equal*)
        let ag_ints_equal =
          List.fold_left (fun agent_ints equiv_class ->
              normalize_equal_internal_states_in_agent
                equiv_class (*int list list*)
                agent_ints (*int option array*)
            ) agent.Raw_mixture.a_ints partition
        in
        {
          agent with
          Raw_mixture.a_ints = ag_ints_equal
        }
      ) (List.rev init_raw_mixture)
  in
  raw_mixture
