let normalize_in_agent_gen
    (get:int -> 'a -> 'b)
    (set:int -> 'b -> 'a -> unit)
    (cmp: 'b -> 'b -> int)
    equiv_class agent =
  let asso =
    List.rev_map
      (fun x -> x, get x agent)
      equiv_class
  in
  let asso =
    List.sort
      (fun (_,x) (_,y) -> cmp x y)
      asso
  in
  let asso' =
    List.rev_map2
      (fun k (_,value) -> (k,value))
      equiv_class
      asso
  in
  let () =
    List.iter
      (fun (k,value) -> set k value agent)
      asso'
  in
  agent


let normalize_gen
    get_type get set cmp which symmetries raw_mixture =
  let raw_mixture =
    List.rev_map
    (fun agent ->
      let agent_type = get_type agent in
      let partition =
         try
           which (symmetries.(agent_type))
         with
           _ -> []
       in
       let agent =
         List.fold_left
           (fun agent equiv_class ->
              normalize_in_agent_gen get set cmp equiv_class agent
           )
           agent
           partition
       in
       agent
    ) (List.rev raw_mixture)
  in
  raw_mixture

let
  normalize_internal_states equiv_class raw_mixture
  =
  normalize_gen
    (fun agent -> agent.Raw_mixture.a_type)
    (fun i agent -> agent.Raw_mixture.a_ints.(i))
    (fun i data agent -> agent.Raw_mixture.a_ints.(i)<-data)
    compare
    fst
    equiv_class raw_mixture


let add i j map =
  let old =
    Mods.IntMap.find_default [] i map
  in
  Mods.IntMap.add i (j::old) map

let pop i map =
  match
    Mods.IntMap.find_option i map
  with
  | None ->
    raise
      (ExceptionDefn.Internal_Error ("Illegal map", Locality.dummy))
  | Some [a;b] ->
    Mods.IntMap.add i [a] map, b
  | Some [a] ->
    Mods.IntMap.remove i map,a
  | Some _ ->
    raise
      (ExceptionDefn.Internal_Error ("Illegal map", Locality.dummy))

let enrich_binding_state raw_mixture =
  let map =
    List.fold_left
    (fun map agent ->
       let agent_type = agent.Raw_mixture.a_type in
       let bonds = agent.Raw_mixture.a_ports in
       Tools.array_fold_lefti
         (fun site map port ->
            match port
            with
            | Raw_mixture.FREE -> map
            | Raw_mixture.VAL i -> add i (agent_type,site) map)
         map bonds
    )
    Mods.IntMap.empty
    raw_mixture
  in
  let refined_raw_mixture_rev, map  =
    List.fold_left
      (fun (list,map) agent ->
         let array = Array.make (Array.length agent.Raw_mixture.a_ports) None in
         let map =
           Tools.array_fold_lefti
             (fun site map port ->
                match port with
                | Raw_mixture.FREE -> map
                | Raw_mixture.VAL i ->
                  let map, binding_type = pop i map in
                  let () = array.(site) <- Some binding_type in
                  map)
             map
             agent.Raw_mixture.a_ports
         in
         ((agent,array)::list,map))
      ([],map)
      raw_mixture
  in
  let () =
    if not (Mods.IntMap.is_empty map)
    then
    raise
      (ExceptionDefn.Internal_Error ("Illegal map", Locality.dummy))
  in
  List.rev refined_raw_mixture_rev

let remove_binding_state refined_raw_mixture =
  List.rev_map fst (List.rev refined_raw_mixture)

let
  normalize_binding_states equiv_class raw_mixture
  =
  let refined_raw_mixture = enrich_binding_state raw_mixture in
  let refined_raw_mixture =
    normalize_gen
      (fun (agent,_) -> agent.Raw_mixture.a_type)
      (fun i (agent,agent') ->
         agent.Raw_mixture.a_ports.(i),
         agent'.(i))
      (fun i (data,data') (agent,agent') ->
         agent.Raw_mixture.a_ports.(i)<-data;
         agent'.(i)<-data')
      (fun (_,a) (_,b) -> compare a b)
      snd
      equiv_class refined_raw_mixture 
  in
  (* to do *)
  remove_binding_state refined_raw_mixture

let normalize equiv_class raw_mixture =
  let raw_mixture = normalize_binding_states equiv_class raw_mixture in
  normalize_internal_states equiv_class raw_mixture
(**************************************************************)
(*check for each agent, that each pair of sites in a same equivalence
  class carry the same internal state *)
(* The function shall output a Boolean *)
(* The function shall have no side effect *)


let check_symmetries_of_internal_states_in_agent equiv_class agent_ints =
  (*reverse the equivalence classes*)
  (*  let equiv_class_rev = List.rev equiv_class in
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
  in *)
  true (* to do *)


let check_symmetries_of_internal_states partition  init_raw_mixture =
  (*  let raw_mixture =
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
  in*)
  true (* to do *)
