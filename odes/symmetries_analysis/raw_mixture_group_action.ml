let get_internal_state_partition (a,_) = a
let get_binding_state_partition (_,a) = a

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
    get_internal_state_partition
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
    Mods.IntMap.add i [b] map, a
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

let refine_class equiv_class agent output =
  match equiv_class with
  | [] -> output
  | h::t ->
    begin
      let rec aux ref_value to_do current_class output =
        match to_do with
        | [] ->
          begin
            match ref_value with
            | None -> output
            | Some _ -> current_class::output
          end
        | h::t ->
          if agent.(h) = ref_value
          then
            aux ref_value t (h::current_class) output
          else
            match ref_value with
              | None -> aux (agent.(h)) t [h] output
              | Some _ -> aux (agent.(h)) t [h] (current_class::output)

      in
      aux (agent.(h)) t [h] output
    end

let refine_partition which symmetries refined_raw_mixture =
  List.rev_map
    (fun (agent,agent') ->
       let ag_type = agent.Raw_mixture.a_type in
       List.fold_left
         (fun output equiv_class ->
            refine_class equiv_class agent' output)
         [] (which symmetries.(ag_type))
    )
    (List.rev refined_raw_mixture)

let apply_permutation get set perm agent =
  let assign =
    List.rev_map
      (fun (i,j) ->
         (j,get i agent))
      perm
  in
  let () = List.iter (fun (j,data) -> set j data agent) assign in
  agent

let apply_permutation_inv get set perm agent =
  let perm_inv =
    List.rev_map (fun (a,b) -> (b,a)) perm
  in
  apply_permutation get set perm_inv agent

let rec fold_symmetries_over_agent get set f covering agent accu =
  match covering with
  | h::t ->
    Tools.fold_over_permutations
      (fun perm accu ->
         let perm = List.rev_map2 (fun a b -> (a,b)) h perm in
         let agent = apply_permutation get set perm agent in
         let accu = fold_symmetries_over_agent get set f t agent accu  in
         let _ = apply_permutation_inv get set perm agent in
         accu)
      h accu
  | [] -> f agent accu

let fold_symmetries_over_raw_mixture
    get set f raw_mixture covering_list accu =
  let raw_mixture0 = raw_mixture in
  let rec aux get set f raw_mixture covering_list accu =
    match raw_mixture,covering_list
    with
    | [], [] -> f raw_mixture0 accu
    | _::_, [] | [],_::_ ->
      raise
        (ExceptionDefn.Internal_Error
           ("Arguments of fold_symmetries_over_rw_mixture shall have the same length",
            Locality.dummy))
    | h::t, h'::t' ->
      fold_symmetries_over_agent
        get set
        (fun agent accu -> aux get set f t t' accu)
        h' h accu
  in aux get set f raw_mixture covering_list accu

let lkappa_of_raw_mixture raw_mixture =
  {
    LKappa.r_mix =  [];
    LKappa.r_created = raw_mixture ;
    LKappa.r_delta_tokens = [] ;
    LKappa.r_rate = Alg_expr.int 0 ;
    LKappa.r_un_rate = None  ;
  }

let copy raw_mixture =
  List.rev_map
    (fun agents ->
       {agents with Raw_mixture.a_ints = Array.copy agents.Raw_mixture.a_ints ;
                    Raw_mixture.a_ports = Array.copy agents.Raw_mixture.a_ports })
    (List.rev raw_mixture)


let
  normalize_binding_states rule_cache symmetries raw_mixture
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
      get_binding_state_partition
      symmetries refined_raw_mixture
  in
  let covering_list =
    refine_partition
      get_binding_state_partition
      symmetries
      refined_raw_mixture
  in
  let raw_mixture = remove_binding_state refined_raw_mixture in
  let rule_cache, hash =
    LKappa_auto.cannonic_form rule_cache (lkappa_of_raw_mixture raw_mixture)
  in
  let rule_cache, (_,raw_mixture) =
    fold_symmetries_over_raw_mixture
      (fun i agent -> agent.Raw_mixture.a_ports.(i))
      (fun i data agent -> agent.Raw_mixture.a_ports.(i)<-data)
      (fun raw_mixture (rule_cache,(best_hash,best_raw_mixture)) ->
         let rule_cache, hash =
           LKappa_auto.cannonic_form
             rule_cache
             (lkappa_of_raw_mixture raw_mixture)
         in
         if compare hash best_hash < 0
         then
           (rule_cache, (hash, copy raw_mixture))
         else
           (rule_cache, (best_hash, best_raw_mixture)))
      raw_mixture
      covering_list
      (rule_cache, (hash, copy raw_mixture))
  in
  rule_cache, raw_mixture


let normalize rule_cache symmetries raw_mixture =
  let raw_mixture = normalize_internal_states symmetries raw_mixture in
    normalize_binding_states rule_cache symmetries raw_mixture


(**************************************************************)
(*check for each agent, that each pair of sites in a same equivalence
  class carry the same internal state *)
(* The function shall output a Boolean *)
(* The function shall have no side effect *)


let check_symmetries_of_internal_states_in_agent partition agent_ints =
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
