type 'a cleanable_array = { array: 'a array; default: 'a; list: int list }

let set parameters error ~pos i im array =
  try
    let old = array.array.(i) in
    let () = array.array.(i) <- im in
    match old = array.default, im = array.default with
    | true, false -> error, { array with list = i :: array.list }
    | _ -> error, array
  with err -> Exception.warn parameters error pos err array

let get parameters error ~pos i array =
  try error, array.array.(i)
  with err -> Exception.warn parameters error pos err array.default

let clean array =
  List.iter (fun i -> array.array.(i) <- array.default) array.list

let origin =
  { Remanent_state.fwd = 0; Remanent_state.bwd = 0; Remanent_state.total = 0 }

let init_blackboard nrules nvars =
  let n = nrules + nvars in
  {
    Remanent_state.blackboard_distance = Array.make n None;
    Remanent_state.blackboard_is_done = Array.make n false;
    Remanent_state.blackboard_to_be_explored = Array.make n false;
  }

type remanent_state = {
  waiting_list: int list;
  next_round: int list;
  distance: Remanent_state.distance option cleanable_array;
  is_done: bool cleanable_array;
  to_be_explored: bool array;
  influence_map: Remanent_state.internal_influence_map;
}

let is_empty remanent_state =
  remanent_state.waiting_list = [] && remanent_state.next_round = []

let rec pop ~pos parameters error remanent_state =
  match remanent_state.waiting_list with
  | head :: tail -> error, (head, { remanent_state with waiting_list = tail })
  | [] ->
    (match remanent_state.next_round with
    | [] ->
      Exception.warn parameters error pos
        (Invalid_argument "empty waiting list") (0, remanent_state)
    | _ ->
      pop ~pos parameters error
        {
          remanent_state with
          waiting_list = remanent_state.next_round;
          next_round = [];
        })

let clean_remanent_state remanent_state =
  let () = clean remanent_state.distance in
  let () = clean remanent_state.is_done in
  ()

let go_fwd distance =
  {
    distance with
    Remanent_state.fwd = succ distance.Remanent_state.fwd;
    Remanent_state.total = succ distance.Remanent_state.total;
  }

let go_bwd distance =
  {
    distance with
    Remanent_state.bwd = succ distance.Remanent_state.bwd;
    Remanent_state.total = succ distance.Remanent_state.total;
  }

let leq int int_ref_opt =
  match int_ref_opt with
  | None -> true
  | Some int_ref -> compare int int_ref <= 0

let has_improved ~new_distance ~old_distance_opt =
  match old_distance_opt with
  | None -> true
  | Some old_distance ->
    new_distance.Remanent_state.fwd < old_distance.Remanent_state.fwd
    || new_distance.Remanent_state.bwd < old_distance.Remanent_state.bwd
    || new_distance.Remanent_state.total < old_distance.Remanent_state.total

let best_distance new_distance old_distance_opt =
  match old_distance_opt with
  | None -> new_distance
  | Some old_distance ->
    {
      Remanent_state.fwd =
        min new_distance.Remanent_state.fwd old_distance.Remanent_state.fwd;
      Remanent_state.bwd =
        min new_distance.Remanent_state.bwd old_distance.Remanent_state.bwd;
      total =
        min new_distance.Remanent_state.total old_distance.Remanent_state.total;
    }

let add_node source remanent_state =
  let nodes, pos, neg = remanent_state.influence_map in
  let nodes = Ckappa_sig.rule_id_of_int source :: nodes in
  let influence_map = nodes, pos, neg in
  { remanent_state with influence_map }

let add_influence source target label influence_map =
  Ckappa_sig.PairRule_setmap.Map.add
    (Ckappa_sig.rule_id_of_int source, Ckappa_sig.rule_id_of_int target)
    label influence_map

let add_positive_influence source target label remanent_state =
  let nodes, pos, neg = remanent_state.influence_map in
  let pos = add_influence source target label pos in
  let influence_map = nodes, pos, neg in
  { remanent_state with influence_map }

let add_negative_influence source target label remanent_state =
  let nodes, pos, neg = remanent_state.influence_map in
  let neg = add_influence source target label neg in
  let influence_map = nodes, pos, neg in
  { remanent_state with influence_map }

let add_rev f source target label remanent_state =
  f target source label remanent_state

let explore_one_map good_distance add_influence parameters error remanent_state
    source is_done new_distance list =
  List.fold_left
    (fun (error, remanent_state) (target, label) ->
      let target = Ckappa_sig.int_of_rule_id target in
      let error, target_done =
        get parameters error ~pos:__POS__ target remanent_state.is_done
      in
      let remanent_state =
        match is_done, target_done || target = source with
        | false, true -> add_influence source target label remanent_state
        | _ -> remanent_state
      in
      if good_distance new_distance then (
        let error, old_distance_opt =
          get parameters error ~pos:__POS__ target remanent_state.distance
        in
        let new_distance = best_distance new_distance old_distance_opt in
        if
          good_distance new_distance
          && has_improved ~new_distance ~old_distance_opt
        then (
          let error, distance =
            set parameters error ~pos:__POS__ target (Some new_distance)
              remanent_state.distance
          in
          let to_be_explored = remanent_state.to_be_explored in
          let next_round = remanent_state.next_round in
          let next_round, to_be_explored =
            if to_be_explored.(target) then
              next_round, to_be_explored
            else (
              let () = to_be_explored.(target) <- true in
              target :: next_round, to_be_explored
            )
          in
          error, { remanent_state with distance; next_round; to_be_explored }
        ) else
          error, remanent_state
      ) else
        error, remanent_state)
    (error, remanent_state) list

let visit parameters error node distance remanent_state good_distance
    bidirectional_influence_map =
  let (error, is_done), pos_fwd, pos_bwd, neg_fwd, neg_bwd =
    ( get parameters error ~pos:__POS__ node remanent_state.is_done,
      bidirectional_influence_map.Remanent_state.positive_influence_fwd.(node),
      bidirectional_influence_map.Remanent_state.positive_influence_bwd.(node),
      bidirectional_influence_map.Remanent_state.negative_influence_fwd.(node),
      bidirectional_influence_map.Remanent_state.negative_influence_bwd.(node) )
  in
  let distance_next = go_fwd distance in
  let distance_prev = go_bwd distance in
  let remanent_state =
    if is_done then
      remanent_state
    else
      add_node node remanent_state
  in
  let error, remanent_state =
    explore_one_map good_distance add_positive_influence parameters error
      remanent_state node is_done distance_next pos_fwd
  in
  let error, remanent_state =
    explore_one_map good_distance add_negative_influence parameters error
      remanent_state node is_done distance_next neg_fwd
  in
  let error, remanent_state =
    explore_one_map good_distance
      (add_rev add_positive_influence)
      parameters error remanent_state node is_done distance_prev pos_bwd
  in
  let error, remanent_state =
    explore_one_map good_distance
      (add_rev add_negative_influence)
      parameters error remanent_state node is_done distance_next neg_bwd
  in
  let error, is_done =
    set parameters error ~pos:__POS__ node true remanent_state.is_done
  in
  let error, to_be_explored =
    try
      let () = remanent_state.to_be_explored.(node) <- false in
      error, remanent_state.to_be_explored
    with _ ->
      Exception.warn parameters error __POS__ Exit remanent_state.to_be_explored
  in
  error, { remanent_state with is_done; to_be_explored }

let explore_influence_map ?fwd ?bwd ~total parameters error blackboard
    initial_node bidirectional_influence_map =
  let initial_node = Ckappa_sig.int_of_rule_id initial_node in
  let n = Array.length blackboard.Remanent_state.blackboard_is_done in
  let influence_map =
    ( [],
      Ckappa_sig.PairRule_setmap.Map.empty,
      Ckappa_sig.PairRule_setmap.Map.empty )
  in
  if initial_node < n then (
    let good_distance d =
      leq d.Remanent_state.fwd fwd
      && leq d.Remanent_state.bwd bwd
      && leq d.Remanent_state.total (Some total)
    in
    let distance =
      {
        array = blackboard.Remanent_state.blackboard_distance;
        list = [];
        default = None;
      }
    in
    let is_done =
      {
        array = blackboard.Remanent_state.blackboard_is_done;
        list = [];
        default = false;
      }
    in
    let error, distance =
      set parameters error ~pos:__POS__ initial_node (Some origin) distance
    in
    let next_round = [] in
    let waiting_list = [ initial_node ] in
    let remanent_state =
      {
        distance;
        is_done;
        next_round;
        to_be_explored = blackboard.Remanent_state.blackboard_to_be_explored;
        waiting_list;
        influence_map;
      }
    in
    let rec aux parameters error remanent_state =
      if is_empty remanent_state then
        error, remanent_state
      else (
        let error, (node, remanent_state) =
          pop parameters error ~pos:__POS__ remanent_state
        in
        let error, distance_opt =
          get parameters error ~pos:__POS__ node remanent_state.distance
        in
        let error, distance =
          match distance_opt with
          | None -> Exception.warn parameters error __POS__ Exit origin
          | Some distance -> error, distance
        in
        let error, remanent_state =
          visit parameters error node distance remanent_state good_distance
            bidirectional_influence_map
        in
        aux parameters error remanent_state
      )
    in
    let error, remanent_state = aux parameters error remanent_state in
    let () = clean_remanent_state remanent_state in
    ( error,
      remanent_state.influence_map,
      {
        Remanent_state.blackboard_distance = remanent_state.distance.array;
        Remanent_state.blackboard_is_done = remanent_state.is_done.array;
        Remanent_state.blackboard_to_be_explored = remanent_state.to_be_explored;
      } )
  ) else
    error, influence_map, blackboard
