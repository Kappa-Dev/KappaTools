type 'a cleanable_array =
  {
    array:'a array;
    default:'a;
    list:int list;
  }

let set parameters error ~pos i im array =
  try
    (let old = array.array.(i) in
  let () = array.array.(i)<-im in
    match old=array.default,im=array.default with
    | true, false -> error, {array with list = i::array.list}
    | _ -> error, array)
  with
  | err ->
    Exception.warn
      parameters error pos err array

let get parameters error ~pos i array =
  try
    error, array.array.(i)
  with
    err ->
    Exception.warn
      parameters error pos err array.default

let clean array =
  List.iter (fun i -> array.array.(i)<-array.default) array.list

type distance =
  {
    fwd: int ;
    bwd: int ;
    total: int
  }

let origin =
  {
    fwd=0;
    bwd=0;
    total=0;
  }

let combine_distance a b =
  match a with
  | None -> Some b
  | Some a ->
    Some
      {
        fwd = min a.fwd b.fwd ;
        bwd = min a.fwd b.fwd ;
        total = min a.total b.total
      }

type blackboard =
  {
    blackboard_distance: distance option array;
    blackboard_is_done: bool array;
    blackboard_to_be_explored: bool array
  }

let init_blackboard nrules nvars =
  let n = nrules + nvars in
  {
    blackboard_distance       = Array.make n None;
    blackboard_is_done        = Array.make n false;
    blackboard_to_be_explored = Array.make n false
  }

type remanent_state =
  {
    waiting_list: int  list ;
    next_round: int list ;
    distance: distance option cleanable_array ;
    is_done: bool cleanable_array ;
    to_be_explored: bool array ;
    influence_map: Remanent_state.internal_influence_map
  }

let is_empty remanent_state =
  remanent_state.waiting_list=[] && remanent_state.next_round=[]

let rec pop ~pos parameters error remanent_state =
  match remanent_state.waiting_list with
  | head::tail ->
    error,(head,{remanent_state with waiting_list = tail})
  | [] ->
    begin
      match remanent_state.next_round with
      | [] ->
        Exception.warn parameters error pos
          (Invalid_argument "empty waiting list") (0,remanent_state)
      | _ ->
        pop ~pos parameters error
          {remanent_state with waiting_list = remanent_state.next_round ;
                               next_round = []}
    end


let clean_remanent_state remanent_state =
  let () = clean remanent_state.distance in
  let () = clean remanent_state.is_done in
  ()

let go_fwd distance =
  {distance with fwd = succ distance.fwd ; total = succ distance.total}
let go_bwd distance =
  {distance with bwd = succ distance.bwd ; total = succ distance.total}

let leq int int_ref_opt =
  match int_ref_opt with
  | None -> true
  | Some int_ref -> compare int int_ref <= 0

let has_improved ~new_distance ~old_distance_opt =
  match old_distance_opt
  with
  | None -> true
  | Some old_distance ->
    new_distance.fwd < old_distance.fwd
    || new_distance.bwd < old_distance.bwd
    || new_distance.total < old_distance.total

let best_distance new_distance old_distance_opt =
  match old_distance_opt with
  | None -> new_distance
  | Some old_distance ->
    {
      fwd = min new_distance.fwd old_distance.fwd;
      bwd = min new_distance.bwd old_distance.bwd;
      total = min new_distance.total old_distance.total
    }

let add_influence source target label influence_map =
  Ckappa_sig.PairRule_setmap.Map.add
    (Ckappa_sig.rule_id_of_int source,
     Ckappa_sig.rule_id_of_int target)
    label
    influence_map

let add_positive_influence source target label remanent_state =
  let pos,neg = remanent_state.influence_map in
  let pos = add_influence source target label pos in
  let influence_map = pos,neg in
  {remanent_state with influence_map}

let add_negative_influence source target label remanent_state =
  let pos,neg = remanent_state.influence_map in
  let neg = add_influence source target label neg in
  let influence_map = pos,neg in
  {remanent_state with influence_map}

let add_rev f source target label remanent_state =
  f target source label remanent_state

let explore_one_map
    good_distance add_influence
    parameters error remanent_state source is_done new_distance list
  =
  if good_distance new_distance then
    List.fold_left
      (fun (error, remanent_state) (target,label) ->
         let target = Ckappa_sig.int_of_rule_id target in
         let error, target_done =
           get parameters error ~pos:__POS__ target remanent_state.is_done
         in
         let remanent_state =
           match
             is_done, target_done || target=source
           with
           | false,true ->
             add_influence
               source target label remanent_state
           | _ -> remanent_state
         in
         let error,old_distance_opt =
           get parameters error ~pos:__POS__ target remanent_state.distance in
         let new_distance = best_distance new_distance old_distance_opt in
         if good_distance new_distance &&
            has_improved ~new_distance ~old_distance_opt
         then
           let error, distance =
             set
               parameters error ~pos:__POS__
               target (Some new_distance) remanent_state.distance
           in
           let to_be_explored = remanent_state.to_be_explored in
           let next_round = remanent_state.next_round in
           let next_round,to_be_explored =
             if to_be_explored.(target)
             then
               next_round, to_be_explored
             else
               let () = to_be_explored.(target)<-true in
               target::next_round, to_be_explored
           in
           error, {remanent_state with
            distance = distance ;
            next_round = next_round ;
            to_be_explored = to_be_explored  }
         else
           error,
           remanent_state)
      (error, remanent_state)
      list
  else
    error, remanent_state


let visit parameters error
    node distance remanent_state good_distance bidirectional_influence_map
  =
  let (error,is_done),pos_fwd,pos_bwd,neg_fwd,neg_bwd =
          get parameters error ~pos:__POS__ node remanent_state.is_done,
          bidirectional_influence_map.Remanent_state.positive_influence_fwd.(node),
          bidirectional_influence_map.Remanent_state.positive_influence_bwd.(node),
          bidirectional_influence_map.Remanent_state.negative_influence_fwd.(node),
          bidirectional_influence_map.Remanent_state.negative_influence_bwd.(node)
  in
  let distance_next = go_fwd distance in
  let distance_prev = go_bwd distance in
  let error, remanent_state =
    explore_one_map
      good_distance add_positive_influence
      parameters error remanent_state node is_done distance_next pos_fwd
  in
  let error, remanent_state =
    explore_one_map
      good_distance add_negative_influence
      parameters error remanent_state node is_done distance_next neg_fwd
  in
  let error, remanent_state =
    explore_one_map
      good_distance (add_rev add_positive_influence)
      parameters error remanent_state node is_done distance_prev pos_bwd
  in
  let error, remanent_state =
    explore_one_map
      good_distance (add_rev add_negative_influence)
      parameters error remanent_state node is_done distance_next neg_bwd
  in
  let error, is_done =
    set parameters error ~pos:__POS__ node true remanent_state.is_done
  in
  let error, to_be_explored =
    try
      let () = remanent_state.to_be_explored.(node)<-false in
      error, remanent_state.to_be_explored
    with
    _ ->
      Exception.warn parameters error __POS__ Exit remanent_state.to_be_explored
  in
  error,
  {remanent_state with is_done = is_done ; to_be_explored = to_be_explored}

let explore_influence_map
    ?fwd ?bwd ~total
    parameters error blackboard initial_node bidirectional_influence_map =
  let good_distance d =
    leq d.fwd fwd
    || leq d.bwd bwd
    || leq d.total (Some total)
  in
  let distance = {array=blackboard.blackboard_distance;list=[];default=None} in
  let is_done = {array=blackboard.blackboard_is_done;list=[];default=false} in
  let error, distance =
    set parameters error ~pos:__POS__ initial_node (Some origin) distance
  in
  let next_round = [] in
  let waiting_list = [initial_node] in
  let influence_map =
    Ckappa_sig.PairRule_setmap.Map.empty,
    Ckappa_sig.PairRule_setmap.Map.empty
  in
  let remanent_state =
    {
      distance=distance;
      is_done=is_done;
      next_round=next_round ;
      to_be_explored=blackboard.blackboard_to_be_explored;
      waiting_list=waiting_list;
      influence_map=influence_map
    }
  in
  let rec aux parameters error remanent_state =
    if is_empty remanent_state then error, remanent_state
    else
      let error, (node,remanent_state) =
        pop parameters error ~pos:__POS__ remanent_state
      in
      let error, distance_opt =
        get parameters error ~pos:__POS__ node remanent_state.distance
      in
      let error, distance =
        match distance_opt with
        | None ->
          Exception.warn
            parameters error __POS__
            Exit origin
        | Some distance -> error, distance
      in
      let error, remanent_state =
        visit parameters error
          node distance remanent_state good_distance bidirectional_influence_map
      in
      aux parameters error remanent_state
  in
  let error, remanent_state = aux parameters error remanent_state in
  let () = clean_remanent_state remanent_state in
  error,remanent_state.influence_map,
  {blackboard_distance = remanent_state.distance.array ;
   blackboard_is_done = remanent_state.is_done.array ;
   blackboard_to_be_explored = remanent_state.to_be_explored}
