let set i im (array,list) =
  let old = array.(i) in
  let () = array.(i)<-im in
    match old,im with
    | None, Some _ -> array,(i::list)
    | None, None
    | Some _, (None | Some _) -> array,list

let get i (array,list) = array.(i)

let clean default (array,list) =
  List.iter (fun i -> array.(i)<-default) list

type distance =
  {
    fwd: int ;
    bwd: int ;
    total: int
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

type remanent_state =
  {
    waiting_list: int  list ;
    next_round: int list ;
    distance: distance option array * int list ;
    is_done: bool array * int list ;
    to_be_explored: bool array ;
    influence_map: Remanent_state.internal_influence_map
  }

let clean_remanent_state remanent_state =
  let () = clean None remanent_state.distance in
  let () = clean false remanent_state.is_done in
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
    error remanent_state source is_done new_distance list
  =
  if good_distance new_distance then
    List.fold_left
      (fun (error, remanent_state) (target,label) ->
         let target = Ckappa_sig.int_of_rule_id target in
         let target_done = get target remanent_state.is_done in
         let remanent_state =
           match
             is_done, target_done || target=source
           with
           | false,true ->
             add_influence
               source target label remanent_state
           | _ -> remanent_state
         in
         let old_distance_opt = get target remanent_state.distance in
         let new_distance = best_distance new_distance old_distance_opt in
         if good_distance new_distance &&
            has_improved ~new_distance ~old_distance_opt
         then
           let distance =
             set target (Some new_distance) remanent_state.distance in
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


let explore_influence_map
    ?fwd ?bwd ~total
    parameters error distance is_done to_be_explored init bidirectional_influence_map =
  let good_distance d =
    leq d.fwd fwd
    || leq d.bwd bwd
    || leq d.total (Some total)
  in
  let distance = distance,[] in
  let is_done = is_done,[] in
  let next_round = [] in
  let waiting_list = [] in
  let influence_map =
    Ckappa_sig.PairRule_setmap.Map.empty,
    Ckappa_sig.PairRule_setmap.Map.empty
  in
  let remanent_state =
    {
      distance=distance;
      is_done=is_done;
      next_round=next_round ;
      to_be_explored=to_be_explored;
      waiting_list=waiting_list;
      influence_map=influence_map
    }
  in
  let visit node distance remanent_state =
    let is_done,pos_fwd,pos_bwd,neg_fwd,neg_bwd =
        get node remanent_state.is_done,
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
        error remanent_state node is_done distance_next pos_fwd
    in
    let error, remanent_state =
      explore_one_map
        good_distance add_negative_influence
        error remanent_state node is_done distance_next neg_fwd
    in
    let error, remanent_state =
      explore_one_map
        good_distance (add_rev add_positive_influence)
        error remanent_state node is_done distance_prev pos_bwd
    in
    let error, remanent_state =
      explore_one_map
        good_distance (add_rev add_negative_influence)
        error remanent_state node is_done distance_next neg_bwd
    in
    error, remanent_state
  in
  let _ = visit in 
  let () = clean_remanent_state remanent_state in
  error,remanent_state.influence_map, remanent_state.is_done, remanent_state.distance, remanent_state.to_be_explored


  (*let rec explore waiting_list next_round rule_distance rule_done var_distance var_done
      influence_map
  *)
