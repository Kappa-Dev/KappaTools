type id = int

(*type preupdate = {elts: Mods.IntSet.t; previous_size: int; current_size: int}*)

type thresholds_state = int option * int Mods.IntMap.t
type previous_threshold_value = int Array.t * int Array.t Array.t

type update = {
  id: id * id option;
  previous_threshold: id;
  current_threshold: id;
}

type updates = update list

module Blackboard = struct
  type 'a t = { array: 'a option Mods.DynArray.t; keys: id list }

  let create () = { array = Mods.DynArray.create 0 None; keys = [] }
  let copy t = { array = Mods.DynArray.copy t.array; keys = t.keys }
  let get t i = Mods.DynArray.get t.array i

  let define t i j =
    let () = Mods.DynArray.set t.array i j in
    let keys = i :: t.keys in
    { t with keys }

  let overwrite t i j =
    let () = Mods.DynArray.set t.array i j in
    t

  let set t i j =
    let old = get t i in
    match old with
    | None -> define t i j
    | Some _ -> overwrite t i j

  let fold_and_clean f t acc =
    let array, acc =
      List.fold_left
        (fun (array, acc) elt ->
          match get t elt with
          | None -> array, acc
          | Some a ->
            let () = Mods.DynArray.set array elt None in
            array, f elt a acc)
        (t.array, acc) t.keys
    in
    { t with array }, acc

  let fold f t acc =
    let acc =
      List.fold_left
        (fun acc elt ->
          match get t elt with
          | None -> acc
          | Some a -> f elt a acc)
        acc t.keys
    in
    acc

  let fold_and_flush f t acc =
    let t, acc = fold_and_clean f t acc in
    let keys = [] in
    { t with keys }, acc
end

type weight = int * int Mods.IntMap.t

let fold_weight f w a =
  let a = f None (fst w) a in
  Mods.IntMap.fold (fun agent_id -> f (Some agent_id)) (snd w) a

type t = {
  array: id option Mods.DynArray.t; (* its transitive closure defines the rep *)
  array_update: id option Blackboard.t; (* changes in a buffer *)
  back_trans: Mods.IntSet.t Mods.DynArray.t; (* rep to its equivalence class *)
  back_trans_update: Mods.IntSet.t Blackboard.t;
  size: weight option Mods.DynArray.t;
      (* rep to the size of its equivalence class *)
  size_update: weight Blackboard.t; (* change in size in a buffer *)
  to_check_unbind: Mods.IntSet.t;
  split: id Blackboard.t;
  threshold_update: Mods.IntIntOptSet.t Blackboard.t;
  threshold_old: Mods.IntIntOptSet.t Blackboard.t;
  degraded: int list;
  fresh: int list;
}

let flush_updates t =
  let array_update, array =
    Blackboard.fold_and_flush
      (fun i j array ->
        let () = Mods.DynArray.set array i j in
        array)
      t.array_update t.array
  in
  let back_trans_update, back_trans =
    Blackboard.fold_and_flush
      (fun i j array ->
        let () = Mods.DynArray.set array i j in
        array)
      t.back_trans_update t.back_trans
  in
  let size_update, size =
    Blackboard.fold_and_flush
      (fun i j array ->
        let () = Mods.DynArray.set array i (Some j) in
        array)
      t.size_update t.size
  in
  let degraded = [] in
  let fresh = [] in
  {
    t with
    array;
    array_update;
    back_trans;
    back_trans_update;
    size;
    size_update;
    degraded;
    fresh;
  }

let print f t =
  let () = Format.fprintf f "ARRAY @." in
  let () =
    Mods.DynArray.iteri
      (fun i j ->
        Format.fprintf f "%i -> %i@." i
          (match j with
          | None -> -1
          | Some j -> j))
      t.array
  in
  let () = Format.fprintf f "BACK @." in
  let () =
    Mods.DynArray.iteri
      (fun i j ->
        Format.fprintf f "%i -> " i;
        Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) j;
        Format.fprintf f "@.")
      t.back_trans
  in
  let () = Format.fprintf f "SIZE @." in
  let () =
    Mods.DynArray.iteri
      (fun i j_opt ->
        match j_opt with
        | None -> ()
        | Some (j, jmap) ->
          Format.fprintf f "size: %i -> %i @." i j;
          Mods.IntMap.iter
            (fun id j -> Format.fprintf f "nb_of_%i: %i -> %i @." id i j)
            jmap)
      t.size
  in
  ()

let print_update f t =
  let () = Format.fprintf f "ARRAY(UPDATE) @." in
  let () =
    Blackboard.fold
      (fun i j () ->
        Format.fprintf f "%i -> %i@." i
          (match j with
          | None -> -1
          | Some j -> j))
      t.array_update ()
  in
  let () = Format.fprintf f "BACK(UPDATE) @." in
  let () =
    Blackboard.fold
      (fun i j () ->
        Format.fprintf f "%i -> " i;
        Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) j;
        Format.fprintf f "@.")
      t.back_trans_update ()
  in
  let () = Format.fprintf f "SIZE(UPDATE) @." in
  let () =
    Blackboard.fold
      (fun i (j, jmap) () ->
        Format.fprintf f "size: %i -> %i @." i j;
        Mods.IntMap.iter
          (fun id j -> Format.fprintf f "nb_of_%i: %i -> %i @." id i j)
          jmap)
      t.size_update ()
  in
  let () = Format.fprintf f "TO CHECK UNBIND @." in
  let () =
    Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) t.to_check_unbind
  in
  let () = Format.fprintf f "@." in
  let () = Format.fprintf f "DEGRADED @." in
  let () = List.iter (fun j -> Format.fprintf f "%i," j) t.degraded in
  ()

let print_all f t =
  print f t;
  print_update f t

let init () =
  {
    array = Mods.DynArray.create 0 None;
    array_update = Blackboard.create ();
    back_trans = Mods.DynArray.create 0 Mods.IntSet.empty;
    back_trans_update = Blackboard.create ();
    size = Mods.DynArray.create 0 None;
    size_update = Blackboard.create ();
    to_check_unbind = Mods.IntSet.empty;
    split = Blackboard.create ();
    threshold_update = Blackboard.create ();
    threshold_old = Blackboard.create ();
    degraded = [];
    fresh = [];
  }

let copy t =
  {
    array = Mods.DynArray.copy t.array;
    array_update = Blackboard.copy t.array_update;
    back_trans = Mods.DynArray.copy t.back_trans;
    back_trans_update = Blackboard.copy t.back_trans_update;
    size = Mods.DynArray.copy t.size;
    size_update = Blackboard.copy t.size_update;
    to_check_unbind = t.to_check_unbind;
    split = Blackboard.copy t.split;
    threshold_update = Blackboard.copy t.threshold_update;
    threshold_old = Blackboard.copy t.threshold_old;
    degraded = t.degraded;
    fresh = t.fresh;
  }

let lift t acc i =
  let i = Some i in
  let () = List.iter (fun j -> Mods.DynArray.set t.array j i) acc in
  t

let lift_new t acc i =
  let i = Some (Some i) in
  let array_update =
    List.fold_left
      (fun array_update j -> Blackboard.set array_update j i)
      t.array_update acc
  in
  { t with array_update }

let old_exists t i =
  match Mods.DynArray.get t.array i with
  | None -> false
  | Some _ -> true

let get_old_rep t i =
  let rec aux t i acc =
    match Mods.DynArray.get t.array i with
    | None -> assert false
    | Some j when j = i -> lift t acc i, i
    | Some j -> aux t j (i :: acc)
  in
  aux t i []

let get_new_rep t i =
  let rec aux t i acc =
    match Blackboard.get t.array_update i with
    | Some None -> assert false
    | Some (Some j) when j = i -> lift_new t acc i, i
    | Some (Some j) -> aux t j (i :: acc)
    | None ->
      (match Mods.DynArray.get t.array i with
      | None -> assert false
      | Some j when j = i -> lift_new t acc i, i
      | Some j -> aux t j (i :: acc))
  in
  aux t i []

let get_old_size t i =
  let t, rep = get_old_rep t i in
  t, Mods.DynArray.get t.size rep

let get_new_size_rep t rep =
  match Blackboard.get t.size_update rep with
  | None ->
    ( t,
      (match Mods.DynArray.get t.size rep with
      | None -> assert false
      | Some i -> i) )
  | Some s -> t, s

let get_old_equiv_class t i =
  let t, rep = get_old_rep t i in
  t, Mods.DynArray.get t.back_trans rep

let get_new_equiv_class_rep t rep =
  match Blackboard.get t.back_trans_update rep with
  | None -> t, Mods.DynArray.get t.back_trans rep
  | Some s -> t, s

let fresh t i id =
  let array_update = Blackboard.set t.array_update i (Some (Some i)) in
  let size_update =
    Blackboard.set t.size_update i
      (Some (1, Mods.IntMap.add id 1 Mods.IntMap.empty))
  in
  let back_trans_update =
    Blackboard.set t.back_trans_update i (Some (Mods.IntSet.singleton i))
  in
  let fresh = i :: t.fresh in
  { t with array_update; size_update; back_trans_update; fresh }

let clean_degraded_agent t i = 
  let () = Mods.DynArray.set t.array i None in 
  let () = Mods.DynArray.set t.back_trans i Mods.IntSet.empty in 
  let () = Mods.DynArray.set t.size i None in 
  t
  
let map2 f g h m n =
  (* TO DO : improve data_structures *)
  let (), rep =
    Mods.IntMap.monadic_fold2 () ()
      (fun () () i a a' b -> (), Mods.IntMap.add i (f a a') b)
      (fun () () i a b -> (), Mods.IntMap.add i (g a) b)
      (fun () () i a b -> (), Mods.IntMap.add i (h a) b)
      m n Mods.IntMap.empty
  in
  rep

let size_add (i, imap) (j, jmap) =
  i + j, map2 (fun i j -> i + j) (fun i -> i) (fun i -> i) imap jmap

let join t i j =
  let t, repi = get_new_rep t i in
  let t, repj = get_new_rep t j in
  if repi = repj then
    t
  else (
    let t, sizei = get_new_size_rep t repi in
    let t, sizej = get_new_size_rep t repj in
    let repi, repj =
      if repi < repj then
        repi, repj
      else
        repj, repi
    in
    let size_update =
      Blackboard.set t.size_update repi (Some (size_add sizei sizej))
    in
    let size_update =
      Blackboard.set size_update repj (Some (0, Mods.IntMap.empty))
    in
    let array_update = Blackboard.set t.array_update repj (Some (Some repi)) in
    let t, classi = get_new_equiv_class_rep t repi in
    let t, classj = get_new_equiv_class_rep t repj in
    let back_trans_update =
      Blackboard.set t.back_trans_update repi
        (Some (Mods.IntSet.union classi classj))
    in
    let back_trans_update =
      Blackboard.set back_trans_update repj (Some Mods.IntSet.empty)
    in
    { t with size_update; array_update; back_trans_update }
  )

let unbind t i j =
  let to_check_unbind = t.to_check_unbind in
  let to_check_unbind = Mods.IntSet.add i to_check_unbind in
  let to_check_unbind = Mods.IntSet.add j to_check_unbind in
  { t with to_check_unbind }

let create = fresh
let bind = join

let degrade ~neighbor:f t i =
  let list = f i in
  let t = List.fold_left (fun t j -> unbind t i j) t list in
  { t with degraded = i :: t.degraded }

let scan i l =
  let rec aux i l acc =
    match l with
    | (k, d) :: t when k = i -> List.rev acc @ t, Some d
    | (k, d) :: t -> aux i t ((k, d) :: acc)
    | [] -> List.rev acc, None
  in
  aux i l []

let scan2 i l l' =
  match scan i l with
  | l, Some a -> Some a, l, l'
  | _, None ->
    let l', a = scan i l' in
    a, l, l'

let unify_weight w_new w_old =
  ( fst w_new,
    snd
      (Mods.IntMap.map2_with_logs
         (fun () () _ _ _ -> ())
         () ()
         (fun () () j -> (), j)
         (fun () () _ -> (), 0)
         (fun () () j _ -> (), j)
         (snd w_new) (snd w_old)) )

(* The following functions implement a fusion relation specified as a list of equalitiy between integer identifiers *)
(* It is not called very often, but there is room for improvement with an incremental version *)

let build_alias aliases =
  match aliases with
  | [] -> [], fun i -> i
  | _ ->
    let aliases =
      List.rev_map
        (fun (i, j) ->
          if i < j then
            i, j
          else
            j, i)
        aliases
    in
    let aliases = List.sort (fun (a, _) (b, _) -> compare a b) aliases in
    let aliases_map =
      List.fold_left
        (fun m (i, j) ->
          let im =
            match Mods.IntMap.find_option i m with
            | None -> i
            | Some j -> j
          in
          Mods.IntMap.add j im m)
        Mods.IntMap.empty aliases
    in
    let aliases_fun i =
      match Mods.IntMap.find_option i aliases_map with
      | None -> i
      | Some j -> j
    in
    aliases, aliases_fun

let init_alias = [], fun i -> i
let apply_alias (_, f) = f
let add_alias (i, j) (l, _) = build_alias ((i, j) :: l)

let setminus a b = Mods.IntSet.inter a (Mods.IntSet.diff a b)

let flush ~neighbor ~agtype ~(thresholds : weight -> weight) t =
  let set = t.to_check_unbind in
  let degraded_set =
    List.fold_left
      (fun set i -> Mods.IntSet.add i set)
      Mods.IntSet.empty t.degraded
  in
  let fresh_set =
    List.fold_left
      (fun set i -> Mods.IntSet.add i set)
      Mods.IntSet.empty t.fresh
  in
  let proper_degraded_set = setminus degraded_set fresh_set in 
  let set = setminus set proper_degraded_set in  
    (* Id of agents with some unbinding without the degraded agents which have not been replaced with a fresh one *)
  let with_degradation =
    match t.degraded with
    | [] -> false
    | _ :: _ -> true
  in
  let l = Mods.IntSet.fold (fun i l -> (i, [ i ]) :: l) set [] in
  let rec aux to_visit to_visit_after t alias =
    match to_visit, to_visit_after with
    | [], [] -> t, alias
    | [], _ -> aux (List.rev to_visit_after) [] t alias
    | (_, []) :: tail', _ -> aux tail' to_visit_after t alias
    | (i, h :: tail) :: tail', _ ->
      (match Blackboard.get t.split h with
      | None ->
        let split = Blackboard.set t.split h (Some i) in
        let t = { t with split } in
        let tail = List.fold_left (fun l x -> x :: l) tail (neighbor h) in
        aux tail' ((i, tail) :: to_visit_after) t alias
      | Some i' when i = apply_alias alias i' ->
        (* already seen, nothing to do *)
        aux tail' ((i, tail) :: to_visit_after) t alias
      | Some i' ->
        (* seen in another equivalent class -> merge *)
        let i' = apply_alias alias i' in
        merge (i, tail) i' tail' to_visit_after t (add_alias (i, i') alias))
  and merge (i, l1) j to_visit to_visit_after t alias =
    let lj_opt, to_visit, to_visit_after = scan2 j to_visit to_visit_after in
    match lj_opt with
    | None -> assert false
    | Some l2 ->
      (match to_visit, to_visit_after with
      | [], [] when with_degradation -> t, alias
      | _, _ ->
        let l = l1 @ l2 in
        let i = min i j in
        let to_visit_after = (i, l) :: to_visit_after in
        aux to_visit to_visit_after t alias)
  in
  let t, (_, aliases) = aux l [] t init_alias in
  let split =
    Blackboard.fold
      (fun i j split -> Blackboard.overwrite split i (Some (aliases j)))
      t.split t.split
  in
  let split, join =
    Blackboard.fold_and_flush
      (fun i j m ->
        let old =
          match Mods.IntMap.find_option j m with
          | None -> Mods.IntSet.empty
          | Some i -> i
        in
        Mods.IntMap.add j (Mods.IntSet.add i old) m)
      split Mods.IntMap.empty
  in
  let t = { t with split } in
  let t =
    Mods.IntMap.fold
      (fun _ set t ->
        match Mods.IntSet.min_elt set with
        | None -> t
        | Some rep ->
          let rep_opt = Some (Some rep) in
          let n = Mods.IntSet.size set in
          let map =
            Mods.IntSet.fold
              (fun i map ->
                let t = agtype i in
                Mods.IntMap.add t
                  (match Mods.IntMap.find_option t map with
                  | Some i -> i + 1
                  | None -> 1)
                  map)
              set Mods.IntMap.empty
          in
          let array_update, size_update, back_trans_update =
            Mods.IntSet.fold
              (fun j (array_update, size_update, back_trans_update) ->
                ( Blackboard.set array_update j rep_opt,
                  Blackboard.set size_update j (Some (0, Mods.IntMap.empty)),
                  Blackboard.set back_trans_update j (Some Mods.IntSet.empty) ))
              set
              (t.array_update, t.size_update, t.back_trans_update)
          in
          let size_update = Blackboard.set size_update rep (Some (n, map)) in
          let back_trans_update =
            Blackboard.set back_trans_update rep (Some set)
          in
          { t with array_update; size_update; back_trans_update })
      join t
  in
  let t, threshold_update, threshold_old =
    Blackboard.fold
      (fun rep (set_update : Mods.IntSet.t) (t, threshold_update, threshold_old) ->
        let t, new_size = get_new_size_rep t rep in
        let new_threshold = thresholds new_size in
        let t, old_size =
          if old_exists t rep then
            get_old_size t rep
          else
            t, Some (0, Mods.IntMap.empty)
        in
        let t, old_set =
          if old_exists t rep then
            get_old_equiv_class t rep
          else
            t, Mods.IntSet.empty
        in
        let old_threshold =
          match old_size with
          | None -> 0, Mods.IntMap.empty
          | Some old_size -> thresholds old_size
        in
        let new_threshold = unify_weight new_threshold old_threshold in
        let threshold_update =
          fold_weight
            (fun agent_id_opt new_threshold threshold_update ->
              match Blackboard.get threshold_update new_threshold with
              | None ->
                Blackboard.define threshold_update new_threshold
                  (Some
                     (Mods.IntSet.fold
                        (fun i set ->
                          Mods.IntIntOptSet.add (i, agent_id_opt) set)
                        set_update Mods.IntIntOptSet.empty))
              | Some set ->
                Blackboard.overwrite threshold_update new_threshold
                  (Some
                     (Mods.IntSet.fold
                        (fun i set ->
                          Mods.IntIntOptSet.add (i, agent_id_opt) set)
                        set_update set)))
            new_threshold threshold_update
        in
        let threshold_old =
          fold_weight
            (fun agent_id_opt old_threshold threshold_old ->
              match Blackboard.get threshold_old old_threshold with
              | None ->
                Blackboard.define threshold_old old_threshold
                  (Some
                     (Mods.IntSet.fold
                        (fun i -> Mods.IntIntOptSet.add (i, agent_id_opt))
                        old_set Mods.IntIntOptSet.empty))
              | Some set ->
                Blackboard.overwrite threshold_old old_threshold
                  (Some
                     (Mods.IntSet.fold
                        (fun i -> Mods.IntIntOptSet.add (i, agent_id_opt))
                        old_set set)))
            old_threshold threshold_old
        in
        t, threshold_update, threshold_old)
      t.back_trans_update
      (t, t.threshold_update, t.threshold_old)
  in
  let threshold_update =
    Blackboard.fold
      (fun threshold set threshold_update ->
        match Blackboard.get threshold_old threshold with
        | None -> threshold_update
        | Some a ->
          Blackboard.overwrite threshold_update threshold
            (Some (Mods.IntIntOptSet.minus set a)))
      threshold_update threshold_update
  in
  let threshold_update, (t, updates) =
    Blackboard.fold_and_flush
      (fun current_threshold set (t, updates) ->
        Mods.IntIntOptSet.fold
          (fun id (t, updates) ->
            let t, w =
              if old_exists t (fst id) then (
                let t, size = get_old_size t (fst id) in
                match size with
                | None -> t, (0, Mods.IntMap.empty)
                | Some size -> t, thresholds size
              ) else
                t, (0, Mods.IntMap.empty)
            in
            let previous_threshold =
              match snd id with
              | None -> fst w
              | Some i ->
                (match Mods.IntMap.find_option i (snd w) with
                | None -> 0
                | Some i -> i)
            in
            t, { id; previous_threshold; current_threshold } :: updates)
          set (t, updates))
      threshold_update (t, [])
  in
  let t = 
    Mods.IntSet.fold 
      (fun a b -> clean_degraded_agent b a) 
    proper_degraded_set t 
  in 
  let to_check_unbind = Mods.IntSet.empty in
  let t = { t with to_check_unbind; threshold_update; threshold_old } in
  let t = flush_updates t in 
  t, updates

(*let t = [|0;1;1;1;1;1;1;1;1;1;10;10;10;10|]

  let threshold x =
    try t.(x) with _ -> 10 *)

(*
let do_it () = 
    let t = init in 
    let t = 
      let rec aux k t = 
         if k = 0 then t 
         else aux (k-1) (fresh t k)
       in aux 2000 t  in 
    let () = Format.fprintf Format.std_formatter "STEP INIT CREATE 1 - ... - 20 @." in 
    (*let () = print_all Format.std_formatter t in *)
    let t = 
      let rec aux k t = 
        if k=1 then t else aux (k-1) (join t k (k-1))
      in aux 2000 t 
    in 
    let () = Format.fprintf Format.std_formatter "STEP (BIND 1-2-3-4-5-6-7-8-9-10-11-12-13-14-15-16-17-18-19-20)) @." in
    let t, updates = flush ~neighbor:(fun i -> 
      match i with 
      | 2000 -> [1999]
      | 1 -> [2]
      | _ -> [i+1;i-1]) ~threshold t in 
    let t = flush_updates t in 
(*    let () = print Format.std_formatter t in *)
    let () = Format.fprintf Format.std_formatter "TO CHECK @." in
    let () = List.iter (fun i -> Format.fprintf Format.std_formatter "UPDATE: %i: %i -> %i @." i.id i.previous_threshold i.current_threshold) updates  in 
    let () = Format.fprintf Format.std_formatter "@." in 
 
    let () = Format.fprintf Format.std_formatter "STEP (UNBIND 5 6) @." in
    let t = unbind t 5 6 in 
    let t, updates = flush ~neighbor:(fun i -> 
      match i with 
      | 2000 -> [1999]
      | 1 -> [2]
      | 5 -> [4]
      | 6 -> [7]
      | _ -> [i+1;i-1]) ~threshold t in 
    let t = flush_updates t in 
(*    let () = print Format.std_formatter t in *)
    let () = Format.fprintf Format.std_formatter "TO CHECK @." in
    let () = List.iter (fun i -> Format.fprintf Format.std_formatter "UPDATE: %i: %i -> %i @." i.id i.previous_threshold i.current_threshold) updates  in 
    let () = Format.fprintf Format.std_formatter "@." in 
    let () = Format.fprintf Format.std_formatter "STEP (UNBIND 12  13) @." in
    let t = unbind t 12 13 in 
    let t, updates = flush ~neighbor:(fun i -> 
      match i with 
      | 2000 -> [1999]
      | 1 -> [2]
      | 5 -> [4]
      | 6 -> [7]
      | 12 -> [11]
      | 13 -> [14]
      | _ -> [i+1;i-1]) ~threshold t in 
    let _ = flush_updates t in 
 (*   let () = print Format.std_formatter t in *)
    let () = Format.fprintf Format.std_formatter "TO CHECK @." in
    let () = List.iter (fun i -> Format.fprintf Format.std_formatter "UPDATE: %i: %i -> %i @." i.id i.previous_threshold i.current_threshold) updates  in 
    let () = Format.fprintf Format.std_formatter "@." in 
    ()

*)

let map_w_array max_elt f dft w =
  let m = Array.make (max_elt + 1) dft in
  ( f (fst w),
    let () =
      Mods.IntMap.iter
        (fun i data ->
          let output = f data in
          try Array.set m i output
          with _ -> Format.printf "Problème line 638 %i" i)
        (snd w)
    in
    m )

let build_threshold n =
  map_w_array n
    (fun s ->
      if Mods.IntSet.is_empty s then
        Array.make 0 0
      else (
        let l = Mods.IntSet.fold (fun i l -> i :: l) s [] in
        let max =
          match l with
          | t :: _ -> t
          | [] -> assert false
        in
        let l = List.rev l in
        let array = Array.make (max + 1) 0 in
        let rec aux k acc current =
          if k = max + 1 then
            ()
          else (
            match acc with
            | h :: t when k >= h -> aux k t h
            | _ ->
              array.(k) <- current;
              aux (k + 1) acc current
          )
        in
        let () = aux 0 l 0 in
        array
      ))
    [||]

let compute_threshold_list_inc threshold_list id_opt i j =
  let rec aux2 thresholds acc =
    match thresholds with
    | h :: t when h <= j -> aux2 t (h :: acc)
    | _ :: _ | [] -> List.rev acc
  in
  let rec aux1 thresholds =
    match thresholds with
    | h :: t when h < i -> aux1 t
    | _ :: _ | [] -> aux2 thresholds []
  in
  let threshold_list =
    match id_opt with
    | None -> snd (fst threshold_list)
    | Some i ->
      snd
        (try Array.get (snd threshold_list) i
         with _ ->
           Format.printf "Problème line 685 %i" i;
           assert false)
  in
  aux1 threshold_list

type 'a pos_neg = { negative_update: 'a list; positive_update: 'a list }

type cache =
  ((id * id option * bool) pos_neg option array array * id list)
  * ((id * id option * bool) pos_neg option array array * id list) array

let empty_pos_neg = { negative_update = []; positive_update = [] }
let dummy_cache = ([||], []), [||]

let compute_threshold_list threshold_list id_opt i j =
  let b, min, max =
    if i < j then
      true, i, j
    else
      false, j, i
  in
  let list = compute_threshold_list_inc threshold_list id_opt min max in
  let list =
    if i = 0 || j = 0 then
      list
    else if b then (
      match list with
      | _ :: t -> t
      | _ -> []
    ) else (
      match List.rev list with
      | _ :: t -> List.rev t
      | _ -> []
    )
  in
  let l_true = List.rev_map (fun a -> a, id_opt, true) (List.rev list) in
  let l_false = List.rev_map (fun a -> a, id_opt, false) (List.rev list) in
  if b then
    { negative_update = l_true; positive_update = l_false }
  else
    { negative_update = l_false; positive_update = l_true }

let get_positive_update a = a.positive_update
let get_negative_update a = a.negative_update

let get_matrix m id_opt i j =
  let m =
    match id_opt with
    | None -> fst (fst m)
    | Some i -> fst (Array.get (snd m) i)
  in
  m.(i).(j)

let set_matrix m id_opt i j a =
  let m =
    match id_opt with
    | None -> fst m
    | Some i -> Array.get (snd m) i
  in
  Array.set (Array.get (fst m) i) j a

let init_between_thresholds n threshold_set =
  map_w_array n
    (fun threshold_set ->
      let threshold_list = Mods.IntSet.elements threshold_set in
      let max =
        match List.rev threshold_list with
        | t :: _ -> t
        | [] -> 0
      in
      Array.init (max + 1) (fun _ -> Array.make (max + 1) None), threshold_list)
    ([||], []) threshold_set

let get_between_thresholds (m : cache) id_opt i j =
  if i = j then
    empty_pos_neg
  else (
    match get_matrix m id_opt i j with
    | None ->
      let rep = compute_threshold_list m id_opt i j in
      let () = set_matrix m id_opt i j (Some rep) in
      rep
    | Some rep -> rep
  )

let eval_threshold array =
  let max = Array.length array in
  if max = 0 then
    fun _ ->
  0
  else (
    let f (i : id) =
      if i < max then
        array.(i)
      else
        array.(max - 1)
    in
    f
  )

let eval_threshold array weight =
  ( eval_threshold (fst array) (fst weight),
    Mods.IntMap.mapi (fun i w -> eval_threshold (snd array).(i) w) (snd weight)
  )

(*let log_array t =
    Format.printf "ARRAY : @.";
    Format.printf "Size: ";
    Array.iter (Format.printf "%i " ) (fst t);
    Format.printf "@." ;
    Array.iteri
      (fun i j ->
        (Format.printf "Agent_type %i: @." i );
      Array.iter (Format.printf "%i " ) j;
      Format.printf "@." ; )
      (snd t)

  let log_threshold t =
    Format.printf "Thresholds-Diff : @.";
    Format.printf "Size: %i @." (fst t);
    Mods.IntMap.iter
      (fun i j -> (Format.printf "Agent_type %i: %i @." i j)) (snd t)

  let eval_threshold array weight =
    let () = Format.printf "ARRAY: @." in
    let () = log_array array in
    let () = Format.printf "BEFORE: @." in
    let () = log_threshold weight in
    let w = eval_threshold array weight in
    let () = Format.printf "AFTER: @." in
    let () = log_threshold w in
    w *)

let is_connected t id id' =
  let t, rep = get_old_rep t id in
  let t, rep' = get_old_rep t id' in
  t, rep = rep'

let json_of_pos_neg to_json pos_neg =
  `Assoc
    [
      "negative_update", JsonUtil.of_list to_json pos_neg.negative_update;
      "positive_update", JsonUtil.of_list to_json pos_neg.positive_update;
    ]

let pos_neg_of_json of_json = function
  | `Assoc l as x when List.length l = 2 ->
    {
      negative_update =
        JsonUtil.to_list of_json (Yojson.Basic.Util.member "negative_update" x);
      positive_update =
        JsonUtil.to_list of_json (Yojson.Basic.Util.member "positive_update" x);
    }
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct pos_neg", x))

let cache_of_json =
  JsonUtil.to_pair
    (JsonUtil.to_pair
       (JsonUtil.to_array
          (JsonUtil.to_array
             (JsonUtil.to_option
                (pos_neg_of_json
                   (JsonUtil.to_triple
                      (JsonUtil.to_int ?error_msg:None)
                      (JsonUtil.to_option (JsonUtil.to_int ?error_msg:None))
                      (JsonUtil.to_bool ?error_msg:None))))))
       (JsonUtil.to_list (JsonUtil.to_int ?error_msg:None)))
    (JsonUtil.to_array
       (JsonUtil.to_pair
          (JsonUtil.to_array
             (JsonUtil.to_array
                (JsonUtil.to_option
                   (pos_neg_of_json
                      (JsonUtil.to_triple
                         (JsonUtil.to_int ?error_msg:None)
                         (JsonUtil.to_option (JsonUtil.to_int ?error_msg:None))
                         (JsonUtil.to_bool ?error_msg:None))))))
          (JsonUtil.to_list (JsonUtil.to_int ?error_msg:None))))

let json_of_cache cache =
  JsonUtil.of_pair
    (JsonUtil.of_pair
       (JsonUtil.of_array
          (JsonUtil.of_array
             (JsonUtil.of_option
                (json_of_pos_neg
                   (JsonUtil.of_triple JsonUtil.of_int
                      (JsonUtil.of_option JsonUtil.of_int)
                      JsonUtil.of_bool)))))
       (JsonUtil.of_list JsonUtil.of_int))
    (JsonUtil.of_array
       (JsonUtil.of_pair
          (JsonUtil.of_array
             (JsonUtil.of_array
                (JsonUtil.of_option
                   (json_of_pos_neg
                      (JsonUtil.of_triple JsonUtil.of_int
                         (JsonUtil.of_option JsonUtil.of_int)
                         JsonUtil.of_bool)))))
          (JsonUtil.of_list JsonUtil.of_int)))
    cache

(*
  let do_it () = 
    let s = Mods.IntSet.add 10 (Mods.IntSet.singleton 100) in 
    let f = build_threshold s in 
    let () = 
     List.iter (fun i -> Format.fprintf Format.std_formatter "%i -> %i @." i (f i)) 
      [1;2;3;9;10;11;99;100;104]
  in ()*)
