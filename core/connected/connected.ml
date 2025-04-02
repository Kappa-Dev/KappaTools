type id = int

(*type preupdate = {elts: Mods.IntSet.t; previous_size: int; current_size: int}*)
type update = { id: id; previous_threshold: int; current_threshold: int }
type updates = update list

module Blackboard = struct
  type 'a t = { array: 'a option Mods.DynArray.t; keys: id list }

  let create () = { array = Mods.DynArray.create 0 None; keys = [] }
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

type t = {
  array: id option Mods.DynArray.t; (* its transitive closure defines the rep *)
  array_update: id option Blackboard.t; (* changes in a buffer *)
  back_trans: Mods.IntSet.t Mods.DynArray.t; (* rep to its equivalence class *)
  back_trans_update: Mods.IntSet.t Blackboard.t;
  size: int option Mods.DynArray.t;
      (* rep to the size of its equivalence class *)
  size_update: int Blackboard.t; (* change in siez in a buffer *)
  to_check_bind: Mods.IntSet.t;
  to_check_unbind: Mods.IntSet.t;
  to_check_fresh: Mods.IntSet.t;
  split: id Blackboard.t;
  threshold_update: Mods.IntSet.t Blackboard.t;
  threshold_old: Mods.IntSet.t Blackboard.t;
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

  {
    t with
    array;
    array_update;
    back_trans;
    back_trans_update;
    size;
    size_update;
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
      (fun i j ->
        Format.fprintf f "%i -> %i @." i
          (match j with
          | None -> 0
          | Some j -> j))
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
      (fun i j () -> Format.fprintf f "%i -> %i @." i j)
      t.size_update ()
  in
  let () = Format.fprintf f "TO CHECK BIND @." in
  let () =
    Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) t.to_check_bind
  in
  let () = Format.fprintf f "@." in
  let () = Format.fprintf f "TO CHECK UNBIND @." in
  let () =
    Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) t.to_check_unbind
  in
  let () = Format.fprintf f "@." in
  let () = Format.fprintf f "TO CHECK FRESH @." in
  let () =
    Mods.IntSet.iter (fun j -> Format.fprintf f "%i," j) t.to_check_fresh
  in
  let () = Format.fprintf f "@." in
  ()

let print_all f t =
  print f t;
  print_update f t

let init =
  {
    array = Mods.DynArray.create 0 None;
    array_update = Blackboard.create ();
    back_trans = Mods.DynArray.create 0 Mods.IntSet.empty;
    back_trans_update = Blackboard.create ();
    size = Mods.DynArray.create 0 None;
    size_update = Blackboard.create ();
    to_check_bind = Mods.IntSet.empty;
    to_check_unbind = Mods.IntSet.empty;
    to_check_fresh = Mods.IntSet.empty;
    split = Blackboard.create ();
    threshold_update = Blackboard.create ();
    threshold_old = Blackboard.create ();
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

let fresh t i =
  let array_update = Blackboard.set t.array_update i (Some (Some i)) in
  let size_update = Blackboard.set t.size_update i (Some 1) in
  let back_trans_update =
    Blackboard.set t.back_trans_update i (Some (Mods.IntSet.singleton i))
  in
  let to_check_fresh = Mods.IntSet.add i t.to_check_fresh in
  { t with array_update; size_update; back_trans_update; to_check_fresh }

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
      Blackboard.set t.size_update repi (Some (sizei + sizej))
    in
    let size_update = Blackboard.set size_update repj (Some 0) in
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
    let to_check_bind =
      Mods.IntSet.add repj (Mods.IntSet.add repi t.to_check_bind)
    in
    { t with size_update; array_update; back_trans_update; to_check_bind }
  )

let unbind t i j =
  let to_check_unbind = t.to_check_unbind in
  let to_check_unbind = Mods.IntSet.add i to_check_unbind in
  let to_check_unbind = Mods.IntSet.add j to_check_unbind in
  { t with to_check_unbind }

let create = fresh
let bind = join

let degrade ~neightbor:f t i =
  let list = f i in
  List.fold_left (fun t j -> unbind t i j) t list

let scan i l =
  let rec aux i l acc =
    match l with
    | (k, d) :: t when k = i -> List.rev acc @ t, Some d
    | (k, d) :: t -> aux i t ((k, d) :: t)
    | [] -> List.rev acc, None
  in
  aux i l []

let scan2 i l l' =
  match scan i l with
  | l, Some a -> Some a, l, l'
  | _, None ->
    let l', a = scan i l' in
    a, l, l'

let flush ~neighbor ~threshold t =
  let set = t.to_check_unbind in
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
      | Some i' when i = i' ->
        (* already seen, nothing to do *)
        aux tail' ((i, tail) :: to_visit_after) t alias
      | Some i' ->
        (* seen in another equialent class -> merge *)
        merge (i, tail) i' tail' to_visit_after t ((i, i') :: alias))
  and merge (i, l1) j to_visit to_visit_after t alias =
    let lj_opt, to_visit, to_visit_after = scan2 j to_visit to_visit_after in
    match lj_opt with
    | None -> assert false
    | Some l2 ->
      (match to_visit, to_visit_after with
      | [], [] -> t, []
      | _, _ ->
        let l = l1 @ l2 in
        let i = min i j in
        let to_visit_after = (i, l) :: to_visit_after in
        aux to_visit to_visit_after t alias)
  in
  let t, aliases = aux l [] t [] in
  let aliases =
    match aliases with
    | [] -> fun i -> i
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
      let aliases =
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
      let aliases i =
        match Mods.IntMap.find_option i aliases with
        | None -> i
        | Some j -> j
      in
      aliases
  in
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
          let array_update, size_update, back_trans_update =
            Mods.IntSet.fold
              (fun j (array_update, size_update, back_trans_update) ->
                ( Blackboard.set array_update j rep_opt,
                  Blackboard.set size_update j (Some 0),
                  Blackboard.set back_trans_update j (Some Mods.IntSet.empty) ))
              set
              (t.array_update, t.size_update, t.back_trans_update)
          in
          let size_update = Blackboard.set size_update rep (Some n) in
          let back_trans_update =
            Blackboard.set back_trans_update rep (Some set)
          in
          { t with array_update; size_update; back_trans_update })
      join t
  in
  let t, threshold_update, threshold_old =
    Blackboard.fold
      (fun rep (set_update : Mods.IntSet.t)
           (t, (threshold_update : Mods.IntSet.t Blackboard.t), threshold_old) ->
        let t, new_size = get_new_size_rep t rep in
        let new_threshold = threshold new_size in
        let t, old_size =
          if old_exists t rep then
            get_old_size t rep
          else
            t, Some 0
        in
        let t, old_set =
          if old_exists t rep then
            get_old_equiv_class t rep
          else
            t, Mods.IntSet.empty
        in
        let old_threshold =
          match old_size with
          | None -> 0
          | Some old_size -> threshold old_size
        in
        let threshold_update =
          match Blackboard.get threshold_update new_threshold with
          | None ->
            Blackboard.define threshold_update new_threshold (Some set_update)
          | Some set ->
            Blackboard.overwrite threshold_update new_threshold
              (Some (Mods.IntSet.union set set_update))
        in
        let threshold_old =
          match Blackboard.get threshold_old old_threshold with
          | None -> Blackboard.define threshold_old old_threshold (Some old_set)
          | Some set ->
            Blackboard.overwrite threshold_old old_threshold
              (Some (Mods.IntSet.union set old_set))
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
            (Some (Mods.IntSet.minus set a)))
      threshold_update threshold_update
  in
  let threshold_update, (t, updates) =
    Blackboard.fold_and_flush
      (fun current_threshold set (t, updates) ->
        Mods.IntSet.fold
          (fun id (t, updates) ->
            let t, previous_threshold =
              if old_exists t id then (
                let t, size = get_old_size t id in
                match size with
                | None -> t, 0
                | Some size -> t, threshold size
              ) else
                t, 0
            in
            t, { id; previous_threshold; current_threshold } :: updates)
          set (t, updates))
      threshold_update (t, [])
  in
  let threshold_old, () =
    Blackboard.fold_and_flush (fun _ _ () -> ()) threshold_old ()
  in
  let to_check_bind = Mods.IntSet.empty in
  let to_check_fresh = Mods.IntSet.empty in
  let to_check_unbind = Mods.IntSet.empty in
  let t =
    {
      t with
      to_check_bind;
      to_check_fresh;
      to_check_unbind;
      threshold_update;
      threshold_old;
    }
  in
  flush_updates t, updates

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

let build_threshold s =
  if Mods.IntSet.is_empty s then
    fun _ ->
  0
  else (
    let l = Mods.IntSet.fold (fun i l -> i :: l) s [] in
    let max =
      match l with
      | t :: _ -> t
      | [] -> assert false
    in
    let l = List.rev l in
    let array = Array.make max 0 in
    let rec aux k acc current =
      if k = max then
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
    let f i =
      if i < max then
        array.(i)
      else
        max
    in
    f
  )

(*
  let do_it () = 
    let s = Mods.IntSet.add 10 (Mods.IntSet.singleton 100) in 
    let f = build_threshold s in 
    let () = 
     List.iter (fun i -> Format.fprintf Format.std_formatter "%i -> %i @." i (f i)) 
      [1;2;3;9;10;11;99;100;104]
  in ()*)
