type id = int  
type update = {id: id ; previous_size: int ; current_size: int} 
type updates = update list 

type t = 
 {
  rep: int option Mods.DynArray.t ; 
  back: Mods.IntSet.t Mods.DynArray.t ; 
  size: int Mods.DynArray.t; 

  blackboard : int option Mods.DynArray.t ; 
  blackboard_keys: id list ;  
  to_check: Mods.IntSet.t ; 
  updates: update option Mods.DynArray.t ; 
  update_keys: id list; 
 }

(*val bind: t -> id -> id -> t
val check: neightboor:(id -> id list) -> t -> id list -> t  
val flush_updates: t -> t * updates 
val convert_updates: updates -> (int * int * int) list *)

let init = 
  {
    blackboard = Mods.DynArray.create 0 None ; 
    blackboard_keys = [];
    to_check = Mods.IntSet.empty ; 
    updates = Mods.DynArray.create 0 None; 
    update_keys = [] ;    
    rep = Mods.DynArray.create 0 None ; 
    size = Mods.DynArray.create 0 0 ; 
    back = Mods.DynArray.create 0 (Mods.IntSet.empty);  
     }

let get_rep t i = 
  let rec aux t x l = 
    let rep_opt = Mods.DynArray.get t.rep x in 
    match rep_opt with 
      | Some rep -> 
        if x = rep then 
          let t = List.fold_left  (fun t x -> let () = Mods.DynArray.set t.rep x (Some rep) in t) t l in 
           t, rep 
        else 
        aux t rep (x::l)
        | None -> (* TO DO ERROR *) assert false 
  in aux t i []

let get_rep_size t i = 
  t, Mods.DynArray.get t.size i 

let get_size t i = 
  let t, rep = get_rep t i in 
  get_rep_size t rep 

let add_update t update = 
  match 
    Mods.DynArray.get t.updates update.id 
  with 
  | None -> 
    let t, previous_size = get_size t update.id in 
    if previous_size = update.current_size then 
      t
    else 
      let id = update.id in 
      let () = 
          Mods.DynArray.set 
            t.updates 

          update.id 
            (Some {id ; previous_size; current_size = update.current_size})
      in 
      let update_keys = id :: t.update_keys  in 
      {t with  update_keys} 
  
  | Some last -> 
    let () = 
      Mods.DynArray.set t.updates  update.id (Some {last with current_size = update.current_size}) 
    in  t 
    
let flush_updates t =
  let updates, l = 
    List.fold_left 
      (fun (updates,l) i -> 
        match Mods.DynArray.get t.updates i with 
        | None -> (updates, l)
        | Some u -> 
          let () = Mods.DynArray.set updates i None in 
          updates, 
         (u)::l)
      (t.updates,[])  
      t.update_keys
  in 
  {t with updates}, l 

let create t i = 
  let () = Mods.DynArray.set t.rep i (Some i) in 
  let () = Mods.DynArray.set t.size i 1 in 
  let () = Mods.DynArray.set t.back i (Mods.IntSet.singleton i) in 
  add_update t {id=i; previous_size=0; current_size=1} 

let get_back_set t i = Mods.DynArray.get t.back i 

let bind t i j = 
  let t, repi = get_rep t i in 
  let t, repj = get_rep t j in 
  if repi = repj then 
     t 
  else 
     let t, sizei = get_size t repi in 
     let t, sizej = get_size t repj in 
     let size = sizei + sizej in 
     let repi, repj = if repi < repj then repi, repj else repj, repi in 
     let () = Mods.DynArray.set t.rep repj (Some repi) in 
     let () = Mods.DynArray.set t.size repi size in 
     let cc = Mods.IntSet.union (get_back_set t repi)  (get_back_set t repj) in 
     let () = Mods.DynArray.set t.back repi cc in 
     let () = Mods.DynArray.set t.back repj (Mods.IntSet.empty) in 
     t 

(* TO DO *)
let unbind t i j = 
  let to_check = t.to_check in 
  let to_check = Mods.IntSet.add i to_check in 
  let to_check = Mods.IntSet.add j to_check in 
  {t with to_check}  

let degrade ~neightbor:f t i = 
  let list = f i in 
  List.fold_left 
    (fun t j -> unbind t i j)
    t list 
 
let scan i l = 
    let rec aux i l acc = 
          match l with 
           | (k,d)::t when k = i -> (List.rev acc)@t, Some d 
           | (k,d)::t -> aux i t ((k,d)::t)
           | [] -> List.rev acc, None 
    in  
    aux i l []

let scan2 i l l' = 
  match scan i l with 
  | l, Some a -> Some a, l, l' 
  | _, None -> 
    begin
      let l', a = scan i l' in a, l, l' 
    end 


let flush ~neightbor:f t  = 
  let set = t.to_check in 
  let l = Mods.IntSet.fold (fun i l -> (i,[i])::l) set [] in 
  let rec aux to_visit to_visit_after t alias = 
    match to_visit, to_visit_after with 
      | [],[] -> t, alias 
      | [], _ -> aux (List.rev to_visit_after) [] t alias 
      | (_, [])::tail', _ -> aux tail' to_visit_after t alias 
      | (i,(h::tail))::tail', _ -> 
        begin 
          match Mods.DynArray.get t.blackboard h with 
          | None -> 
            let () = Mods.DynArray.set t.blackboard h (Some i) in 
            let blackboard_keys = h::t.blackboard_keys in 
            let t = {t with blackboard_keys} in 
            let tail = 
              List.fold_left 
                (fun l x -> x::l)
                tail (f h)
            in 
            aux tail' ((i,tail)::to_visit_after) t alias 
          | Some i' when i=i' -> 
            (* already seen, nothing to do *)
            aux tail' ((i,tail)::to_visit_after) t alias  
          | Some i' ->  
            (* seen in another equialent class -> merge *)
            merge (i,tail) i' tail' to_visit_after t ((i,i')::alias)
        end  
        
  and merge (i,l1) j to_visit to_visit_after t alias  = 
      let lj_opt, to_visit, to_visit_after = scan2 j to_visit to_visit_after in 
      match  lj_opt with 
        | None -> assert false 
        | Some l2 -> 
          begin 
            match to_visit, to_visit_after with 
            | [], [] -> t, []
            | _, _ -> 
               let l = l1@l2 in 
               let i = min i j in 
               let to_visit_after = (i,l)::to_visit_after in 
               aux to_visit to_visit_after t alias 
          end
  in 
  let t, aliases  = aux l [] t [] in
  let t = 
    match aliases with 
   | [] -> t 
   | _ -> 
    begin 
       let aliases = List.rev_map (fun (i,j) -> if i<j then i,j else j,i) aliases in  
       let aliases = List.sort (fun (a,_) (b,_) -> compare a b) aliases in 
       let aliases = 
        List.fold_left 
          (fun m (i,j) -> 
             let im = 
                match Mods.IntMap.find_option i m with 
                | None -> i 
                | Some j -> j 
             in 
             Mods.IntMap.add j im m) 
          Mods.IntMap.empty aliases   
       in
       let rec split l m = 
        match l with 
          | [] -> m 
          | h::q -> 
            begin 
              match Mods.IntMap.find_option h aliases with 
                | None -> assert false 
                | Some a -> 
                  let oldset = 
                    match Mods.IntMap.find_option a m with 
                    | None -> []
                    | Some a -> a 
                  in 
                  split q (Mods.IntMap.add a (h::oldset) m) 
            end 
        in 
        let m = split t.blackboard_keys Mods.IntMap.empty in 
        let _ = m in 
        (* iterate over blackboard_keys, to change representant *)
       (* iterater *)
       t 
    end 
  in 
  flush_updates t 
