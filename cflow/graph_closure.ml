module A = Array (*LargeArray.GenArray*)
module S = Mods.IntSet 
module M = Mods.IntMap

let swap f a b = f b a 
let ignore_fst f = (fun _ -> f) 

let rec compare_succ p l = 
  match l with 
  | [] | [_] -> true 
  | a::q -> 
    begin 
      match q with 
      | b::t when p a b -> compare_succ p q
      | _ -> false 
    end


let strictly_increasing = compare_succ (fun (a:int) b -> a < b)
let strictly_decreasing = compare_succ (fun (a:int) b -> a > b)

let concat_rev a b = 
  List.fold_left 
    (fun list t -> 
      t::list)
    b 
    a 

let concat a = concat_rev (List.rev a)

let print_list l = 
  let _ = List.iter (Printf.fprintf stderr "%i,") l in 
  let _ = Printf.fprintf stderr "\n" in 
  let _ = flush stderr in 
  ()




let check p f string a b = 
  match p a,p b
  with 
  | false,false -> (let _ = print_list a in 
                    let _ = print_list b in 
                    failwith (string^"_arg1_2"))
  | true,false  -> (print_list b;failwith (string^"_arg2"))
  | false,true -> (let _ = print_list a in 
                   failwith (string^"_arg1"))
  | true,true -> 
    let rep = f a b in 
    let _ = print_list rep in 
    if p rep 
    then 
      rep
    else 
      (print_list a;print_list b;print_list rep;failwith (string^"_output"))

let merge_list p a b = 
  let rec aux a b accu =
      match a,b 
      with 
      | l,[] | [],l ->  List.rev (concat_rev l accu)
      | h::t,h'::t' when h=h' -> aux t t' (h::accu)
      | h::t,h'::t' when p h h' -> aux t b (h::accu)
      | h::t,h'::t' -> aux a t' (h'::accu)
  in aux a b []

let insert_elt p e = merge_list p [e]
  
let diff_list p a b = 
  let rec aux a b accu = 
    match a,b 
    with 
    | b,[] -> List.rev (concat_rev b accu)
    | [],b -> List.rev accu 
    | h::t,h'::t' when h=h' -> aux t t' accu
    | h::t,h'::t' when p h h' -> aux t b (h::accu) 
    | h::t,h'::t' -> aux a t' accu 
  in aux a b []

let compare_bool a b = compare a b < 0 
let diff_list_decreasing =  diff_list (swap compare_bool)
let merge_list_decreasing = merge_list (swap compare_bool)
      
let closure_old prec = 
  let max_index = 
    M.fold 
      (fun i _ -> max i)
      prec 
      0 
  in 
  let tick = 
    if max_index > 0 
    then Mods.tick_stories max_index (false,0,0) 
    else (false,0,0)
  in 
  let n_pred = A.create (max_index+1) 0 in 
  let l_succ = A.create (max_index+1) S.empty in 
  let l_pred = A.create (max_index+1) S.empty in 
  let s_pred_star = A.create (max_index+1) S.empty in 
  let add (pred:int) (succ:int) = 
    let _ = A.set l_succ pred (S.add succ (A.get l_succ pred )) in 
    let _ = A.set n_pred succ (1+(A.get n_pred succ)) in 
    ()
  in 
  let remove pred succ to_do = 
    let k = (A.get n_pred succ)-1 in 
    let _ = A.set n_pred succ k in 
    if k = 0 
    then 
      S.add succ to_do 
    else 
      to_do 
  in 
  let _ = 
    M.iter  
      (fun succ s_pred -> 
        let l = s_pred in 
        let _ = A.set l_pred succ l in 
        S.iter 
          (fun pred -> add pred succ)
          s_pred  
      )
      prec 
  in 
  let l = ref S.empty in 
  let _  = 
    A.iteri  
      (fun i n  -> 
        if n=0 then l:= S.add i (!l))
      n_pred 
  in 
  let to_do = !l in 
  let rec aux tick to_do = 
    if S.is_empty to_do
    then s_pred_star
    else 
      let succ,to_do = S.destruct_min to_do in 
      begin
        let pred_star = 
          let rec aux l accu = 
            if S.is_empty l 
            then accu 
            else 
              let pred,t = S.destruct_max l in 
              let new_l = A.get s_pred_star pred in 
              let l' = S.diff t new_l in 
              aux l'
                (S.add pred (S.union new_l accu))
          in 
          aux ((A.get l_pred succ)) S.empty
        in 
        let _ = 
          A.set s_pred_star succ pred_star 
        in 
        let tick = Mods.tick_stories max_index tick in 
        let to_do = 
          S.fold 
            (remove succ)
            (A.get l_succ succ)
            to_do 
        in 
        aux tick to_do 
      end 
  in 
  let t = aux tick to_do in 
  let m = ref M.empty in 
  let _ = 
    A.iteri 
      (fun i s -> m:= M.add i s (*(S.fold_inv S.add s  S.empty)*) (!m))
      t
  in 
  !m

let closure prec = 
  let max_index = 
    M.fold 
      (fun i _ -> max i)
      prec 
      0 
  in 
  let n_edges = 
   M.fold 
      (ignore_fst
        (S.fold 
          (ignore_fst succ)
        ))
      prec 0 
  in 
  let _ = Printf.fprintf stderr "Transitive closure (%i nodes, %i edges)\n" max_index n_edges in 
  let _ = flush stderr in 
  let tick = 
    if max_index > 0 
    then Mods.tick_stories max_index (false,0,0) 
    else (false,0,0)
  in 
(*  let n_pred = A.create (max_index+1) 0 in *)
(*  let l_succ = A.create (max_index+1) S.empty in *)
  let l_pred = A.create (max_index+1) S.empty in 
  let s_pred_star = A.create (max_index+1) [] in 
(*  let add (pred:int) (succ:int) = 
    let _ = A.set l_succ pred (S.add succ (A.get l_succ pred )) in 
    let _ = A.set n_pred succ (1+(A.get n_pred succ)) in 
    ()
  in*) 
(*  let remove pred succ to_do = 
    let k = (A.get n_pred succ)-1 in 
    let _ = A.set n_pred succ k in 
    if k = 0 
    then 
      S.add succ to_do 
    else 
      to_do 
  in *)
  let _ = 
    M.iter  
      (fun succ s_pred -> 
        let l = s_pred in 
        let _ = A.set l_pred succ l in 
     (*   S.iter 
          (fun pred -> add pred succ)
          s_pred  *)
        ()
      )
      prec 
  in
(*  let l = ref S.empty in 
  let _  = 
    A.iteri  
      (fun i n  -> 
        if n=0 then l:= S.add i (!l))
      n_pred 
  in *)
(*  let to_do = !l in *)
(* 
  let rec aux tick to_do = *)
  let _ = 
    M.fold 
      (fun succ s_pred  tick              -> 
        begin
          let pred_star = 
            let l_pred = S.fold (fun i j -> i::j) s_pred [] in 
            let rec aux (l:int list) (accu:int list) = 
              match l with 
              | [] -> accu 
              | pred::t -> 
                let new_l = A.get s_pred_star pred in 
                aux 
                  (diff_list_decreasing t new_l) 
                  (merge_list_decreasing (pred::new_l) accu)
            in 
            aux 
              l_pred 
              []
          in 
          let _ = 
            A.set s_pred_star succ pred_star 
          in 
          let tick = Mods.tick_stories max_index tick in 
          tick           
        end)
      prec tick 
  in 
  s_pred_star 
(*
  let m = ref M.empty in 
  let _ = 
    A.iteri 
      (fun i s -> m:= M.add i s (*(List.fold_left (swap S.add) S.empty s)*) (!m))
      s_pred_star
  in 
  !m*)

(*
let generate_triangle n = 
let rec aux k set map =       
  if k > n 
  then map 
  else 
    aux 
      (k+1) 
      (S.add k set)
      (M.add k set map)
in aux 1 S.empty M.empty 


let t = generate_triangle 10000
let _ = Printf.fprintf stderr "generated\n"
let _ = flush stderr 
let _ = closure t
let _ = Printf.fprintf stderr "OK\n" 
let _ = flush stderr 
*)
