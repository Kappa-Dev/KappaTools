module A = Array (*LargeArray.GenArray*)
module S = Mods.IntSet 
module M = Mods.IntMap

let enable_gc = false
let stat_trans_closure_for_big_graphs = true

let create_gc = 
  if enable_gc 
  then 
    let create p n clean = 
      let n_succ = A.create n 0 in 
      let add n = A.set n_succ n (succ (A.get n_succ n)) in 
      let sub n = 
        let x = pred (A.get n_succ n) in 
        let _ = 
          if x=0 && not (p n)
          then 
            clean x 
        in 
        A.set n_succ n x 
      in add,sub
    in create 
  else 
    let create _ _ _ = (fun _ -> ()),(fun _ -> ())
    in create 

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
      
let closure prec to_keep = 
  let max_index = 
    M.fold 
      (fun i _ -> max i)
      prec 
      0 
  in 
  let _ = 
    if stat_trans_closure_for_big_graphs && max_index > 300 
    then 
      let n_edges = 
        M.fold 
          (ignore_fst
             (S.fold 
                (ignore_fst succ)
             ))
          prec 0 
      in let _ = Debug.tag ("Transitive closure ("^(string_of_int max_index)^" nodes, "^(string_of_int n_edges)^" edges\n") in 
         let _ = flush stderr in 
         ()
    else ()
  in 
  let do_tick,tick = 
    if max_index > 300 
    then 
      let tick = Mods.tick_stories max_index (false,0,0) in 
      let f = Mods.tick_stories max_index in 
      f,tick 
    else 
      (fun x -> x),(false,0,0)
  in 
  let l_pred = A.create (max_index+1) S.empty in 
  let s_pred_star = A.create (max_index+1) [] in 
  let add_succ,remove_succ = 
    create_gc to_keep (succ max_index) (fun i -> A.set s_pred_star i [])
  in 
  let _ = 
    M.iter  
      (fun succ s_pred -> 
        let l = s_pred in           
        let _ = A.set l_pred succ l in 
        S.iter 
          (fun pred -> add_succ pred)
          s_pred 
      )
      prec 
  in
  let _ = 
    M.fold 
      (fun succ s_pred  tick -> 
        begin
          let pred_star = 
            let l_pred = S.fold (fun i j -> i::j) s_pred [] in 
            let rec aux (l:int list) (accu:int list) = 
              match l with 
              | [] -> accu 
              | pred::t -> 
                let new_l = A.get s_pred_star pred in 
                let _ = remove_succ pred in 
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
          let tick = do_tick tick in 
          tick           
        end)
      prec tick 
  in 
  s_pred_star 

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
