module A = Array (*LargeArray.GenArray*)
module S = Mods.IntSet 
module M = Mods.IntMap

let enable_gc = true
let stat_trans_closure_for_big_graphs = true
let cut_transitive_path = true
let detect_separable_components = Parameter.do_detect_separable_components 

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

let is_sublist p a b = 
  let rec aux a b = 
    match a,b 
    with 
    | l,[] -> false 
    | [],l -> true 
    | h::t,h'::t' when h=h' -> aux t t' 
    | h::t,h'::t' when p h h' -> false 
    | h::t,h'::t' -> aux a t' 
  in aux a b 

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
(*let is_sublist_decreasing = is_sublist (swap compare_bool)*)

(* TO DO: deal with created agents in the optimization *) 
      
let closure prec to_keep weak_events init  = 
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
      in 
      let _ = Debug.tag ("\t\tTransitive closure ("^(string_of_int max_index)^" nodes, "^(string_of_int n_edges)^" edges)") in 
      let _ = flush stderr in 
      ()
    else 
      ()
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
  let s_pred_star = A.create (max_index+1) ([],0) in 
  let clean,max_succ,set_succ,redirect,subs  = 
    if enable_gc or detect_separable_components 
    then 
      begin 
        let max_succ = A.create (max_index+1) 0 in 
        let set_succ = A.create (max_index+1) S.empty in 
        let redirect_tab = A.create (max_index+1) None in 
        let redirect i j = 
          A.set redirect_tab i (Some j) in 
        let subs i = 
          match 
            A.get redirect_tab i 
          with 
            None -> [i]
          | Some j -> j 
        in 
        let _ = 
          M.iter
            (fun succ -> 
              S.iter 
                (fun pred -> 
                  let _ = A.set max_succ pred (max succ (A.get max_succ pred)) in 
                  let _ = A.set set_succ pred (S.add succ (A.get set_succ pred)) in  ()))
            prec
        in 
        
        let is_last_succ_of = A.create (max_index+1) [] in 
        
        let add node max_succ = 
          if not (to_keep node)
          then 
            let old_l = A.get is_last_succ_of max_succ in 
            let l' = node::old_l in 
            A.set is_last_succ_of max_succ l'
        in 
        let _ = 
          A.iteri 
            add 
            max_succ 
        in 
        let _ = A.set is_last_succ_of 0 [] in 
        let gc_when_visit node = 
          List.iter 
            (fun k -> A.set s_pred_star k ([],0))
            (A.get is_last_succ_of node)
        in 
        let gc_when_visit = 
          if enable_gc 
          then gc_when_visit 
          else (fun _ -> ())
        in 
        let set_succ,redirect,subs = 
          if detect_separable_components 
          then 
            (fun i -> A.get set_succ i),
            redirect,
            subs
          else 
            (fun _ -> S.empty),
            (fun _ _ -> ()),
            (fun i -> [i])
        in 
        gc_when_visit,
        (fun i -> A.get max_succ i),
        set_succ,
        redirect,
        subs
      end 
    else  
      (fun _ -> ()),
      (fun _ -> (max_index+1)),
      (fun _ -> S.empty),
      (fun _ _ -> ()),(fun i -> [i])
  in 
  let is_init x = 
    S.is_empty (M.find x prec) 
  in 
  let is_like_init x = 
    match 
      subs x 
    with 
    | [y] -> not (x=y)
    | _ -> true 
  in 
  let does_not_count x = 
    (*to_keep x or*) is_like_init x or is_init x  
  in 
    
  let _ = 
    M.fold 
      (fun succ s_pred  tick -> 
        begin
          let pred_star,max_out = 
            let l_pred = S.fold (fun i j -> i::j) s_pred [] in 
            let rec aux (l:int list) (accu:int list) max_out = 
                match l with 
                | [] -> accu,max_out
                | pred::t ->
                  let l_pred  = subs pred 
                  in 
                  match 
                    l_pred 
                  with 
                    [x] when x=pred 
                      -> 
                        begin 
                          let new_l,max_out' = A.get s_pred_star pred in 
                          let max_out' = 
                            if does_not_count pred or to_keep pred 
                            then 0 
                            else max_out' 
                          in 
                          let diff = 
                            if cut_transitive_path 
                            then 
                              diff_list_decreasing t new_l 
                            else 
                              t 
                          in 
                          aux 
                            diff
                            (merge_list_decreasing (pred::new_l) accu)
                            (if new_l=[] then max_out else max max_out max_out')
                        end 
                  | _ -> 
                    begin
                      let l_pred = 
                        List.sort (fun a b -> compare b a) l_pred 
                      in 
                      aux 
                        t 
                        (merge_list_decreasing 
                           l_pred  
                           accu
                        )
                        max_out
                             end 
                      
              in 
              aux 
                l_pred 
                []
                0 
          in 
          let _ = 
            if (try ((does_not_count max_out or to_keep max_out or max_out <= succ) && weak_events succ ) with _ -> false)
             && 
                begin 
                  let s = 
                    List.fold_left 
                      (fun s' elt -> if does_not_count elt or (to_keep elt && elt<=succ) then s' 
                        else S.union (set_succ elt) s')
                      S.empty pred_star 
                  in
                  let l = List.rev (S.elements s) in 
                  let b = diff_list_decreasing l (succ::pred_star) in 
(*(*                  let b =*) is_sublist_decreasing l (succ::pred_star) (*in*)*)
(*                  diff_list_decreasing l (succ::pred_star)  
                  in *)
                  List.for_all (fun a -> does_not_count a or (to_keep a && a<=succ)) b 
                end 
            then 
              let init_succ = List.sort (fun a b -> compare b a) (init succ) in 
              let _ = redirect succ init_succ in 
              match init_succ 
              with [x] when x=succ -> () 
              | _ -> 
                A.set s_pred_star succ (init_succ,0)
            else 
              A.set s_pred_star succ (pred_star,max (max_succ succ) max_out)
          in
          let _ = clean succ in 
          let tick = do_tick tick in 
          tick           
        end)
      prec tick 
  in 
  s_pred_star 


(*let generate_triangle n = 
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
let _ = closure t (fun x -> x - (x/100)*100 = 0) 
let _ = flush stderr 
*)
