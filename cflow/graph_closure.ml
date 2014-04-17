(**
   * graph_closure.ml 
   *
   * Algorithms to compute transitive closure of obervables in traces
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * Jean Krivine, Université Paris-Diderot, CNRS 
   * 
   * KaSim
   * Jean Krivine, Université Paris Dederot, CNRS 
   *  
   * Creation: 17/08/2013
   * Last modification: 10/09/2013
   * * 
   *  
   * Copyright 2011,2012,2013 Institut National de Recherche en Informatique  
   * et en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)


module A = Array 
module S = Mods.IntSet 
module M = Mods.IntMap

type config = 
  { 
    do_tick: bool ;
    enable_gc: bool ; 
    cut_transitive_path: bool ; 
    detect_separable_components: bool ;
    stat_trans_closure_for_big_graphs: bool;
      max_index:int
  }

let config_init = 
  {
   do_tick = true;
    enable_gc=true;
    cut_transitive_path=true ;
    detect_separable_components=true ;
      stat_trans_closure_for_big_graphs=true;
      max_index=300;
  }

let config_intermediary = 
  { config_init with do_tick = false}
    
let config_std = 
  {
    do_tick = false;
    enable_gc = false ;
    cut_transitive_path = false ;
    detect_separable_components = false;
    stat_trans_closure_for_big_graphs=true;
    max_index = 300;
  }


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

let closure config prec is_obs init_to_eidmax weak_events init = 
  let max_index = 
    M.fold 
      (fun i _ -> max i)
      prec 
      0 
  in 
  let is_init x = 
    try 
      let s = M.find x prec in 
      S.is_empty s || (S.equal s (S.singleton x))
    with _ -> true 
  in 
  let weak_events = 
    if config.detect_separable_components 
    then (fun x -> (try weak_events x with _ -> false) && (init_to_eidmax x = 0) && (not (is_obs x)))
    else (fun _ -> false)
  in 
  let _ = 
    if config.stat_trans_closure_for_big_graphs && config.max_index > 300 
    then 
      let n_edges = 
        M.fold 
          (ignore_fst
             (S.fold 
                (ignore_fst succ)
             ))
          prec 0 
      in 
      let _ = Debug.tag "" in 
      let _ = Debug.tag ("\t\tTransitive closure ("^(string_of_int max_index)^" nodes, "^(string_of_int n_edges)^" edges)") in 
      let _ = flush stderr in 
      ()
    else 
      ()
  in 
  let do_tick,tick = 
    if max_index > 300 && config.do_tick 
    then 
      let tick = Mods.tick_stories max_index (false,0,0) in 
      let f = Mods.tick_stories max_index in 
      f,tick 
    else 
      (fun x -> x),(false,0,0)
  in 
  let s_pred_star = A.create (max_index+1) ([],0) in 
  let clean,max_succ,set_succ,redirect,cut_event,is_final = 
    if config.enable_gc || config.detect_separable_components 
    then 
      begin 
        let max_succ = A.create (max_index+1) 0 in 
        let set_succ = A.create (max_index+1) S.empty in 
        let is_final x = 
          S.is_empty (A.get set_succ x) 
        in 
        let redirect_tab = A.create (max_index+1) false in 
        let redirect i = 
          A.set redirect_tab i true in 
        let subs i = 
          A.get redirect_tab i 
        in 
        let _ = A.iteri (fun i _ -> A.set max_succ i (init_to_eidmax i)) max_succ in 
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
          if not (is_obs node) && not (is_init node)
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
          if config.enable_gc 
          then gc_when_visit 
          else (fun _ -> ())
        in 
        let set_succ,redirect,subs,is_final = 
          if config.detect_separable_components 
          then 
            (fun i -> A.get set_succ i),
            redirect,
            subs,
            is_final
          else 
            (fun _ -> S.empty),
            (fun _  -> ()),
            (fun i -> false),
            (fun i -> false)
        in 
        gc_when_visit,
        (fun i -> A.get max_succ i),
        set_succ,
        redirect,
        subs,
        is_final
      end 
    else  
      (fun _ -> ()),
      (fun _ -> (max_index+1)),
      (fun _ -> S.empty),
      (fun _  -> ()),
      (fun _ -> false),
      (fun _ -> false)

  in 
  let is_like_init = cut_event 
  in 
  let does_not_count x = 
    is_like_init x || is_init x  
  in 
    
  let _ = 
    M.fold 
      (fun succ s_pred  tick -> 
        begin
          let rec aux (l:int list) (accu:int list) max_out = 
            match l with 
            | [] -> accu,max_out
            | pred::t ->
              if 
                    cut_event pred
              then 
                aux t accu max_out 
              else 
                begin 
                  let new_l,max_out' = A.get s_pred_star pred in 
                  let max_out' = 
                    if does_not_count pred  || is_obs pred
                    then 0 
                    else max_out' 
                  in 
                  let diff = 
                    if config.cut_transitive_path 
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
          in
          let pred_star,max_out = 
            let l_pred = S.fold (fun i j -> i::j) s_pred [] in 
            let l_pred = merge_list_decreasing l_pred (init succ) in
            
            let s,max_out = A.get s_pred_star succ in 
            aux 
              l_pred 
              s
              max_out 
          in 
          let _ = 
            if (does_not_count max_out || max_out <= succ || is_final max_out)
              &&
                weak_events succ 
              && 
                not (is_obs succ )
              && 
                let bool,list = 
                  begin 
                    let s = 
                      List.fold_left 
                        (fun s' elt -> 
                          if does_not_count elt || (is_obs elt && elt<=succ) || elt=succ then s' 
                          else S.union (set_succ elt) s')
                        S.empty pred_star 
                    in
                    let l = List.rev (S.elements s) in 
                    let b = diff_list_decreasing l (succ::pred_star) in 
                    let rec aux2 list accu = 
                      match list 
                      with [] -> true,accu  
                      | h::t -> 
                        if does_not_count h 
                          || (is_obs h && h<=succ) 
                          || h=succ 
                        then 
                          aux2 t accu 
                        else if is_final h
                        then 
                          aux2 t (h::accu)
                            
                        else 
                          false,[]
                    in 
                    aux2 b []
                  end 
                in 
                let _ = 
                  if bool 
                  then 
                    let set_tmp = A.get s_pred_star succ in 
                    let _ = A.set s_pred_star succ (pred_star,(max (max_succ succ) max_out)) in 
                    let _ = 
                      List.iter 
                        (fun h -> 
                          let s_pred = M.find h prec in 
                          let l_pred = S.fold (fun i j -> i::j) s_pred [] in 
                          let l_pred = merge_list_decreasing l_pred (init h) in
                        
                          let s,max_out = A.get s_pred_star succ in 
                          let pred_star,max_out = aux l_pred s max_out in 
                          let _ = A.set s_pred_star h (pred_star,max_out) in 
                          ())
                        list 
                    in 
                    let _ = A.set s_pred_star succ set_tmp in 
                    () 
                in bool 
            then 
              let _ = redirect succ  in 
              () 
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
