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

let ignore_flow_from_outgoing_siphon = true
					 
type config = 
  { 
    do_tick: bool ;
    enable_gc: bool ; 
    cut_transitive_path: bool ; 
    stat_trans_closure_for_big_graphs: bool;
    max_index:int
  }

let config_init = 
  {
   do_tick = true;
    enable_gc=true;
    cut_transitive_path=true ;
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

let print_list f l =
  Format.fprintf f "@[%a@]@." (Pp.list Pp.comma Format.pp_print_string) l

let check form p f string a b =
  match p a,p b with
  | false,false -> let () = print_list form a in
                   let () = print_list form b in
                   failwith (string^"_arg1_2")
  | true,false  -> let () = print_list form b in failwith (string^"_arg2")
  | false,true -> let () = print_list form a in failwith (string^"_arg1")
  | true,true ->
     let rep = f a b in
     let () = print_list form rep in
     if p rep then rep
     else
       (print_list form a;print_list form b;print_list form rep;
	failwith (string^"_output"))

let merge_list p a b =
  let rec aux a b accu =
    match a,b with
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

let closure err_fmt config prec is_obs init_to_eidmax weak_events init =
  let max_index = M.fold (fun i _ -> max i) prec 0 in
  let is_init x =
    try
      let s = M.find x prec in
      S.is_empty s || (S.equal s (S.singleton x))
    with _ -> true
  in
  let () =
    if config.stat_trans_closure_for_big_graphs && config.max_index > 300
    then
      let n_edges =
        M.fold (ignore_fst (S.fold (ignore_fst succ))) prec 0 in
      Format.fprintf err_fmt "@.\t\tTransitive closure (%i nodes, %i edges)@."
		     max_index n_edges in
  let do_tick,tick =
    if max_index > 300 && config.do_tick
    then
      let tick = Mods.tick_stories err_fmt max_index (false,0,0) in
      let f = Mods.tick_stories err_fmt max_index in
      f,tick
    else
      (fun x -> x),(false,0,0)
  in
  let s_pred_star = A.make (max_index+1) ([],0) in
  let clean,max_succ(*,redirect,cut_event*) = 
    if config.enable_gc 
    then 
      begin 
        let max_succ = A.make (max_index+1) 0 in 
        let _ = A.iteri (fun i _ -> A.set max_succ i (init_to_eidmax i)) max_succ in 
        let _ =
	  M.iter
            (fun succ -> 
              S.iter 
                (fun pred -> 
                  A.set max_succ pred (max succ (A.get max_succ pred))))
              prec
        in 
        let is_last_succ_of = A.make (max_index+1) [] in 
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
        gc_when_visit,
        (fun i -> A.get max_succ i)
      end 
    else  
      (fun _ -> ()),
      (fun _ -> (max_index+1))
  in 
  let _ = 
    M.fold 
      (fun succ s_pred  tick -> 
        begin
          let rec aux (l:int list) (accu:int list) max_out = 
            match l with 
            | [] -> accu,max_out
            | pred::t ->
              begin 
                let new_l,max_out' = A.get s_pred_star pred in 
                let max_out' = 
                  if  is_obs pred
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
            A.set s_pred_star succ (pred_star,max (max_succ succ) max_out)
          in 
          let _ = clean succ in 
          let tick = do_tick tick in 
          tick           
        end)
      prec tick 
  in 
  s_pred_star 



