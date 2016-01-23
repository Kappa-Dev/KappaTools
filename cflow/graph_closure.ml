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
let check_mode = false

type algo_type = Top_down | Bottom_up | Check
type order = Increasing_with_last_event | Decreasing_without_last_event

type config =
  {
    do_tick: bool ;
    keep_all_nodes: bool ;
    cut_transitive_path: bool ;
    stat_trans_closure_for_big_graphs: bool;
    max_index:int;
    algo: algo_type
  }

let config_big_graph_with_progress_bar =
  {
   do_tick = true;
    keep_all_nodes=false;
    cut_transitive_path=true ;
    stat_trans_closure_for_big_graphs=true;
    max_index=300;
    algo= Bottom_up (*if check_mode then Check else Top_down*) ;
  }

let config_big_graph_without_progress_bar =
  { config_big_graph_with_progress_bar with do_tick = false}

let config_small_graph =
  {
    do_tick = false;
    keep_all_nodes = true ;
    cut_transitive_path = false ;
    stat_trans_closure_for_big_graphs=true;
    algo= Bottom_up ;
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
      | b::_ when p a b -> compare_succ p q
      | _ -> false
    end


let strictly_increasing = compare_succ (fun (a:int) b -> a < b)
let strictly_decreasing = compare_succ (fun (a:int) b -> a > b)

let concat a = List.rev_append (List.rev a)

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
    | l,[] | [],l ->  List.rev (List.rev_append l accu)
    | h::t,h'::t' when h=h' -> aux t t' (h::accu)
    | h::t,h'::_ when p h h' -> aux t b (h::accu)
    | _,h'::t' -> aux a t' (h'::accu)
  in aux a b []

let is_strict_sublist p a b =
  let rec aux a b =
    match a,b with
    | _,[] -> false
    | [],_::_ -> true
    | h::t,h'::t' when h=h' -> aux t t'
    | h::_,h'::_ when p h h' -> false
    | _,_::t' -> aux a t'
  in aux a b

let insert_elt p e = merge_list p [e]

let diff_list p a b =
  let rec aux a b accu =
    match a,b
    with
    | b,[] -> List.rev (List.rev_append b accu)
    | [],_::_ -> List.rev accu
    | h::t,h'::t' when h=h' -> aux t t' accu
    | h::t,h'::_ when p h h' -> aux t b (h::accu)
    | _,_::t' -> aux a t' accu
  in aux a b []

let compare_bool a b = compare a b < 0
let diff_list_decreasing =  diff_list (swap compare_bool)
let merge_list_decreasing = merge_list (swap compare_bool)


let closure_bottom_up_with_fold parameter handler log_info error event config prec is_obs f a  =
  let err_logger = Remanent_parameters.get_logger parameter in
  let err_fmt = Loggers.formatter_of_logger err_logger in
  let is_obs = if config.keep_all_nodes then (fun _ -> true) else is_obs in
  let max_index = M.fold (fun i _ -> max i) prec 0 in
  let () =
    if config.stat_trans_closure_for_big_graphs && config.max_index > 300
    then
      let n_edges =
        M.fold (ignore_fst (S.fold (ignore_fst succ))) prec 0 in
      let () = Loggers.print_newline err_logger in
      let () = Loggers.fprintf err_logger "\t\tTransitive closure (%i nodes, %i edges)" max_index n_edges in
      let () = Loggers.print_newline err_logger in
      ()
  in
  let do_tick,tick,close_tick =
    if max_index > 300 && config.do_tick
    then
      begin
	match
	  err_fmt
	with
	| None ->
	  (fun x -> x),(false,0,0),(fun () -> ())
	| Some err_fmt ->
	  let tick = Mods.tick_stories err_fmt max_index (false,0,0) in
	  let f = Mods.tick_stories err_fmt max_index in
	  let close = Format.pp_print_newline err_fmt in
	  f,
	  tick,
	  close
      end
    else
      (fun x -> x),(false,0,0),(fun () -> ())
  in
  let s_pred_star = A.make (max_index+1) [] in
  let clean,max_succ =
    begin
      let max_succ = A.init (max_index+1) (fun i -> i) in
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
          (fun k -> A.set s_pred_star k [])
          (A.get is_last_succ_of node)
      in
      gc_when_visit,
      (fun i -> A.get max_succ i)
    end
  in
  let error,log_info =
    match
      event
    with
    | None -> error,log_info
    | Some e -> StoryProfiling.StoryStats.add_event parameter error e None log_info
  in
  let output =
    M.fold_with_interruption
      (fun succ s_pred (tick,error,log_info,counter,a) ->
        begin
          let rec aux (l:int list) (accu:int list) =
            match l with
            | [] -> accu
            | pred::t ->
              begin
                let new_l = A.get s_pred_star pred in
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
              end
          in
          let pred_star =
            let l_pred = S.fold (fun i j -> i::j) s_pred [] in
            let s = A.get s_pred_star succ in
            aux l_pred s
          in
          let _ =
            A.set s_pred_star succ pred_star
          in
	  let _ = clean succ in
          let tick = do_tick tick in
	  if is_obs succ
	  then
	    let error,log_info,a = f parameter handler log_info error succ pred_star a in
	    Stop.success_or_stop
	      (fun a -> Stop.success (tick,error,log_info,counter+1,a))
	      (fun b -> Stop.stop (error,log_info,b))
	      a
	  else
	    Stop.success (tick,error,log_info,counter,a)

	end)
      prec (tick,error,log_info,1,a)
  in
  let _ = close_tick () in
  Stop.success_or_stop
    (fun (_,error,log_info,_,a) ->
     let error,log_info =
       match event
       with
	 None -> error,log_info
       | Some e ->
	  StoryProfiling.StoryStats.close_event parameter error e None log_info
     in
     Stop.success (error,log_info,a))
    (fun (error,log_info,b) ->
     let error,log_info =
       match event
       with
	 None -> error,log_info
       | Some e ->
	  StoryProfiling.StoryStats.close_event parameter error e None log_info
     in
     Stop.stop (error,log_info,b))
    output

let closure_bottom_up parameter handler log_info error event_opt config prec is_obs =
  let max_index = M.fold (fun i _ -> max i) prec 0 in
  let s_pred_star = A.make (max_index+1) [] in
  let f p h c e i s a =
    let _ = A.set a i s in (e,c,Stop.success a)
  in
  let output = closure_bottom_up_with_fold parameter handler log_info error event_opt config prec is_obs
					   f s_pred_star in
  Stop.success_or_stop
    (fun (error,log_info,graph) ->
     error,log_info,(graph,Decreasing_without_last_event))
    (fun (error,log_info,_) -> error,log_info,(s_pred_star,Decreasing_without_last_event))
    output

let closure_top_down parameter handler log_info error event_opt config prec is_obs  delta =
  let err_logger = Remanent_parameters.get_logger parameter in
  let err_fmt = Loggers.formatter_of_logger err_logger in
  let is_obs = if config.keep_all_nodes then (fun _ -> true) else is_obs in
  let max_index = M.fold (fun i _ -> max i) prec 0 in
  let prec =
    M.fold (fun i s_pred l -> (i,s_pred)::l) prec []
  in
  let create_taints i =
    let rec aux delta output =
      if delta = 0 then output
      else aux (delta - 1) (S.empty::output)
    in
    aux delta [S.singleton i]
  in
  let shift_taints l =
    match l with t::t'::q -> (S.union t t')::q
	       | _ -> l
  in
  let rec merge_taints l1 l2 =
    match
      l1,l2
    with
    | _,[] -> l1
    | [],_ -> l2
    | t::q,t'::q' ->
       (S.union t t')::(merge_taints q q')
  in
  let s_pred_star = A.make (max_index+1) [] in
  let taint i taints =
    match
      taints
    with [] -> ()
       | t::q ->
	  S.iter
	    (fun taint -> A.set s_pred_star taint (i::(A.get s_pred_star taint)))
	    t
  in
  let do_tick,tick,close_tick =
    if max_index > 300 && config.do_tick
    then
      match
	err_fmt
      with
      | None -> (fun x->x),(false,0,0),(fun () -> ())
      | Some err_fmt ->
	let tick = Mods.tick_stories err_fmt max_index (false,0,0) in
	let f = Mods.tick_stories err_fmt max_index in
	let close = Format.pp_print_newline err_fmt in
	f,tick,close
    else
      (fun x -> x),(false,0,0),(fun () -> ())
  in
  let tainting = A.make (max_index+1) [] in
  let _ =
    List.fold_left
      (fun tick (i,s_pred) ->
       let new_taint =
	 if is_obs i
	 then
	   create_taints i
	 else
	   []
       in
       let taints = merge_taints new_taint (A.get tainting i) in
       let () = taint i taints in
       let shifted_taints = shift_taints taints in
       let taint x = A.set tainting x (merge_taints shifted_taints  (A.get tainting x)) in
       let () = S.iter taint s_pred in
       let () = A.set tainting i [] in
       do_tick tick)
      tick prec
  in
  let () = close_tick () in
  error,log_info,(s_pred_star,Increasing_with_last_event)

let get_list_in_increasing_order_with_last_event i (m,mode) =
  match
    mode
  with
  | Increasing_with_last_event -> m.(i)
  | Decreasing_without_last_event ->
     begin
       match
	 m.(i)
       with
	 [] -> []
       | l -> List.rev (i::l)
     end

let closure_check parameter handler log_info error event_opt config prec is_obs =
  let t = Sys.time () in
  let error,log_info,(a,a') = closure_top_down parameter handler log_info error event_opt config prec is_obs 0 in
  let t' = Sys.time () in
  let error,log_info,(b,b') = closure_bottom_up parameter handler log_info error event_opt {config with do_tick = false} prec is_obs in
  let t'' = Sys.time () in
  let _ = Printf.fprintf stderr "NEW: %f OLD: %f \n" (t'-.t) (t''-.t') in
  let _ =
    A.iteri
      (fun i s ->
       let s = get_list_in_increasing_order_with_last_event i (a,a') in
       let s' = get_list_in_increasing_order_with_last_event i (b,b') in
       if s = s' then ()
       else
	 let _ = Printf.fprintf stderr "DIFFER %i\n" i in
	 let _ = List.iter (Printf.fprintf stderr "%i, ") s in
	 let _ = Printf.fprintf stderr "\n" in
	   let _ = List.iter (Printf.fprintf stderr "%i, ") s' in
	   let _ = Printf.fprintf stderr "\n" in
	   ())
      a
  in error,log_info,(a,a')

let closure parameter handler log_info error event_opt config prec is_obs =
  match
    config.algo
  with
  | Check -> closure_check parameter handler log_info error event_opt config prec is_obs
  | Bottom_up -> closure_bottom_up parameter handler log_info error event_opt config prec is_obs
  | Top_down -> closure_top_down parameter handler log_info error event_opt config prec is_obs 0
		

let reduction_top_down parameter handler log_info error prec =
  let error,log_info = StoryProfiling.StoryStats.add_event parameter error StoryProfiling.Graph_reduction None log_info in
  let error,log_info,(prec_star,_) = closure_top_down parameter handler log_info error StoryProfiling.Transitive_closure config_big_graph_without_progress_bar prec (fun _ -> true) 2 in
  let output =
    M.fold
      (fun eid neigh out ->
       let to_remove = A.get prec_star eid in
       let s =
	 S.fold (fun i l -> i::l) neigh []
	   in
	   let s = List.rev s in
	   let rec aux l1 l2 output =
	     match l1,l2 with
	       _,[] -> List.fold_left (fun set i -> S.add i set) output l1 (* This is quite annoying, why prec is not described with ordered list *)
	     | [],_ -> output
	     | h::q,h'::q' ->
		let cmp = compare h h' in
		if cmp < 0 then aux q l2 (S.add h output)
		else if cmp = 0 then aux q q' output
		else  aux (h::q) q' output
	   in
	   let s = aux s to_remove S.empty in
	   M.add eid s out
      )
      prec prec
  in
  let error,log_info = StoryProfiling.StoryStats.close_event parameter error StoryProfiling.Graph_reduction None log_info in
  error,log_info,output

let reduction = reduction_top_down 	
