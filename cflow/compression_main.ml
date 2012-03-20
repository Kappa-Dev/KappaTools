(**
  * compression_main.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 19/10/2011
  * Last modification: 19/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module S = Generic_branch_and_cut_solver.Solver 

let debug_mode = false

let weak_compression env state step_list =  
  let _ = print_newline () in 
  let _ = print_newline () in 
  let _ = Debug.tag "+ Story compression" in 
  let _ = Debug.tag "\t - blackboard generation" in 
  let _ = 
    if debug_mode 
    then 
      Printf.fprintf stderr "\nRefining event\n" 
  in 
  let refined_event_list = List.map (Kappa_instantiation.Cflow_linker.refine_step (Kappa_instantiation.Cflow_linker.import_env env)) (List.rev step_list) in 
  let _ = 
    if debug_mode
    then 
      List.iter (Kappa_instantiation.Cflow_linker.print_refined_step stderr (Kappa_instantiation.Cflow_linker.import_env env)) refined_event_list  
  in 
  let parameter = () in 
  let handler = () in 
  let error = [] in 
  let _ = 
    if debug_mode 
    then 
      Printf.fprintf stderr "\nDealing with initial states\n" 
  in 
  let error,blackboard = S.PH.B.PB.init parameter handler error   in
  let _ = 
    if debug_mode 
    then 
      Printf.fprintf stderr "\nDealing with steps\n" 
  in 
  let error,blackboard = 
    List.fold_left 
      (fun (error,blackboard) refined_event  -> 
        S.PH.B.PB.add_step parameter handler error refined_event blackboard)
      (error,blackboard)
      refined_event_list
  in 
  let error,preblackboard = 
    S.PH.B.PB.finalize parameter handler error blackboard in 
  let _ = 
    if debug_mode 
    then 
      Printf.fprintf stderr "\nPretty printing the grid\n"
  in 
  let error = 
    if debug_mode
    then 
      S.PH.B.PB.print_preblackboard parameter handler error stderr preblackboard 
    else 
      error 
  in
  let error,blackboard = S.PH.B.import parameter handler error preblackboard in 
  let _ = 
    if debug_mode 
    then 
      Printf.fprintf stderr "\nPretty printing the grid\n"
  in 
  let error = 
    if debug_mode
    then 
      S.PH.B.print_blackboard parameter handler error stderr blackboard 
    else 
      error 
  in  
  let error,list = S.PH.forced_events parameter handler error blackboard in 
  let n_stories = List.length list in 
  let _ = Debug.tag ("\t - story computation ("^(string_of_int n_stories)^")") in 
  let tick = 
    if n_stories > 0 
    then Mods.tick_stories n_stories (false,0,0) 
    else (false,0,0)
  in 
  let error,_,_ = 
    List.fold_left 
      (fun (error,counter,tick) (list_order,list_eid) -> 
        let _ = 
          if debug_mode 
          then 
            Printf.fprintf stderr "COMPRESS %i" (List.length list_eid) 
        in 
        let error,blackboard,output,result_wo_compression = 
          S.compress parameter handler error blackboard  list_order list_eid 
        in 
        let error = 
          if debug_mode 
          then 
            let _ = Printf.fprintf stderr "*****\nRESULT:\n*****\n" in 
            let _ =
              if S.PH.B.is_failed output 
              then 
                let _ = Printf.fprintf stderr "Fail_to_compress" in  error
              else 
                let _ = Printf.fprintf stderr "Succeed_to_compress" in 
              error
            in 
            error 
          else 
            error
        in 
        let error = 
          if S.PH.B.is_failed output 
          then error
          else 
            let error,list = S.PH.B.translate_blackboard parameter handler error blackboard in 
            let grid = S.PH.B.PB.K.build_grid list false env in
						let filename_comp = (Filename.chop_suffix !Parameter.cflowFileName ".dot") ^"_"^(string_of_int counter)^"weak_comp"^".dot"
						and filename =  (Filename.chop_suffix !Parameter.cflowFileName ".dot")^"_"^(string_of_int counter)^".dot"
						in
            let _ = Causal.dot_of_grid filename_comp grid state env in 
            let grid = S.PH.B.PB.K.build_grid result_wo_compression true env in 
            let _ = Causal.dot_of_grid filename grid state env in 
            let error,blackboard = S.PH.B.reset_init parameter handler error blackboard in 
            error
        in 
        let tick = Mods.tick_stories n_stories tick in 
        error,counter+1,tick)
      (error,1,tick) list 
  in 
  let _ = 
    List.iter 
      (S.PH.B.PB.H.dump_error parameter handler error stderr)
      error
  in 
(*  let error,blackboard,output = *)
  () 
