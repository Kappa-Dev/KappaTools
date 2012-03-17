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
  * Last modification: 23/02/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module S = Generic_branch_and_cut_solver.Solver 

let debug_mode = true 

let weak_compression env state step_list =  
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
  let _ = Printf.fprintf stderr "%i" (List.length list) in 
  let error = 
    List.fold_left 
      (fun error list -> 
        let _ = Printf.fprintf stderr "COMPRESS" in 
        let error,blackboard,output = 
          S.compress parameter handler error blackboard  list 
        in 
        let error = 
          if debug_mode 
          then 
            let _ = Printf.fprintf stderr "*****\nRESULT:\n*****\n" in 
            let _ =
              if S.PH.B.is_failed output 
              then 
                let _ = Printf.fprintf stderr "Fail" in error 
              else 
                let error,list = S.PH.B.translate_blackboard parameter handler error blackboard in 
                let grid = S.PH.B.PB.K.build_grid list env in 
                let _ = Causal.dump grid state env in 
                let _ = Causal.dot_of_grid "essai" grid state env in 
               (* let _ = S.PH.B.print_blackboard parameter handler error stderr blackboard in 
                let _ = Printf.fprintf stderr "*********************\n" in 
               *) let error,blackboard = S.PH.B.reset_init parameter handler error blackboard in 
                error
            in error 
          else 
            error
        in error) 
      error list 
  in 
  let _ = 
    List.iter 
      (S.PH.B.PB.H.dump_error parameter handler error stderr)
      error
  in 
(*  let error,blackboard,output = *)
  () 
