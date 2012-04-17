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
  * Last modification: 16/04/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module S = Generic_branch_and_cut_solver.Solver 


let log_step = true
let debug_mode = false

let th_of_int n = 
  match n mod 10  
  with 
    | 1 -> (string_of_int n)^"st"
    | 2 -> (string_of_int n)^"nd"
    | 3 -> (string_of_int n)^"rd"
    | _ -> (string_of_int n)^"th"

let weak_compression env state log_info step_list =  
  let parameter = S.PH.B.PB.Po.K.H.build_parameter () in 
  let mode = parameter.S.PH.B.PB.Po.K.H.compression_mode in 
  let causal_trace_on = Parameter.get_causal_trace mode in 
  let weak_compression_on = Parameter.get_weak_compression mode in 
  let strong_compression_on = Parameter.get_strong_compression mode in 
  let handler = 
    {
      S.PH.B.PB.Po.K.H.env = env ;
      S.PH.B.PB.Po.K.H.state = state 
    }
  in 
  let _ = print_newline () in 
  let _ = print_newline () in 
  if (not causal_trace_on)
    && (not weak_compression_on)  
    && (not strong_compression_on)
  then 
    ()
  else
    begin 
      if S.PH.B.PB.Po.K.no_obs_found step_list 
      then 
        let _ = Debug.tag "+ No story found" in () 
      else 
        begin 
          let _ = 
            if (weak_compression_on or strong_compression_on)
            then 
              let _ = Debug.tag "+ Story compression" in ()
            else 
              let _ = Debug.tag "+ Causal traces" in ()
          in 
          let _ = Debug.tag "\t - blackboard generation" in 
          let _ = 
            if log_step
            then 
              Debug.tag "\t\t * refining events" 
          in 
          let refined_event_list = 
            List.rev_map (S.PH.B.PB.Po.K.refine_step handler) step_list in       
          let _ = 
            if debug_mode
            then 
              let _ = 
                List.iter 
                  (S.PH.B.PB.Po.K.print_refined_step parameter handler) 
                  refined_event_list  
              in flush parameter.S.PH.B.PB.Po.K.H.out_channel
          in 
          let refined_event_list_cut,int = 
            if Parameter.do_global_cut 
            then 
              begin 
                let _ = 
                  if log_step
                  then 
                    Debug.tag "\t\t * cutting concurrent events" 
                in 
                let refined_event_list_cut,int = S.PH.B.PB.Po.cut refined_event_list  in 
                let _ = 
                  if debug_mode
                  then 
                    let _ = 
                      List.iter 
                        (S.PH.B.PB.Po.K.print_refined_step parameter handler) 
                        refined_event_list_cut  
                    in flush parameter.S.PH.B.PB.Po.K.H.out_channel_err
                in 
                refined_event_list_cut,int 
              end
            else 
              refined_event_list,0 
          in 
          let error = [] in 
          let error,blackboard = S.PH.B.PB.init parameter handler error log_info  in
          let _ = 
            if log_step
            then 
              Debug.tag "\t\t * dealing with steps" 
          in 
          let error,blackboard = 
            List.fold_left 
              (fun (error,blackboard) refined_event  -> 
                S.PH.B.PB.add_step parameter handler error refined_event blackboard)
              (error,blackboard)
              refined_event_list_cut
          in 
          let error,preblackboard = 
            S.PH.B.PB.finalize parameter handler error blackboard 
          in 
          let _ = 
            if log_step 
            then 
              Debug.tag "\t\t * converting into the blackboard" 
          in 
          let error = 
            if debug_mode
            then 
              let _ = 
                S.PH.B.PB.print_preblackboard parameter handler error preblackboard 
              in 
              let _ = flush parameter.S.PH.B.PB.Po.K.H.out_channel_err in 
              error 
            else 
              error 
          in
          let error,blackboard = S.PH.B.import parameter handler error preblackboard in 
          let blackboard = S.PH.B.set_profiling_info (S.PH.B.PB.Po.K.P.set_global_cut int) blackboard in 
          let _ = 
            if log_step  
            then 
              Debug.tag "\t\t * pretty printing the grid" 
          in 
          let error = 
            if debug_mode
            then 
              S.PH.B.print_blackboard parameter handler error blackboard 
            else 
              error 
          in  
          let error,list = S.PH.forced_events parameter handler error blackboard in 
          let n_stories = List.length list in 
          let _ = 
            if strong_compression_on or weak_compression_on 
            then 
              Debug.tag ("\t - story computation ("^(string_of_int n_stories)^")") 
            else 
              Debug.tag ("\t - cut ("^(string_of_int n_stories)^")")
          in 
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
                    Debug.tag ("\t\t * compress "^(string_of_int (List.length list_eid)))
                in 
                let error,blackboard,output,result_wo_compression  = 
                  S.compress parameter handler error blackboard  list_order list_eid 
                in 
                let error = 
                  if debug_mode
                  then 
                    let _ =  Debug.tag "\t\t * result"  in 
                    let _ =
                      if S.PH.B.is_failed output 
                      then 
                        let _ = Printf.fprintf parameter.S.PH.B.PB.Po.K.H.out_channel_err "Fail_to_compress" in  error
                      else 
                        let _ = Printf.fprintf parameter.S.PH.B.PB.Po.K.H.out_channel_err "Succeed_to_compress" in 
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
                    let _ = 
                      if weak_compression_on
                      then 
                        let error,list = S.PH.B.translate_blackboard parameter handler error blackboard in 
                        let grid = S.PH.B.PB.Po.K.build_grid list false handler in
		        let filename_comp = (Filename.chop_suffix !Parameter.cflowFileName ".dot") ^"_"^(string_of_int counter)^"weak_comp"^".dot" in 
                        let _ = Causal.dot_of_grid (fun log -> 
                          let _ = S.PH.B.print_complete_log log blackboard in 
                          let _ = Printf.fprintf parameter.S.PH.B.PB.Po.K.H.out_channel_profiling "\nCompression of the %s story:\n\n" (th_of_int counter) in 
                          let _ = S.PH.B.print_complete_log parameter.S.PH.B.PB.Po.K.H.out_channel_profiling blackboard in ()) filename_comp grid state env in
                        () 
                    in 
                    let _ = 
                      match result_wo_compression 
                      with 
                        | None -> () 
                        | Some result_wo_compression -> 
                          let filename =  (Filename.chop_suffix !Parameter.cflowFileName ".dot")^"_"^(string_of_int counter)^".dot"
		          in
                          let grid = S.PH.B.PB.Po.K.build_grid result_wo_compression true handler in 
                          let _ = Causal.dot_of_grid (fun _ -> ()) filename grid state env in
                          ()
                    in 
                    let error,blackboard = S.PH.B.reset_init parameter handler error blackboard in 
                    error
                in 
                let tick = Mods.tick_stories n_stories tick in 
                error,counter+1,tick)
              (error,1,tick) list 
          in 
          let _ = close_out parameter.S.PH.B.PB.Po.K.H.out_channel_profiling in 
          let _ = 
            List.iter 
              (S.PH.B.PB.Po.K.H.dump_error parameter handler error)
              error
          in 
          ()
        end 
    end 
