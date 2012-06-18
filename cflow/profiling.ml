(**
  * profiling.ml
  *
  * Generate profiling information for stories in KaSim
  * 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 09/04/2012
  * Last modification: 15/06/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type StoryStats = 
  sig 
    type log_info 

    val inc_removed_events: log_info -> log_info 
    val inc_selected_events: log_info -> log_info 

    val inc_cut_events: log_info -> log_info 
    val inc_k_cut_events: int -> log_info -> log_info 
    val reset_cut_events: log_info -> log_info 
    val inc_n_kasim_events: log_info -> log_info 
    val inc_n_init_events: log_info -> log_info 
    val inc_n_side_events: log_info -> log_info 
    val inc_n_obs_events: log_info -> log_info 
    val inc_branch: log_info -> log_info 
    val inc_cut: log_info -> log_info 
    val reset_log: log_info -> log_info 
    val dump_complete_log: out_channel -> log_info -> unit 
    val dump_short_log: out_channel -> log_info -> unit 
    val add_propagation_case_up: int -> log_info -> log_info 
    val add_propagation_case_down: int -> log_info -> log_info 
    val add_look_up_case: int -> log_info -> log_info
    val add_look_down_case: int -> log_info -> log_info

    val copy: log_info -> log_info 

    val set_time: log_info -> log_info 
    val set_step_time: log_info -> log_info
    val set_global_cut: int -> log_info -> log_info 
    val set_pseudo_inv: int -> log_info -> log_info 
    val set_start_compression: log_info -> log_info 
    val set_grid_generation: log_info -> log_info 
    val set_canonicalisation: log_info -> log_info 
    val set_concurrent_event_detection_time : log_info -> log_info 
    val set_concurrent_event_deletion_time : log_info -> log_info 
    val set_story_research_time: log_info -> log_info 
    val ellapsed_global_time: log_info -> float
    val ellapsed_time: log_info -> float
    val init_log_info: unit -> log_info 
      
    val tick: log_info -> bool * log_info 

  end

module StoryStats =
     (struct 
       type stack_head = 
           { 
             current_branch : int ;
             selected_events: int ;
             remaining_events: int ;
             removed_events: int ;
             stack_size: int ;
           }

       type log_info = 
           {
             last_tick:float;
             global_start_time:float;
             story_start_time:float;
             step_start_time: float;
             grid_time: float;
             concurrent_event_detection_time: float; 
             concurrent_event_deletion_time: float;
             story_research_time:float;
             canonicalization_time: float;
             propagation: int array;
             branch: int;
             global_cut: int; 
             pseudo_inv_cut: int;
             cut: int; 
             kasim_events: int;
             init_events: int;
             obs_events: int;
             fictitious_events: int;
             cut_events: int;
             stack: stack_head list ;
             current_stack: stack_head ;
           }
             

     
       let propagation_labels = 
         [|
           "None" ; 
           "Up:        case 1 " ; 
           "Up:        case 2 " ; 
           "Up:        case 3 " ;
           "Up:        case 4 " ;
           "Up:        case 5 " ;
           "Up:        case 6 " ; 
           "Up:        case 7 " ;
           "Up:        case 8 " ;
           "Up:        case 9 " ;
           "Up:        case 10" ; 
           "Up:        case 11" ;
           "Up:        case 12" ;
           "Up:        case 13" ;
           "Up:        case 14" ; 
           "Up:        case 15" ;
           "Up:        case 16" ;
           "Down:      case 1 " ;(*17*)
           "Down:      case 2 " ;(*18*) 
           "Down:      case 3 " ;(*19*)
           "Down:      case 4 " ;(*20*)
           "Down:      case 5 " ;(*21*)
           "Down:      case 6 " ;(*22*) 
           "Down:      case 7 " ;(*23*)
           "Down:      case 8 " ;(*24*)
           "Down:      case 9 " ;(*25*)
           "Down:      case 10" ;(*26*)
           "Down:      case 11" ;(*27*)
           "Down:      case 12" ;(*28*)
           "Down:      case 13" ;(*29*)
           "Down:      case 14" ;(*30*) 
           "Down:      case 15" ;(*31*)
           "Down:      case 16" ;(*32*)
           "Look_up:   case  1" ;(*33*)
           "Look_up:   case  2" ;(*34*)
           "Look_up:   case  3" ;(*35*)
           "Look_up:   case  4" ;(*36*)
           "Look_down: case  1" ;(*37*)
           "Look_down: case  2" ;(*38*)
           "Look_down: case  3" ;(*39*)
           "Look_down: case  4" ;(*40*)
         |]
        
       let propagation_cases = Array.length propagation_labels 
         
       let copy log_info = 
         {log_info with propagation = Array.copy log_info.propagation}

       let init_log_info () = 
         let time = Sys.time () in
         { 
           global_start_time = time ;
           story_start_time = time ;
           step_start_time = time ; 
           grid_time = 0.;
           concurrent_event_detection_time = 0.; 
           concurrent_event_deletion_time = 0.;
           story_research_time = 0.;
           canonicalization_time = 0. ; 
           last_tick = time ;
           propagation = Array.make propagation_cases 0 ;
           branch = 0 ;
           cut = 0 ;
           global_cut = 0 ; 
           pseudo_inv_cut = 0 ;
           kasim_events = 0 ;
           init_events = 0 ;
           obs_events = 0 ;
           fictitious_events = 0 ;
           current_stack = 
             {
               current_branch = 0 ;
               selected_events = 0 ;
               remaining_events = 0 ;
               removed_events = 0 ;
               stack_size = 0 
             } ;
           stack = [] ;
           cut_events = 0 ;
         }

       let dump_short_log log log_info = 
         let _ = Printf.fprintf log "Remaining events: %i ; Stack size: %i ; Total branch: %i ; Total cut: %i ; Current depth: %i \n " log_info.current_stack.remaining_events log_info.current_stack.stack_size log_info.branch log_info.cut log_info.current_stack.current_branch in 
         flush log

           
       let reset_log log = 
         let time = Sys.time () in 
         let t = log.propagation in 
         let _ = Array.fill t 0 (Array.length t) 0 in 
         {log 
           with 
             cut_events = 0;
             story_start_time = time ; 
             step_start_time = time ;
             grid_time = 0.;
             concurrent_event_detection_time = 0. ;
             concurrent_event_deletion_time = 0. ;
             story_research_time = 0.;
             canonicalization_time = 0. ; 
         }

       let propagate_up i = i 
       let propagate_down i = i+16 
       let look_up i = i+32
       let look_down i = i+36 

       let ellapsed_time log = 
         let time = Sys.time () in 
         time -. log.story_start_time 

       let ellapsed_global_time log = 
         let time = Sys.time () in 
         time -. log.global_start_time 

       let set_time log = 
         { log with story_start_time = Sys.time () ; step_start_time = Sys.time ()}
       let set_step_time log = 
         { log with step_start_time = Sys.time ()}


       let set_start_compression = set_time  

       let set_story_research_time log = 
         let t = Sys.time () in 
         let st = log.step_start_time in 
         { log 
           with 
             story_research_time = t -. st ; 
             step_start_time = t }
           
       let set_concurrent_event_detection_time log = 
         let t = Sys.time () in 
         let st = log.step_start_time in 
         {log 
          with 
            concurrent_event_detection_time = t -. st ; 
            step_start_time = t }

       let set_concurrent_event_deletion_time log = 
         let t = Sys.time () in 
         let st = log.step_start_time in 
         {log 
          with 
            concurrent_event_deletion_time = t -. st ; 
            step_start_time = t 
         }


       let set_grid_generation log = 
         let t = Sys.time () in 
         let st = log.step_start_time in 
         { log with grid_time = t -. st ;
           step_start_time = t}

       let set_canonicalisation log =
         let t = Sys.time () in 
         let st = log.step_start_time in 
         { log with canonicalization_time = t -. st ;
                    step_start_time = t}
           
           
           
       let add_case i log = 
         let t = log.propagation in 
         let _ = t.(i)<-t.(i)+1 in 
         log 

       let add_look_down_case i = add_case (look_down i) 
       let add_look_up_case i = add_case (look_up i)
       let add_propagation_case_down i = add_case (propagate_down i)
       let add_propagation_case_up i = add_case (propagate_up i)

       let inc_cut log = 
         match 
           log.stack 
         with 
           | [] -> log 
           | t::q -> 
             { 
               log 
               with 
                 current_stack = t ;
                 stack = q ;
                 cut = log.cut + 1;
             }

       let inc_branch log = 
         {
           log 
          with 
            stack = log.current_stack::log.stack ;
            branch = log.branch + 1;
            current_stack = 
             {log.current_stack 
              with current_branch = log.current_stack.current_branch + 1}}
           
       let inc_n_kasim_events log = 
         let old = log.kasim_events in 
         { log with kasim_events = old + 1}

       let inc_n_obs_events log = 
         let old = log.obs_events in 
         { log with obs_events = old + 1}
           
       let inc_n_side_events log = 
         let old = log.fictitious_events in 
         { log with fictitious_events = old+1}

       let inc_n_init_events log = 
         let old = log.init_events in 
         { log with init_events = old + 1}

       let inc_cut_events log = 
         { log with cut_events = log.cut_events + 1}

       let inc_k_cut_events k log = 
         { log with cut_events = log.cut_events + k}

       let reset_cut_events log = 
         { log with cut_events = 0}

       let inc_selected_events log = 
          { log 
           with current_stack = 
             {log.current_stack 
              with selected_events = 
                 log.current_stack.selected_events + 1
             }
         }

       let inc_removed_events log = 
         { log 
           with current_stack = 
             {log.current_stack 
              with removed_events = 
                 log.current_stack.removed_events + 1
             }
         }

           
       let dump_complete_log log log_info = 
         let _ = Printf.fprintf log "/*\n" in 
         let _ = Printf.fprintf log "Story profiling\n" in 
         let _ = Printf.fprintf log "Ellapsed_time:                  %f\n" (ellapsed_time log_info) in 
         let _ = Printf.fprintf log "Concurrent event research time: %f\n" (log_info.concurrent_event_detection_time) in 
         let _ = Printf.fprintf log "Concurrent event deletion time: %f\n" (log_info.concurrent_event_deletion_time) in 
         let _ = Printf.fprintf log "Story research time:            %f\n" (log_info.story_research_time) in 
         let _ = Printf.fprintf log "Grid generation time:           %f\n" (log_info.grid_time) in 
         let _ = Printf.fprintf log "Canonicalization time:          %f\n" (log_info.canonicalization_time) in 
         let _ = Printf.fprintf log "KaSim events:                   %i\n" log_info.kasim_events in 
         let _ = Printf.fprintf log "Init events:                    %i\n" log_info.init_events in 
         let _ = Printf.fprintf log "Obs events:                     %i\n" log_info.obs_events in 
         let _ = Printf.fprintf log "Fictitious events:              %i\n" log_info.fictitious_events in 
         let _ = Printf.fprintf log "Cut events (globally):          %i\n" log_info.global_cut in 
         let _ = Printf.fprintf log "Pseudo-inverse events:          %i\n" log_info.pseudo_inv_cut in 
         let _ = Printf.fprintf log "Cut events (for this story):    %i\n" log_info.cut_events in 
         let _ = Printf.fprintf log "Selected events:                %i\n" log_info.current_stack.selected_events in 
         let _ = Printf.fprintf log "Removed events:                 %i\n" log_info.current_stack.removed_events in 
         let _ = Printf.fprintf log "Remaining events:               %i\n" (log_info.kasim_events + log_info.obs_events + log_info.init_events +log_info.fictitious_events - log_info.cut_events - log_info.current_stack.selected_events - log_info.current_stack.removed_events - log_info.global_cut - log_info.pseudo_inv_cut) in 
         let _ = Printf.fprintf log "Exploration depth:              %i\n" log_info.current_stack.current_branch in 
         let _ = Printf.fprintf log "Exploration cuts:               %i\n" log_info.cut in 
         let _ = Printf.fprintf log "***\nPropagation Hits:\n" in 
         let rec aux k = 
           if k>=propagation_cases 
           then () 
           else 
             let _ = 
               Printf.fprintf log                           "        %s %i\n" propagation_labels.(k) log_info.propagation.(k)
             in aux (k+1)
         in 
         let _ = aux 1 in 
         let _ = Printf.fprintf log "*/ \n" in 
         flush log

       let tick log_info = 
         let time = Sys.time () in 
         if time-.log_info.last_tick > 600.
         then 
           true,{log_info with last_tick = time}
         else
           false,log_info

       let set_global_cut n log_info = 
         {log_info with global_cut = n}

       let set_pseudo_inv n log_info = 
         {log_info with pseudo_inv_cut = n}
           
      end:StoryStats)
           
