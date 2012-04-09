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
  * Last modification: 09/04/2012
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
    val dec_removed_events: log_info -> log_info 
    val dec_selected_events: log_info -> log_info

    val set_cut_events: log_info -> int -> log_info 
    val set_n_kasim_events: log_info -> int -> log_info 
    val set_n_init_events: log_info -> int -> log_info 
    val set_n_side_events: log_info -> int -> log_info  
    val inc_stack: log_info -> log_info 
    val inc_branch: log_info -> log_info 
    val inc_cut: log_info -> log_info 
    val reset_log: log_info -> log_info 
    val dump_complete_log: out_channel -> log_info -> unit 
    val dump_short_log: out_channel -> log_info -> unit 
    val add_propagation_case_up: int -> log_info -> log_info 
    val add_propagation_case_down: int -> log_info -> log_info 
    val add_look_up_case: int -> log_info -> log_info
    val add_look_down_case: int -> log_info -> log_info

    val set_time: log_info -> log_info 
    val ellapsed_global_time: log_info -> float
    val ellapsed_time: log_info -> float
    val init_log_info: unit -> log_info 


  end

module StoryStats =
     (struct 
       type log_info = 
           {
             global_start_time:float;
             story_start_time:float;
             propagation: int array;
             branch: int;
             cut: int; 
             current_branch: int; 
             kasim_events: int;
             init_events: int;
             fictitious_events: int;
             selected_events: int;
             remaining_events: int;
             removed_events: int;
             cut_events: int;
             stack: int list;
             stack_size: int;
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

       let init_log_info () = 
         let time = Sys.time () in
         { 
           global_start_time = time ;
           story_start_time = time ;
           propagation = Array.make propagation_cases 0 ;
           branch = 0 ;
           cut = 0 ;
           current_branch = 0 ; 
           kasim_events = 0 ;
           init_events = 0 ;
           fictitious_events = 0 ;
           selected_events = 0 ;
           remaining_events = 0 ;
           removed_events = 0 ;
           cut_events = 0 ;
           stack= [0] ;
           stack_size = 0 ;
         }

       let dump_short_log log log_info = 
         let _ = Printf.fprintf log "Remaining events: %i ; Stack size: %i ; Total branch: %i ; Total cut: %i ; Current depth: %i \n " log_info.remaining_events log_info.stack_size log_info.branch log_info.cut log_info.current_branch in 
         flush log

           
       let reset_log log = 
         let time = Sys.time () in 
         let t = log.propagation in 
         let _ = Array.fill t 0 (Array.length t) 0 in 
         {log 
           with 
             story_start_time = time ; 
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
         { log with story_start_time = Sys.time ()}

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
                 stack = q ;
                 cut = log.cut + 1;
                 stack_size = log.stack_size - t ;
                 current_branch = log.current_branch - 1 ; 
            }

       let inc_stack log =
         match 
           log.stack 
         with 
           | [] -> log
           | t::q -> 
             {
               log 
              with 
                stack = (t+1)::q ;
                stack_size = log.stack_size + 1 ;
             }
         
       let inc_branch log = 
         {
           log 
          with 
            stack = 0::log.stack ;
            branch = log.branch + 1
         }
           
       let set_n_kasim_events log i = 
         { log with kasim_events = i}

       let set_n_side_events log i = 
         { log with fictitious_events = i}

       let set_n_init_events log i = 
         { log with init_events = i}

       let set_cut_events log i = 
         { log with cut_events = i}

       let dec_selected_events log = 
         { log with selected_events = log.selected_events - 1}

       let dec_removed_events log = 
         { log with removed_events = log.removed_events - 1}

       let inc_selected_events log = 
         { log with selected_events = log.selected_events + 1}

       let inc_removed_events log = 
         { log with removed_events = log.removed_events + 1}

           
       let dump_complete_log log log_info = 
         let _ = Printf.fprintf log "Story profiling\n" in 
         let _ = Printf.fprintf log "Ellapsed_time:     %f\n" (ellapsed_time log_info) in 
         let _ = Printf.fprintf log "KaSim events:      %i\n" log_info.kasim_events in 
         let _ = Printf.fprintf log "Init events:       %i\n" log_info.init_events in 
         let _ = Printf.fprintf log "Fictitious events: %i\n" log_info.fictitious_events in 
         let _ = Printf.fprintf log "Selected events:   %i\n" log_info.selected_events in 
         let _ = Printf.fprintf log "***\nPropagation Hits:\n" in 
         let rec aux k = 
           if k>=propagation_cases 
           then () 
           else 
             let _ = 
               Printf.fprintf log "   %s%i\n" propagation_labels.(k) log_info.propagation.(k)
             in aux (k+1)
         in 
         let _ = aux 1 in 
         flush log


      end:StoryStats)
           
