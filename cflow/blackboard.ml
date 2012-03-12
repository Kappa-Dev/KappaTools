(**
  * blackboard.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 06/09/2011
  * Last modification: 12/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type Blackboard = 
sig 
  module PB:Blackboard_generation.PreBlackboard 

  (** blackboard matrix *)
  type case_info
  type case_value 
  type case_address  
 
  (** propagation request *)

  type instruction 

  (** blackboard*)

  type blackboard      (*blackboard, once finalized*)
 (* type stack *)
  type assign_result

  val set: (case_address -> case_value -> blackboard  -> PB.H.error_channel * blackboard) PB.H.with_handler 
  val get: (case_address -> blackboard -> PB.H.error_channel * case_value) PB.H.with_handler 
  val overwrite: (case_address -> case_value -> blackboard -> PB.H.error_channel * blackboard) PB.H.with_handler  
  val refine: (case_address -> case_value -> blackboard -> PB.H.error_channel * blackboard * assign_result) PB.H.with_handler  
    

(*  val strictly_more_refined: case_value -> case_value -> bool*)


(*  val record_modif: ((case_address * case_value option) -> stack -> PB.H.error_channel * stack) PB.H.with_handler*)

  val branch: (blackboard -> PB.H.error_channel *blackboard) PB.H.with_handler 
  val reset: (blackboard -> PB.H.error_channel * blackboard ) PB.H.with_handler 

  (** initialisation*)
  val import: (PB.pre_blackboard -> PB.H.error_channel * blackboard) PB.H.with_handler 


  (** heuristics *)
   val next_choice: (blackboard -> PB.H.error_channel * instruction list) PB.H.with_handler 
  val propagation_heuristic: (blackboard -> instruction  -> instruction list -> PB.H.error_channel * instruction list) PB.H.with_handler 
  val apply_instruction: (blackboard -> instruction -> instruction list -> (PB.H.error_channel * (case_address*case_value) list * instruction list)) PB.H.with_handler 


  (** output result*)
  type result 

  (** iteration*)
 val is_maximal_solution: (blackboard -> PB.H.error_channel * bool) PB.H.with_handler 

  (** exporting result*)
  val translate_blackboard: (blackboard -> PB.H.error_channel * result) PB.H.with_handler 

  (**pretty printing*)
  val print_blackboard:(blackboard -> PB.H.error_channel) PB.H.with_handler 
end

module Blackboard = 
  (struct 
    module PB = Blackboard_generation.Preblackboard 
     (** blackboard matrix*) 

    type assign_result = Fail | Success | Ignore 
        
     type step_id = int 
     type step_short_id = int 

     type case_address = 
	     {
	       column_id:PB.predicate_id; 
	       row_id: step_id;
	     }
	       
     type case_value = PB.predicate_value 

     let strictly_more_refined = PB.strictly_more_refined 

     let more_refined x y = x=y or strictly_more_refined x y 

     let conj parameter handler error x y = 
       if x=y or strictly_more_refined x y
       then error,x 
       else if strictly_more_refined y x  
       then error,y
       else 
         let error_list,error = 
              PB.H.create_error parameter handler error (Some "blackbord.ml") None (Some "conj") (Some "174") (Some "Try to compare incompare site states") Exit
         in 
         PB.H.raise_error 
           parameter 
           handler 
           error_list 
           stderr 
           error 
           x 
         
     type case_info_static  = 
	 {
	   row_short_id: step_short_id;
	   state_before: case_value;
           state_after: case_value
	 }

     type case_info_dynamic = 
         {
           pointer_previous: step_short_id;
           pointer_next: step_short_id;
         }
          
     type case_info = 
         {
           static: case_info_static;
           dynamic: case_info_dynamic
         }
       
     let null_pointer = -1 

     let dummy_case_info_static = 
       {
	 row_short_id = null_pointer ;
	 state_before = PB.undefined ;
	 state_after = PB.undefined
       }
     
     let dummy_case_info_dynamic = 
       { 
         pointer_previous = null_pointer;
         pointer_next = null_pointer
       }

     let dummy_case_info = 
       {
         static = dummy_case_info_static ;
         dynamic = dummy_case_info_dynamic
       }

     (** propagation request *)

     type instruction = unit


     (** blackboard*)
	 
     type blackboard = PB.pre_blackboard 

     let set parameter handler error case_address case_value blackboard = error,blackboard 
 
     let get parameter handler error case_address blackboard = error,PB.unknown  

  (** stack *)
     type stack = (case_address*case_value) list 
   
     let record_modif parameter handler error case_address case_value blackboard = error,blackboard
       
     let refine parameter handler error case_address case_value blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       if case_value = old 
       then 
         error,blackboard,Ignore
       else 
       if strictly_more_refined case_value old 
       then 
         let error,blackboard = set parameter handler error case_address case_value blackboard in 
         let error,blackboard = record_modif parameter handler error case_address old blackboard in 
         error,blackboard,Success
       else 
         error,blackboard,Fail 

     let overwrite parameter handler error case_address case_value blackboard = 
       let error,old = get parameter handler error case_address blackboard in 
       if case_value = old 
       then 
         error,blackboard
       else
         let error,blackboard = set parameter handler error case_address case_value blackboard in 
         let error,blackboard = record_modif parameter handler error case_address old blackboard in 
         error,blackboard 
         
     let branch parameter handler error blackboard = error,blackboard
       
     let reset parameter handler error blackboard = error,blackboard 
 
     let import parameter handler error pre_blackboard = error,pre_blackboard 

  (** heuristics *)
       
     let next_choice parameter handler error blackboard = error,[]
       
     let propagation_heuristic parameter handler error blackboard instruction instruction_list = error,instruction_list 
       
     let apply_instruction parameter handler error blackboard instruction instruction_list = error,[],instruction_list 
 
  (** output result*)
     type result = ()
         
  (** iteration*)
     let is_maximal_solution parameter handler error blackboard = error,false 
 
  (** exporting result*)
       
   let translate_blackboard parameter handler error blackboard = error,()
       
  (**pretty printing*)
     let print_blackboard paramter handler error blackboard = error

   end:Blackboard)


      

