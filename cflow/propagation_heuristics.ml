(**
  * propagation_heuristic.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 05/09/2011
  * Last modification: 13/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type Blackboard_with_heuristic = 
  sig
    module B:Blackboard.Blackboard 

    type instruction 
      
     (** heuristics *)
    val next_choice: (B.blackboard -> B.PB.H.error_channel * instruction list) B.PB.H.with_handler 
    val propagation_heuristic: (B.blackboard -> instruction  -> instruction list -> B.PB.H.error_channel * instruction list * instruction list) B.PB.H.with_handler 
    val apply_instruction: (B.blackboard -> instruction -> instruction list -> (B.PB.H.error_channel * (B.case_address*B.case_value) list * instruction list)) B.PB.H.with_handler 
  end 

module Propagation_heuristic = 
  (struct 

    module B=Blackboard.Blackboard 

    type instruction = 
      | Keep_event of int (*B.PB.step_id*)
(*      | Discard_event of B.PB.step_id 
      | Propagate_up of case_address * case_value 
      | Propagate_down of case_address * case_value 
      | Decrease_counter of case_address 
*)        
    let next_choice parameter handler error blackboard = error,[]
    let propagation_heuristic parameter handler error blackboard instruction list = 
      error,[],list
    let apply_instruction parameter handler error blackboard instruction instruction_list = 
      error,[],instruction_list 
        

  end:Blackboard_with_heuristic)
