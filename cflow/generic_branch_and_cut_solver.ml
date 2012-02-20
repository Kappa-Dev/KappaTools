(**
  * generic_branch_and_cup_solver.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 29/08/2011
  * Last modification: 19/10/2011
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type assign_result = Fail | Success | Ignored  




module Solver = 
  functor (B:Blackboard.Blackboard) -> 
    struct 
      let combine_output o1 o2 = 
	match o2
	with 
	  | Ignored -> o1
	  | _ -> o2  

      let reset parameter handler error blackboard stack = 
	let error,stack,list = B.reset parameter handler error stack in 
	let error,blackboard = 
	  List.fold_left 
	    (fun (error,blackboard)  assignment -> B.set parameter handler error assignment blackboard)
	    (error,blackboard) 
	    list
	in
	 error,blackboard,stack 

      let rec deal_with_assignments parameter handler error list blackboard stack output = 
	match list
	with 
	  | [] -> error,blackboard,stack,output
	  | (address,case_value)::tail -> 
	      let error,former_value = B.get parameter handler error address blackboard in 
		match former_value 
		with 
		  | None ->
		      begin
			let error,blackboard = B.set parameter handler error (address,Some case_value) blackboard in 
			let error,stack = B.record_modif parameter handler error (address,None) stack in 
			  deal_with_assignments parameter handler error tail blackboard stack Success 
		      end 
		  | Some x when x=case_value -> 
		      begin
			deal_with_assignments parameter handler error tail blackboard stack output 
		      end
		  | _ -> 
		      begin 
			error,blackboard,stack,Fail 
		      end 

      let rec propagate parameter handler error list blackboard stack = 
	match list 
	with 
	  | []  -> error,blackboard,true
	  | head::tail -> 
	      let error,assignment_list,list = B.apply_instruction parameter handler error blackboard head tail in 
	      let error,blackboard,stack,output = deal_with_assignments parameter handler error assignment_list blackboard stack Ignored in 
		match output 
		with 
		  | Fail -> error,blackboard,false
		  | Success -> 
		      let error,list = B.propagation_heuristic parameter handler error blackboard head tail in 			
			propagate parameter handler error list blackboard stack 
		  | Ignored -> 
		      propagate parameter handler error tail blackboard stack 
		  
      let rec branch_over_assumption_list parameter handler error list blackboard stack = 
	match list 
	with 
	  | [] -> error,blackboard,false 
	  | head::tail -> 
	      begin
		let fail error blackboard stack tail = 
		  let error,blackboard,stack = reset parameter handler error blackboard stack in 
		    branch_over_assumption_list parameter handler error tail blackboard stack 
		in 
		let error,blackboard,stack = B.branch parameter handler error blackboard stack in 
		let error,blackboard,bool = propagate parameter handler error [head] blackboard stack in
		  if bool 
		  then 
		    let error,blackboard,bool = iter parameter handler error blackboard stack in 
		      if bool 
		      then error,blackboard,bool
		      else fail error blackboard stack tail 
		  else fail error blackboard stack tail 
	      end
		
      and iter parameter handler error blackboard stack = 
	let error,bool = B.is_maximal_solution parameter handler error blackboard in 
	  if bool 
	  then 
	    error,blackboard,bool
	  else
	    let error,list = B.next_choice parameter handler error blackboard in 
	      branch_over_assumption_list parameter handler error list blackboard stack  
		
    end 
      
