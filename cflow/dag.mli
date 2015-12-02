(**
  * dag.mli
  *
  * Dag computation and canonical form 
  *
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Diderot, CNRS 
  *  
  * Creation: 02/12/2025
  * Last modification: 02/12/2015
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)



module type StoryTable = 
  sig
    type table
	   
    val fold_table: (((Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.refined_step list -> Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info list -> 'a -> Exception.method_handler * 'a) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler) -> table -> 'a -> Exception.method_handler * 'a) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler 
    val init_table: (Exception.method_handler * table) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler 
    val count_stories: table -> int 
    val add_story: (Causal.grid -> Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.refined_step list -> Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info list -> table -> Exception.method_handler * table) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler 
    val hash_list: (table -> Exception.method_handler * table) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler  
      
    val sort_list: (table -> Exception.method_handler * (Causal.grid * Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.P.log_info Mods.simulation_info list) list) Generic_branch_and_cut_solver.Solver.PH.B.PB.CI.Po.K.H.with_handler 
  end

module StoryTable:StoryTable
       
