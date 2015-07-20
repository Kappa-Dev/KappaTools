(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 18/12/2010
  * Last modification: Time-stamp: <2015-04-17 20:53:09 feret>
  * * 
  * primitive to parse command-line options 
  *  
  * Copyright 2010 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Superarg
open SuperargTk 

let options = 
  List.rev 
    [
      "--do-all", 
      Multi(
	[
	  "--compute-contact-map";
	  "--compute-influence-map";
	  "--compute-ODE-flow-of-information";
	  "--compute-stochastic-flow-of-information";
	  "--compute-site-dependencies";
	  "--compute-iteration-dependencies";
	],[]),"launch everything",["0_Actions"],Normal;
      "--reset-all",
      Multi(
	[
	  "--no-compute-contact-map";
	  "--no-compute-influence-map";
	  "--no-compute-ODE-flow-of-information";
	  "--no-compute-stochastic-flow-of-information";
	  "--no-compute-site-dependencies";
	  "--no-compute-iteration-dependencies";
	],[]),"reset everything",["0_Actions"],Normal;

      "--compute-contact-map",
      Bool Config.do_contact_map, 
      "compute the contact map",
      ["0_Actions";"2_Reachability_analysis"],
      Normal;
      
      "--compute-influence-map",
      Bool Config.do_influence_map,
      "compute the influence map",
      ["0_Actions";"3_Influence_map"],
      Normal; 

      "--compute-ODE-flow-of-information", 
      Bool Config.do_ODE_flow_of_information,
      "Compute an approximation of the flow of information in the ODE semantics",
      ["0_Actions";"3b_Flow_of_information"],
      Hidden;
      
      "--compute-stochastic-flow-of-information",
      Bool Config.do_stochastic_flow_of_information,
      "Compute an approximation of the flow of information in the stochastic semantics",
      ["0_Actions";"3b_Flow_of_information"],
      Hidden;

      "--compute-site-dependencies",
      Bool Config.do_site_dependencies,
      "Compute potential relations between the sites of agents",
      ["0_Actions";"3b_Flow_of_information"],
      Hidden;

      "--compute-iteration-dependencies",
      Bool Config.do_iteration_dependencies,
      "Compute iteration between the sites of agents",
      ["0_Actions";"3b_Flow_of_information"],
      Hidden;
            
      "--output-directory",
      String Config.output_directory,
      "put output files in this directory",
      ["1_Output";"2_Reachability_analysis";"3_Influence_map"], 
      Normal;
      
      "--output-contact-map",
      String Config.contact_map_file,
      "file name for the contact map output",
      ["1_Output";"2_Reachability_analysis"],
      Normal;
      
      "--output-influence-map", 
      String Config.influence_map_file,
      "file name for the influence map",
      ["1_Output";"3_Influence_map"],
      Normal;
      
      "--debugging-mode",
      Bool Config.trace,
      "dump debugging information",
      ["4_debugging_info"],
      Expert;
    ]

let get_option error = 
 let parameters = Remanent_parameters.get_parameters () in   
 let _ = SuperargTk.parse parameters options FileNames.input in 
 let parameters = Remanent_parameters.get_parameters () in   
 error,parameters,!FileNames.input 
  
