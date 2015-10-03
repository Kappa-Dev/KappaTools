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
	  "--compute-reachability-analysis";
	],[]),"launch everything",["0_Actions"],Normal;
      "--reset-all",
      Multi(
	[
	  "--no-compute-contact-map";
	  "--no-compute-influence-map";
	  "--no-compute-ODE-flow-of-information";
	  "--no-compute-stochastic-flow-of-information";
	  "--no-compute-reachability-analysis";
	],[]),"reset everything",["0_Actions"],Normal;

      "--compute-contact-map",
      Bool Config.do_contact_map, 
      "compute the contact map",
      ["0_Actions";"3_Contact_map"],
      Normal;
      
      "--compute-influence-map",
      Bool Config.do_influence_map,
      "compute the influence map",
      ["0_Actions";"4_Influence_map"],
      Normal; 

       "--influence-map-accuracy-level",
        (Choice 
           (["Low","Ignore relations among site";
             "Medium","Ignore reachable states";
             "High",""],
            Config.influence_map_accuracy_level)),
        "Tune the accuracy level of the influence map",
	["4_Influence_map"],
	Normal;
       
      "--compute-ODE-flow-of-information", 
      Bool Config.do_ODE_flow_of_information,
      "Compute an approximation of the flow of information in the ODE semantics",
      ["0_Actions";"5_Flow_of_information"],
      Expert;
      
      "--compute-stochastic-flow-of-information",
      Bool Config.do_stochastic_flow_of_information,
      "Compute an approximation of the flow of information in the stochastic semantics",
      ["0_Actions";"5_Flow_of_information"],
      Expert;

      "--compute-reachability-analysis",
      Bool Config.do_reachability_analysis,
      "Compute iteration between the sites of agents",
      ["0_Actions";"2_Reachability_analysis"],
      Normal;

      "--view-analysis",
      (Choice 
         ([(*"None","No view analysis";*)
	   (*"Low","Non relational site analysis";*)
	   "High","Relational view analysis"],
          Config.view_accuracy_level)),
        "Tune the accuracy level of the view analysis",
	["2_Reachability_analysis"],
	Normal;
      
            
      "--output-directory",
      String Config.output_directory,
      "put output files in this directory",
      ["1_Output";"2_Reachability_analysis";"3_Contact_map";"4_Influence_map"], 
      Normal;

      "--contact-map-accuracy-level",
        (Choice 
           (["Low","Collect info from rhs of rules and initial state";
             "High","Only consider reachable rules";
             ],
            Config.contact_map_accuracy_level)),
        "Tune the accuracy level of the influence map",
	["3_Contact_map"],
	Normal;
       
      
      
      "--output-contact-map",
      String Config.contact_map_file,
      "file name for the contact map output",
      ["1_Output";"3_Contact_map"],
      Normal;
      
      "--output-influence-map", 
      String Config.influence_map_file,
      "file name for the influence map",
      ["1_Output";"4_Influence_map"],
      Normal;
      
      "--debugging-mode",
      Bool Config.trace,
      "dump debugging information",
      ["6_debugging_info"],
      Expert;
    ]

let get_option error = 
 let parameters = Remanent_parameters.get_parameters () in   
 let _ = SuperargTk.parse parameters options FileNames.input in 
 let parameters = Remanent_parameters.get_parameters () in   
 error,parameters,!FileNames.input 
  
