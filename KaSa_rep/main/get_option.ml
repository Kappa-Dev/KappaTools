(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 18/12/2010
  * Last modification: 23/01/2015
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
	],[]),"launch everything",["0_Actions"],Normal;
      "--reset-all",
      Multi(
	[
	  "--no-compute-contact-map";
	  "--no-compute-influence-map";
	],[]),"reset everything",["0_Actions"],Normal;
      "--output-directory",String Config.output_directory,"put output files in this directory",["1_Output"],Normal;
       "--compute-contact-map",Bool Config.do_contact_map,"compute the contact map",["0_Actions";"2_Reachability_analysis"],Normal;
      "--output-contact-map",String Config.contact_map_file,"file name for the contact map output",["1_Output";"2_Reachability_analysis"],Normal;
      "--compute-influence-map",Bool Config.do_influence_map,"compute the influence map",["0_Actions";"3_Influence_map"],Normal; 
      "--output-influence-map",String Config.influence_map_file,"file name for the influence map",["1_Output";"3_Influence_map"],Normal;
      "--debugging-mode",Bool Config.trace,"dump debugging information",["4_debugging_info"],Normal;
    ]


let get_option error = 
 let parameters = Remanent_parameters.get_parameters () in   
 let _ = SuperargTk.parse parameters options FileNames.input in 
 let parameters = Remanent_parameters.get_parameters () in   
 error,parameters,!FileNames.input 
  
