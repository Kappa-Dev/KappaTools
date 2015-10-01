let dot_comment = "#" 

		    
let head parameters = ["This file has been computed by KaSa: a Static Analyzer for Kappa ("^(Remanent_parameters.get_short_version parameters)^")";
		       "Download sources/binaries at https://github.com/Kappa-Dev/KaSim"; 
		       "";	    
		       Remanent_parameters.get_launched_when_and_where parameters;
		       "Command line is: "^(String.concat " " (match Array.to_list (Remanent_parameters.get_command_line parameters) with t::q -> "KaSa"::q | [] -> []));
  		       "";
	   ]		    
		    
let dot_to_pdf = "Please use graphviz (http://www.graphviz.org) or OmniGraffle to export it to a PDF"
    			      					   
let head_influence_map_in_dot =
  ["This file contains the description of the influence map in dot.";
   dot_to_pdf;
  ""]
let head_contact_map_in_dot =
  ["This file contains the description of the contact map in dot.";
   dot_to_pdf;
  ""]
