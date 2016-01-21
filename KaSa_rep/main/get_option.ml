(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 18/12/2010
  * Last modification: Time-stamp: <2016-01-21 15:24:58 feret>
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
	],[]),"launch nothing",["0_Actions"],Normal;

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
(*    "High",""*) ],
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

      "--verbosity-level-for-view-analysis",
      (Choice
	 ([
	     "Mute","No information displayed";
	     "Low","Show analysis result only";
	     "Medium","Also show which rules are applied";
	     "High","Also show when new views are discovered";
	     "Full","Also show which rules are put in the working list"],
	  Config.verbosity_level_for_reachability_analysis)),
	  "Tune the verbosity level for the view analysis",
	  ["2_Reachability_analysis"],
	  Normal;

      "--hide-one-d-relations-from-cartesian-decomposition",
      Bool Config.hide_one_d_relations_from_cartesian_decomposition,
      "Filter out 1-d relations from the Cartesian decomposition",
      ["2_Reachability_analysis"],
      Developper;

      "--smash-relations",
      Bool Config.smash_relations,
      "Recombine relations to get a more precise & compact output",
      ["2_Reachability_analysis"],
      Developper;

      "--use-natural-language",
      Bool Config.use_natural_language,
      "translate relations in sentences when it is possible",
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
             (*    "High","Only consider reachable rules";*)
             ],
            Config.contact_map_accuracy_level)),
        "Tune the accuracy level of the contact map",
	["3_Contact_map"],
	Normal;

      "--pure-contact",
      Bool Config.pure_contact,
      "show in the contact map  only the sites with a binding state",
      ["3_Contact_map"],
      Expert;

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
 let parameters = Remanent_parameters.get_parameters ~called_from:Remanent_parameters_sig.Internalised () in
 let _ = SuperargTk.parse parameters options FileNames.input in
 let parameters = Remanent_parameters.get_parameters () in
 error,parameters,!FileNames.input

