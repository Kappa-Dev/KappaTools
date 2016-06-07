(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 18/12/2010
  * Last modification: Time-stamp: <2016-03-23 20:52:19 feret>
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
      ["0_Actions";"4_Contact_map"],
      Normal;

      "--compute-influence-map",
      Bool Config.do_influence_map,
      "compute the influence map",
      ["0_Actions";"5_Influence_map"],
      Normal;

       "--influence-map-accuracy-level",
        (Choice
           (["Low","Ignore relations among site";
             "Medium","Ignore reachable states";
(*    "High",""*) ],
            Config.influence_map_accuracy_level)),
       "Tune the accuracy level of the influence map",
       ["5_Influence_map"],
       Normal;

      "--compute-ODE-flow-of-information",
      Bool Config.do_ODE_flow_of_information,
      "Compute an approximation of the flow of information in the ODE semantics",
      ["0_Actions";"6_Flow_of_information"],
      Expert;

      "--compute-stochastic-flow-of-information",
      Bool Config.do_stochastic_flow_of_information,
      "Compute an approximation of the flow of information in the stochastic semantics",
      ["0_Actions";"6_Flow_of_information"],
      Expert;

      "--compute-reachability-analysis",
      Bool Config.do_reachability_analysis,
      "Compute an approximation of the states of agent sites",
      ["0_Actions";"2_Reachability_analysis"],
      Normal;

      "--view-analysis",
      (Choice
         ([
           (*"None","No view analysis";*)
           (*"Low","Non relational site analysis";*)
           "High","Relational view analysis"],
           Config.view_accuracy_level)),
        "Tune the accuracy level of the view analysis",
      ["2_Reachability_analysis"],
      Expert;


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

      "--compute-local-traces",
      Bool Config.compute_local_traces,
      "Compute the local traces of interesting parts of agent interfaces",
      ["0_Actions";"3_Trace_analysis"],
      Normal;

      "--show-rule-names-in-local-traces",
      Bool Config.show_rule_names_in_local_traces,
      "Annotate each transition with the name of the rules in trace abstraction",
      ["3_Trace_analysis"],
      Normal;

      "--use-macrotransitions-in-local-traces",
      Bool Config.use_macrotransitions_in_local_traces,
      "Use macrotransitions to get a compact trace up to change of the interleaving order of commuting microtransitions",
      ["3_Trace_analysis"],
      Normal;

      "--ignore-trivial-losanges",
      Bool Config.do_not_compress_trivial_losanges,
      "Do not use macrotransitions for simplifying trivial losanges",
      ["3_Trace_analysis"],
      Expert;

      "--add-singular-macrostates",
      Bool Config.add_singular_macrostates,
      "Complete the simplicial complexes with singular intersection of higher-dimension faces",
      ["3_Trace_analysis"],
      Expert;

      "--add-singular-microstates",
      Bool Config.add_singular_macrostates,
      "Complete the simplicial complexes with singular intersection of higher-dimension faces",
      ["3_Trace_analysis"],
      Developper;

      "--output-directory",
       MultiExt
	[
	  "--output-contact-map-directory","";
	  "--output-influence-map-directory","";
	  "--output-local-traces-directory","";
	  "--output-log-directory",""],
      "Default repository for outputs",
      ["1_Output"],
      Normal;
      "--output-contact-map-directory",
      String Config.output_cm_directory,
      "put the contact map file in this directory",
      ["1_Output";"4_Contact_map"],
      Normal;
      "--output-influence-map-directory",
      String Config.output_im_directory,
      "put the influence map file in this directory",
      ["1_Output";"5_Influence_map"],
      Normal;
       "--output-local-traces-directory",
      String Config.output_local_trace_directory,
      "put the files about local traces in this directory",
      ["1_Output";"3_Trace_analysis"],
      Normal;
       "--output-log-directory",
      String Config.output_directory,
      "put the log files in this directory",
      ["1_Output";"7_Debugging_info"],
      Expert;
      "--contact-map-format",
      (Choice (["DOT","dot format";
                (*"HTML","HTML format"*)],
	       Config.contact_map_format)),
      "Tune the output format for the contact map",
      ["1_Output";"4_Contact_map"],
        Expert;
      "--influence-map-format",
      (Choice ([
           "DOT","dot format";
           "HTML","HTML format"
         ],
       Config.influence_map_format)),
      "Tune the output format for the influence map",
      ["1_Output";"5_Influence_map"],
        Expert;
      "--local-traces-format",
      (Choice (
          [
            "DOT","dot format";
            "HTML","HTML format"
          ],
       Config.local_trace_format)),
      "Tune the output format for the local transition systems",
      ["1_Output";"3_Trace_analysis"],
      Normal;
        "--contact-map-accuracy-level",
        (Choice
           (["Low","Collect info from rhs of rules and initial state";
             (*    "High","Only consider reachable rules";*)
             ],
            Config.contact_map_accuracy_level)),
        "Tune the accuracy level of the contact map",
        ["4_Contact_map"],
          Expert;

      "--pure-contact",
      Bool Config.pure_contact,
      "show in the contact map  only the sites with a binding state",
      ["4_Contact_map"],
      Expert;

      "--output-contact-map",
      String Config.contact_map_file,
      "file name for the contact map output",
      ["1_Output";"4_Contact_map"],
      Normal;

      "--output-influence-map",
      String Config.influence_map_file,
      "file name for the influence map",
      ["1_Output";"5_Influence_map"],
      Normal;

      "--debugging-mode",
      Bool Config.trace,
      "dump debugging information",
      ["7_Debugging_info"],
      Expert;

      "--unsafe-mode",
      Bool Config.unsafe,
      "Exceptions are gathered at the end of the computation, instead of halting it ",
      ["7_Debugging_info"],
      Expert;
    ]

let get_option error =
  let parameters = Remanent_parameters.get_parameters
		     ~called_from:Remanent_parameters_sig.Internalised () in
  let () = SuperargTk.parse parameters options FileNames.input in
  let parameters = Remanent_parameters.get_parameters
		     ~called_from:Remanent_parameters_sig.KaSa () in
  error,parameters,!FileNames.input
