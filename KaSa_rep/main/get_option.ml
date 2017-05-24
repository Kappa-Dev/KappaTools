(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 18/12/2010
  * Last modification: Time-stamp: <May 24 2017>
  * *
  * primitive to parse command-line options
  *
  * Copyright 2010 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Superarg

let actions = "Actions",0,None
let output = "Output",1,None
let reachability = "Reachability analysis",2,None
let traces = "Trace analysis",3,None
let contact_map = "Contact map",4,None
let influence_map = "Influence map",5,None
let flow = "Flow of information",6,None
let debug = "Debugging information",7,None

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
        ],[]),"launch everything",[actions,0],Normal;
      "--reset-all",
      Multi(
        [
          "--no-compute-contact-map";
          "--no-compute-influence-map";
          "--no-compute-ODE-flow-of-information";
          "--no-compute-stochastic-flow-of-information";
          "--no-compute-reachability-analysis";
        ],[]),"launch nothing",[actions,1],Normal;
      "--void",Void,"",[actions,2],Normal;
      "--compute-contact-map",
      Bool Config.do_contact_map,
      "compute the contact map",
      [actions,2;contact_map,0],
      Normal;

      "--compute-influence-map",
      Bool Config.do_influence_map,
      "compute the influence map",
      [actions,3;influence_map,0],
      Normal;

      "--influence-map-accuracy-level",
      (Choice
         (["Indirect","Ignore gluing compatibility";
           "Direct","Ignore reachable states";
           "Realisable","Take into account reachable states" ],
          ["Low";"High";"Full";"Medium"],
          Config.influence_map_accuracy_level)),
      "Tune the accuracy level of the influence map",
      [influence_map,1],
      Normal;

      "--compute-ODE-flow-of-information",
      Bool Config.do_ODE_flow_of_information,
      "Compute an approximation of the flow of information in the ODE semantics",
      [actions,4;flow,6],
      Expert;

      "--compute-stochastic-flow-of-information",
      Bool Config.do_stochastic_flow_of_information,
      "Compute an approximation of the flow of information in the stochastic semantics",
      [actions,5;flow,6],
      Expert;

      "--compute-reachability-analysis",
      Bool Config.do_reachability_analysis,
      "Compute an approximation of the states of agent sites",
      [actions,6;reachability,0],
      Normal;
      "--enable-every-domain",
      Multi(
        [
          "--contact-map-domain";"dynamic";
          "--views-domain";
          "--double-bonds-domain";
          "--sites-across-bonds-domain";
        ],[]),"enable every abstract domain",[reachability,1],Normal;
      "--disable-every-domain",
      Multi(
        [
        "--contact-map-domain";"static";
          "--no-views-domain";
          "--no-double-bonds-domain";
          "--no-sites-across-bonds-domain";
        ],[]),"disable every abstract domain",[reachability,2],Normal;
      "--contact-map-domain",
      Choice
         (["static","Very coarse static abstraction: every bond that occurs in initial states and rhs of rules is considered";
           "dynamic","More accurate abstraction: only the bonds that occur in the initial state or a rule that has already been applied successfully, are considered"],
          [],
          Config.with_dynamic_contact_map),"contact map domain is used to over-approximate side-effects",
       [reachability,3],Normal;
       "--views-domain",
      Bool Config.with_views_analysis,
          "enable local views analysis",
          [reachability,4],Normal;
      "--double-bonds-domain",
      Bool Config.with_parallel_bonds_analysis,
      "enable double bonds analysis",
      [reachability,5],Normal;
      "--sites-across-bonds-domain",
      Bool Config.with_site_across_bonds_analysis,
      "enable the analysis of the relation among the states of sites in connected agents",
      [reachability,6],Normal;
      "--sites-accross-bonds-domain",
      Bool Config.with_site_across_bonds_analysis,
      "enable the analysis of the relation among the states of sites in connected agents",
      [reachability,6],Hidden;

      "--compute-symmetries",
      Bool Config.do_symmetries,
      "Look up for pairs of symmetric sites",
      [actions,7],Normal;
      "--verbosity-level-for-view-analysis",
      (Choice
         ([
           "Mute","No information displayed";
           "Low","Show analysis result only";
           "Medium","Also show which rules are applied";
           "High","Also show when new views are discovered";
           "Full","Also show which rules are put in the working list"],
           [],
           Config.verbosity_level_for_reachability_analysis)),
      "Tune the verbosity level for the view analysis",
      [reachability,7],
      Hidden;

      "--verbosity-level-for-reachability-analysis",
      (Choice
         ([
           "Mute","No information displayed";
           "Low","Show analysis result only";
           "Medium","Also show which rules are applied";
           "High","Also show when patterns are discovered";
           "Full","Also show which rules are put in the working list"],
           [],
           Config.verbosity_level_for_reachability_analysis)),
      "Tune the verbosity level for the reachability analysis",
      [reachability,8],
      Normal;
      "--hide-one-d-relations-from-cartesian-decomposition",
      Bool Config.hide_one_d_relations_from_cartesian_decomposition,
      "Filter out 1-d relations from the Cartesian decomposition",
      [reachability,9],
      Developper;

      "--smash-relations",
      Bool Config.smash_relations,
      "Recombine relations to get a more precise & compact output",
      [reachability,10],
      Developper;

      "--output-mode-for-reachability-analysis",
      (Choice (["raw","no post-processing";
                "kappa","kappa mode";
                "english","natural language"
               ],[],
               Config.use_natural_language)),
      "post-process relation and output the result in the chosen format",
      [reachability,11],
      Normal;

      "--compute-local-traces",
      Bool Config.compute_local_traces,
      "Compute the local traces of interesting parts of agent interfaces",
      [actions,8;traces,0],
      Normal;

      "--show-rule-names-in-local-traces",
      Bool Config.show_rule_names_in_local_traces,
      "Annotate each transition with the name of the rules in trace abstraction",
      [traces,1],
      Normal;

      "--use-macrotransitions-in-local-traces",
      Bool Config.use_macrotransitions_in_local_traces,
      "Use macrotransitions to get a compact trace up to change of the interleaving order of commuting microtransitions",
      [traces,2],
      Normal;

      "--ignore-trivial-losanges",
      Bool Config.do_not_compress_trivial_losanges,
      "Do not use macrotransitions for simplifying trivial losanges",
      [traces,3],
      Expert;

      "--add-singular-macrostates",
      Bool Config.add_singular_macrostates,
      "Complete the simplicial complexes with singular intersection of higher-dimension faces",
      [traces,4],
      Hidden;

      "--add-singular-microstates",
      Bool Config.add_singular_microstates,
      "Complete the simplicial complexes with singular intersection of higher-dimension faces",
      [traces,5],
      Hidden;

      "--compute-separating-transitions",
      Bool Config.compute_separating_transitions,
      "Compute the transitions that separates strongly connected set of configurations",
      [actions,9;traces,6],
      Normal;

      "--output-directory",
      MultiExt
        [
          "--output-contact-map-directory","";
          "--output-influence-map-directory","";
          "--output-local-traces-directory","";
          "--output-log-directory",""],
      "Default repository for outputs",
      [output,0],
      Normal;

      (* CONTACT MAP *)
      "--output-contact-map-directory",
      String Config.output_cm_directory,
      "put the contact map file in this directory",
      [output,0;contact_map,2],
      Normal;

      "--output-contact-map",
      String Config.contact_map_file,
      "file name for the contact map output",
      [output,1;contact_map,2],
      Normal;

      "--contact-map-format",
      (Choice (["DOT","dot format";
                (*"HTML","HTML format"*)],[],
               Config.contact_map_format)),
      "Tune the output format for the contact map",
      [output,2;contact_map,3],
      Hidden;

      "--contact-map-accuracy-level",
      (Choice
         (["Low","Collect info from rhs of rules and initial state";
           "High","Only consider reachable rules";
          ],[],
          Config.contact_map_accuracy_level)),
      "Tune the accuracy level of the contact map",
      [contact_map,4],
      Expert;

      "--pure-contact",
      Bool Config.pure_contact,
      "show in the contact map  only the sites with a binding state",
      [contact_map,5],
      Expert;



      (* INFLUENCE MAP *)
      "--output-influence-map-directory",
      String Config.output_im_directory,
      "put the influence map file in this directory",
      [output,3;influence_map,2],
      Normal;

      "--output-influence-map",
      String Config.influence_map_file,
      "file name for the influence map",
      [output,3;influence_map,3],
      Normal;

      "--influence-map-format",
      (Choice ([
           "DOT","dot format";
           "DIM","DIM format";
           "HTML","HTML format";
         ],[],
           Config.influence_map_format)),
      "Tune the output format for the influence map",
      [output,4;influence_map,4],
      Normal;



      (* LOCAL TRACES *)
      "--output-local-traces-directory",
      String Config.output_local_trace_directory,
      "put the files about local traces in this directory",
      [output,5;traces,7],
      Normal;

      "--local-traces-format",
      (Choice (
          [
            "DOT","dot format";
            "HTML","HTML format"
          ],[],
          Config.local_trace_format)),
      "Tune the output format for the local transition systems",
      [output,6;traces,8],
      Normal;

      (* LOG *)
      "--output-log-directory",
      String Config.output_directory,
      "put the log files in this directory",
      [output,7;debug,0],
      Expert;
      "--debug",
      Bool Config.trace,
      "dump debugging information",
      [debug,1],
      Expert;
      "--debug-mode",
      Bool Config.trace,
      "dump debugging information",
      [debug,2],
      Hidden;
      "--debugging-mode",
      Bool Config.trace,
      "dump debugging information",
      [debug,3],
      Hidden;
      "--unsafe-mode",
      Bool Config.unsafe,
      "exceptions are gathered at the end of the computation, instead of halting it ",
      [debug,4],
      Expert;
      "--print-efficiency",
      Bool Config.print_efficiency,
      "prompt CPU time and various datas",
      [debug,5],
      Expert;
    ]

let get_option error =
  let () = SuperargTk.parse options FileNames.input in
  let parameters =
    Remanent_parameters.get_parameters
      ~called_from:Remanent_parameters_sig.KaSa () in
  error,parameters,!FileNames.input
