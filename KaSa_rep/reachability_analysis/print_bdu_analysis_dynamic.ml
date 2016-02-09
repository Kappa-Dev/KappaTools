(**
  * print_bdu_analysis.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 28th of October
  * Last modification:
  *
  * Print relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Printf
open Remanent_parameters_sig
open Cckappa_sig
open Bdu_analysis_type
open Fifo

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)

let trace = false

(************************************************************************************)
(*syntactic contact map*)

(*let print_contact_map_aux parameter error handler_kappa result =
  Int2Map_CM_Syntactic.Map.iter (fun set1 set2 ->
    Set_triple.Set.iter (fun (agent1, site1, state1) ->
      Set_triple.Set.iter (fun (agent2, site2, state2) ->
        let error, agent_string1 =
          Handler.string_of_agent parameter error handler_kappa agent1
        in
        let error, site_string1 =
          Handler.string_of_site_contact_map parameter error handler_kappa agent1 site1
        in
        let error, state_string1 =
          Handler.string_of_state parameter error handler_kappa agent1 site1 state1
        in
        let error, agent_string2 =
          Handler.string_of_agent parameter error handler_kappa agent2
        in
        let error, site_string2 =
          Handler.string_of_site_contact_map parameter error handler_kappa agent2 site2
        in
        let error, state_string2 =
          Handler.string_of_state parameter error handler_kappa agent2 site2 state2
        in
        fprintf stdout "agent_type:%i:%s@@site_type:%i:%s:state:%i(%s)--agent_type':%i:%s@@site_type':%i:%s:state':%i(%s)\n"
          agent1 agent_string1 site1 site_string1 state1 state_string1
          agent2 agent_string2 site2 site_string2 state2 state_string2
      ) set2
    )set1
  ) result

let print_contact_map parameter error handler_kappa result =
  fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "(Syntactic) Contact map and initital state:\n";
  fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  fprintf (Remanent_parameters.get_log parameter)
    "Sites are annotated with the id of binding type:\n";
  let error =
    print_contact_map_aux
      parameter
      error
      handler_kappa
      result
  in
  error*)

(************************************************************************************)
(*dynamic contact map full information*)

let print_contact_map_full_aux parameter error handler_kappa result =
  Int2Map_CM_state.Map.iter (fun (agent1, site1, state1) set ->
    Set_triple.Set.iter (fun (agent2, site2, state2) ->
      let error, agent_string1 =
        try
          Handler.string_of_agent parameter error handler_kappa agent1
        with
          _ -> warn parameter error (Some "line 87") Exit (string_of_int agent1)
      in
      let error, site_string1 =
        try
          Handler.string_of_site_contact_map parameter error handler_kappa agent1 site1
        with
          _ -> warn parameter error (Some "line 92") Exit (string_of_int site1)
      in
      let error, state_string1 =
        try
          Handler.string_of_state parameter error handler_kappa agent1 site1 state1
        with
          _ -> warn parameter error (Some "line 99") Exit (string_of_int state1)
      in
      let error, agent_string2 =
        try
          Handler.string_of_agent parameter error handler_kappa agent2
        with
          _ -> warn parameter error (Some "line 105") Exit (string_of_int agent2)
      in
      let error, site_string2 =
        try
          Handler.string_of_site_contact_map parameter error handler_kappa agent2 site2
        with
          _ -> warn parameter error (Some "line 111") Exit (string_of_int site2)
      in
      let error, state_string2 =
        try
          Handler.string_of_state parameter error handler_kappa agent2 site2 state2
        with
          _ -> warn parameter error (Some "line 117") Exit (string_of_int state2)
      in
      fprintf stdout
        "agent_type:%i:%s@@site_type:%i:%s:state:%i(%s)--agent_type':%i:%s@@site_type':%i:%s:state':%i(%s)\n"
        agent1 agent_string1
        site1 site_string1
        state1 state_string1
        agent2 agent_string2
        site2 site_string2
        state2 state_string2
    ) set
  ) result

let print_contact_map_full parameter error handler_kappa result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "\n------------------------------------------------------------\n";
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "(Full) Contact map and initital state:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites are annotated with the id of binding type:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_contact_map_full_aux
      parameter
      error
      handler_kappa
      result
  in
  error

(************************************************************************************)
(*print init map*)

(*let print_init_map_aux parameter error handler_kappa result =
  Int2Map_CM_Syntactic.Map.iter
    (fun set1 set2 ->
      Set_triple.Set.iter (fun (agent_type, site_type, state) ->
        Set_triple.Set.iter (fun (agent_type', site_type', state') ->
          Loggers.fprintf stdout "agent_type:%i:site_type:%i:state:%i - > agent_type':%i:site_type':%i:state':%i\n"
            agent_type site_type state
            agent_type' site_type' state'
        ) set2
      ) set1
    ) result

let print_init_map parameter error handler_kappa result =
  Loggers.fprintf (Remanent_parameters.get_log parameter)
    "\n------------------------------------------------------------\n";
  Loggers.fprintf (Remanent_parameters.get_log parameter)
    " Contact map in the initital state:\n";
  Loggers.fprintf (Remanent_parameters.get_log parameter)
    "------------------------------------------------------------\n";
  Loggers.fprintf (Remanent_parameters.get_log parameter)
    "Sites are annotated with the id of binding type:\n";
  let error =
    print_init_map_aux
      parameter
      error
      handler_kappa
      result
  in
  error*)

(************************************************************************************)
(*syntactic contact map and init map*)

let print_syn_map_aux parameter error handler_kappa result =
  Int2Map_CM_Syntactic.Map.iter
    (fun set1 set2 ->
      Set_triple.Set.iter (fun (agent_type, site_type, state) ->
        let error, agent_string =
          try
            Handler.string_of_agent parameter error handler_kappa agent_type
          with
            _ -> warn parameter error (Some "line 192") Exit (string_of_int agent_type)
        in
        let error, site_string =
          try
            Handler.string_of_site_contact_map
              parameter error handler_kappa agent_type site_type
          with
            _ -> warn parameter error (Some "line 199") Exit (string_of_int site_type)
        in
        let error, state_string =
          try
            Handler.string_of_state parameter error handler_kappa agent_type site_type state
          with
            _ -> warn parameter error (Some "line 205") Exit (string_of_int state)
        in
        Set_triple.Set.iter (fun (agent_type', site_type', state') ->
          let error, agent_string' =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type'
            with
              _ -> warn parameter error (Some "line 212") Exit (string_of_int agent_type')
          in
          let error, site_string' =
            try
              Handler.string_of_site_contact_map
                parameter error handler_kappa agent_type' site_type'
            with
              _ -> warn parameter error (Some "line 218") Exit (string_of_int site_type')
          in
          let error, state_string' =
            try
              Handler.string_of_state parameter error handler_kappa
                agent_type' site_type' state'
            with
              _ -> warn parameter error (Some "line 226") Exit (string_of_int state')
          in
          let () =
	    Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "agent_type:%i:%s:site_type:%i:%s:state:%i(%s) - > agent_type':%i:%s:site_type':%i:%s:state':%i(%s)"
            agent_type agent_string
            site_type site_string
            state state_string
            agent_type' agent_string'
            site_type' site_string'
            state' state_string'
	  in
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
        ) set2
      ) set1
    ) result

let print_syn_map parameter error handler_kappa result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "(Syntactic) Contact map and initital state:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites are annotated with the id of binding type:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_syn_map_aux
      parameter
      error
      handler_kappa
      result
  in
  error

(************************************************************************************)
(*update (c) function*)

let print_covering_classes_modification_aux parameter error handler_kappa compiled result =
  Int2Map_CV_Modif.Map.iter
    ( fun (agent_type, y) (_, s2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 268") Exit (string_of_int agent_type)
      in
      let () =
        Loggers.fprintf
	  (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:covering_class_id:%i:@set of rule_id:"
          agent_type agent_string y
      in
      let () =Loggers.print_newline (Remanent_parameters.get_logger parameter) in
     Site_map_and_set.Set.iter
        (fun rule_id ->
        (*mapping rule_id of type int to string*)
          let error, rule_id_string =
            try
              Handler.string_of_rule parameter error handler_kappa
                compiled rule_id
            with
              _ -> warn parameter error (Some "line 283") Exit (string_of_int rule_id)
          in
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" rule_id_string in
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
        ) s2
    ) result

let print_covering_classes_modification parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "List of rules to awake when the state of a site is modified and tested:";
   Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  print_covering_classes_modification_aux
    parameter
    error
    handler_kappa
    compiled
    result

(************************************************************************************)
(*update(c'), when discovered a bond for the first time*)

let print_covering_classes_side_effects parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "List of rules to awake when the state of a site is modified and tested and side effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
Loggers.print_newline (Remanent_parameters.get_logger parameter);
print_covering_classes_modification_aux
    parameter
    error
    handler_kappa
    compiled
    result

(************************************************************************************)
(*Final update function*)

let print_covering_classes_update_full parameter error handler_kappa compiled result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Final list of rules to awake when the state of a site is modified and tested and side effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  print_covering_classes_modification_aux
    parameter
    error
    handler_kappa
    compiled
    result

(************************************************************************************)
(*main print*)

let print_result_dynamic parameter error handler_kappa compiled result =
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "============================================================";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter) "* BDU Analysis:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "============================================================";
  Loggers.print_newline (Remanent_parameters.get_logger parameter); 
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "** Dynamic information:";
    (*------------------------------------------------------------------------------*)
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let _ =
    print_contact_map_full
      parameter
        error
      handler_kappa
      result.store_contact_map_full
  in
  (*------------------------------------------------------------------------------*)
  let _ =
      print_syn_map
        parameter
        error
        handler_kappa
        result.store_syn_contact_map_full
  in
  (*------------------------------------------------------------------------------*)
  let _ =
    print_covering_classes_update_full
        parameter
      error
      handler_kappa
      compiled
      result.store_covering_classes_modification_update_full
  in
  error
  in
  error
    
