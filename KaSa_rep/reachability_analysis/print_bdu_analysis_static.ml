(**
  * print_bdu_analysic_static.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 15th of July
  * Last modification:
  *
  * Print relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Remanent_parameters_sig
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU fixpoint iteration") message exn
    (fun () -> default)

let trace = false

(************************************************************************************)
(*static information of covering classes id*)

let print_covering_classes_id_aux parameter error handler_kappa result =
  Int2Map_CV.Map.iter
    ( fun (agent_type, site_type) (l1, l2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 36") Exit (string_of_int agent_type)
      in
      let error, site_string =
        try
          Handler.string_of_site parameter error handler_kappa agent_type site_type
        with
          _ -> warn parameter error (Some "line 39") Exit (string_of_int site_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:site_type:%i:%s@@list of covering_class_id:"
          agent_type agent_string site_type site_string
      in
      let _ =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      List.iter
	(fun id ->
	  let () =
	    Loggers.fprintf (Remanent_parameters.get_logger parameter) "covering_class_id:%i" id in
	Loggers.print_newline (Remanent_parameters.get_logger parameter))
        l2
    ) result

let print_covering_classes_id parameter error handler_kappa result =
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Mapping between sites and the covering classes they belong to:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_covering_classes_id_aux
      parameter
      error
      handler_kappa
      result
  in
  error

(************************************************************************************)
(*print functions for bdu_common_static*)

(************************************************************************************)
(*side effects*)

let print_half_break_effect parameter error handler_kappa compiled result =
  Int2Map_HalfBreak_effect.Map.iter
    ( fun (agent_type, site_type) (l1, l2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "79") Exit (string_of_int agent_type)
      in
      let error, site_string =
        try
          Handler.string_of_site parameter error handler_kappa agent_type site_type
        with
          _ -> warn parameter error (Some "line 85") Exit (string_of_int site_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:site_type:%i:%s@@list of pair (rule_id, binding state):"
          agent_type agent_string site_type site_string
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      List.iter (fun (rule_id, state) ->
        (*mapping rule_id of type int to string*)
        let error, rule_id_string =
          try
            Handler.string_of_rule parameter error handler_kappa
              compiled rule_id
          with
            _ -> warn parameter error (Some "line 98") Exit (string_of_int rule_id)
        in
        let error, state_string =
          try
            Handler.string_of_state parameter error handler_kappa agent_type site_type state
          with
            _ -> warn parameter error (Some "line 105") Exit (string_of_int state)
        in
	let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "(%s * state:%i:(%s))"
          rule_id_string state state_string in
	Loggers.print_newline (Remanent_parameters.get_logger parameter)) l2
    ) result

let print_remove_effect parameter error handler_kappa compiled result =
  Int2Map_Remove_effect.Map.iter
    ( fun (agent_type, site_type) (l1, l2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 118") Exit (string_of_int agent_type)
      in
      let error, site_string =
        try
          Handler.string_of_site parameter error handler_kappa agent_type site_type
        with
          _ -> warn parameter error (Some "line 124") Exit (string_of_int site_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:site_type:%i:%s@@list of pair (rule_id, binding state):"
          agent_type agent_string site_type site_string
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      List.iter (fun rule_id ->
        let error, rule_id_string =
          try
            Handler.string_of_rule parameter error handler_kappa
              compiled rule_id
          with
            _ -> warn parameter error (Some "line 137") Exit (string_of_int rule_id)
        in
	let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "(%s * state:no_information)" rule_id_string
	in
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
	) l2
    ) result

let print_side_effects_aux parameter error handler_kappa compiled result =
  let result_half_break, result_remove = result in
  let _ =
    print_half_break_effect
      parameter
      error
      handler_kappa
      compiled
      result_half_break
  in
  print_remove_effect
    parameter
    error
    handler_kappa
    compiled
    result_remove

let print_side_effects parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites that may/must occurs side-effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_side_effects_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*Potential partner side-effects*)

let print_potential_partner_free parameter error handler_kappa compiled result =
  Int2Map_potential_effect.Map.iter
    (fun (agent_type, rule_id) l ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 188") Exit (string_of_int rule_id)
      in
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 194") Exit (string_of_int agent_type)
      in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:%s:%s@@(site, free state)"
          agent_type agent_string rule_id_string
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      List.iter (fun (site_type, state) ->
        let error, site_string =
          try
            Handler.string_of_site parameter error handler_kappa agent_type site_type
          with
            _ -> warn parameter error (Some "line 205") Exit (string_of_int site_type)
        in
        let error, state_string =
          try
            Handler.string_of_state parameter error handler_kappa agent_type site_type state
          with
            _ -> warn parameter error (Some "line 211") Exit (string_of_int state)
        in
	let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "(site_type:%i:%s * state:%i(%s))" site_type site_string
	    state state_string
	in
	let () =
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
	in ()) l
    ) result

let print_potential_partner_bind parameter error handler_kappa compiled result =
  Int2Map_potential_effect.Map.iter
    (fun (agent_type, rule_id) l ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 226") Exit (string_of_int rule_id)
      in
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 232") Exit (string_of_int agent_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter) "agent_type:%i:%s:%s@@(site, binding state)"
          agent_type agent_string rule_id_string
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      List.iter (fun (site_type, state) ->
        let error, site_string =
          try
            Handler.string_of_site parameter error handler_kappa agent_type site_type
          with
            _ -> warn parameter error (Some "line 243") Exit (string_of_int site_type)
        in
         let error, state_string =
           try
             Handler.string_of_state parameter error handler_kappa agent_type site_type state
           with
             _ -> warn parameter error (Some "line 249") Exit (string_of_int state)
         in
	 let () =
           Loggers.fprintf (Remanent_parameters.get_logger parameter) "(site_type:%i:%s * state:%i(%s))" site_type site_string state state_string
	 in
	 let () =
	   Loggers.print_newline (Remanent_parameters.get_logger parameter)
	 in
	 ()
      ) l
    ) result

let print_potential_side_effects parameter error handler_kappa compiled result =
  let result1, result2 = result in
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Sites that may/must occurs in the potential partner side-effects:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter) "- Potential partner has site that is free:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let _ =
    print_potential_partner_free
      parameter
      error
      handler_kappa
      compiled
      result1
  in
  Loggers.fprintf (Remanent_parameters.get_logger parameter) "- Potential partner has site that is bind:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  print_potential_partner_bind
    parameter
    error
    handler_kappa
    compiled
    result2

(************************************************************************************)

let print_result_common_static parameter error handler_kappa compiled result =
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
      "** Common static information:";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
  in
  let _ =
    print_side_effects
      parameter
      error
      handler_kappa
      compiled
      result.store_side_effects
  in
  let _ =
    print_potential_side_effects
      parameter
      error
      handler_kappa
      compiled
      result.store_potential_side_effects
  in
  error

(************************************************************************************)
(*print functions of pre_static type*)
(************************************************************************************)
(*update of the views due to modification*)

(*with agent_id*)

let print_modification_sites_aux parameter error handler_kappa compiled result =
  Int2Map_Modif.Map.iter
    ( fun (agent_id, agent_type, site_type) (l1, s2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 293") Exit (string_of_int agent_type)
      in
      let error, site_string =
        try
          Handler.string_of_site parameter error handler_kappa agent_type site_type
        with
          _ -> warn parameter error (Some "line 299") Exit (string_of_int site_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_id:%i:agent_type:%i:%s:site_type:%i:%s@@set of rule_id:"
          agent_id agent_type agent_string site_type site_string
      in
      let () =
	Loggers.print_newline (Remanent_parameters.get_logger parameter) in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          let error, rule_id_string =
            try
              Handler.string_of_rule parameter error handler_kappa
                compiled rule_id
            with
              _ -> warn parameter error (Some "line 313") Exit (string_of_int rule_id)
          in
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" rule_id_string in
	  let () =
	    Loggers.print_newline (Remanent_parameters.get_logger parameter) in ()
        ) s2
    ) result

let print_modification_sites parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may modify a given site (excluding created agents):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_sites_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*with agent_id*)
let print_test_sites parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may test a given site:";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_sites_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*with agent_id*)
let print_test_modification_sites parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may test and modify a given site (excluding created agents):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_sites_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(*map*)
(*without agent_id*)
let print_modification_map_aux parameter error handler_kappa compiled result =
  Int2Map_Test_Modif.Map.iter
    ( fun (agent_type, site_type) (l1, s2) ->
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 344") Exit (string_of_int agent_type)
      in
      let error, site_string =
        try
          Handler.string_of_site parameter error handler_kappa agent_type site_type
        with
          _ -> warn parameter error (Some "line 350") Exit (string_of_int site_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_type:%i:%s:site_type:%i:%s@@set of rules:"
          agent_type agent_string site_type site_string
      in
      let _ =
	Loggers.print_newline (Remanent_parameters.get_logger parameter)
      in
      Site_map_and_set.Set.iter
        (fun rule_id ->
          let error, rule_id_string =
            try
              Handler.string_of_rule parameter error handler_kappa
                compiled rule_id
            with
              _ -> warn parameter error (Some "line 364") Exit (string_of_int rule_id)
          in
          Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s\n" rule_id_string
        ) s2
    ) result

let print_modification_map parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may modify a given site (excluding created agents, without agent_id):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_map_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*valuation of the views that are tested*)



(*without agent_id*)
let print_test_map parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may test a given site (without agent_id):";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
  Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_map_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

(************************************************************************************)
(*update and valuations of the views that are tested and modified.*)

(*without agent_id*)

let print_test_modification_map parameter error handler_kappa compiled result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "------------------------------------------------------------";
 Loggers.print_newline (Remanent_parameters.get_logger parameter);
 Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Set of rules that may test and modify a given site (excluding created agents, without agent_id):";
 Loggers.print_newline (Remanent_parameters.get_logger parameter);
 Loggers.fprintf (Remanent_parameters.get_logger parameter)
   "------------------------------------------------------------";
 Loggers.print_newline (Remanent_parameters.get_logger parameter);
  let error =
    print_modification_map_aux
      parameter
      error
      handler_kappa
      compiled
      result
  in
  error

let print_pre_static parameter error handler_kappa compiled result =
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
      "** Static information:";
    Loggers.print_newline (Remanent_parameters.get_logger parameter);
  in

  let _ =
    print_modification_sites
      parameter
      error
      handler_kappa
      compiled
      result.store_modification_sites
  in
  let _ =
    print_test_sites
      parameter
      error
      handler_kappa
      compiled
      result.store_test_sites
  in
  let _ =
    print_test_modification_sites
      parameter
      error
      handler_kappa
      compiled
      result.store_test_modification_sites
  in
  error

(************************************************************************************)  

let print_proj_bdu_creation_restriction_map parameter error handler_kappa compiled result =
  Map_final_creation_bdu.Map.iter
    (fun rule_id map ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 364") Exit (string_of_int rule_id)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "%s\n" rule_id_string
      in
      Map_agent_type_creation_bdu.Map.iter
        (fun (agent_type, cv_id) bdu ->
          let error, agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 344") Exit (string_of_int agent_type)
          in
          let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_type:%i:%s:cv_id:%i\n" 
              agent_type agent_string cv_id
          in
          Mvbdu_wrapper.Mvbdu.print parameter bdu
        ) map
    ) result

let print_modif_list_restriction_map parameter error handler_kappa compiled result =
  Map_modif_list.Map.iter
    (fun (agent_id, agent_type, rule_id, cv_id) list_a ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 364") Exit (string_of_int rule_id)
      in
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 344") Exit (string_of_int agent_type)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "%s\nagent_type:%i:%s:cv_id:%i\n" 
          rule_id_string agent_type agent_string cv_id
      in
      Mvbdu_wrapper.Mvbdu.print_association_list parameter list_a      
    ) result

let print_proj_bdu_potential_restriction_map parameter error handler_kappa compiled result =
  Map_final_potential_bdu.Map.iter
    (fun rule_id map ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 364") Exit (string_of_int rule_id)
      in
      let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "%s\n"
        rule_id_string
      in
      Map_agent_type_potential_bdu.Map.iter 
        (fun (agent_type, new_site_name, cv_id) (bdu, list_a) ->
          let error, agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 344") Exit (string_of_int agent_type)
          in
          let error, site_string =
            try
              Handler.string_of_site parameter error handler_kappa agent_type new_site_name
            with
              _ -> warn parameter error (Some "line 350") Exit (string_of_int new_site_name)
          in
          let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_type:%i:%s:new_site_name:%i:%s:cv_id:%i\n"
              agent_type agent_string new_site_name site_string cv_id
          in
          Mvbdu_wrapper.Mvbdu.print parameter bdu;
          Mvbdu_wrapper.Mvbdu.print_association_list parameter list_a
        )
        map
    ) result

let print_proj_bdu_test_restriction parameter error handler_kappa compiled result =
  Map_rule_id_views.Map.iter
    (fun rule_id map ->
      let error, rule_id_string =
        try
          Handler.string_of_rule parameter error handler_kappa
            compiled rule_id
        with
          _ -> warn parameter error (Some "line 364") Exit (string_of_int rule_id)
      in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "%s\n"  rule_id_string
      in
      Map_triple_views.Map.iter
        (fun (agent_id, agent_type, cv_id) bdu ->
          let error, agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 344") Exit (string_of_int agent_type)
          in
          let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_type:%i:%s:cv_id:%i\n"
              agent_type agent_string cv_id
          in
          Mvbdu_wrapper.Mvbdu.print parameter bdu
        ) map      
    ) result

let print_result_static parameter error handler_kappa compiled result =
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
       "** Static information:";
     Loggers.print_newline (Remanent_parameters.get_logger parameter);
  in
  let _ =
    print_covering_classes_id
      parameter
      error
      handler_kappa
      result.store_covering_classes_id
  in
  let _ =
    print_proj_bdu_creation_restriction_map
      parameter 
      error
      handler_kappa 
      compiled result.store_proj_bdu_creation_restriction_map
  in
  (*store_modif_list_restriction_map*)
  let _ =
    print_modif_list_restriction_map
      parameter
      error
      handler_kappa
      compiled
      result.store_modif_list_restriction_map
  in
  (*store_proj_bdu_potential_restriction_map*)
  let _ =
    print_proj_bdu_potential_restriction_map
      parameter
      error
      handler_kappa
      compiled
      result.store_proj_bdu_potential_restriction_map
  in
  (*store_proj_bdu_test_restriction*)
  let _ =
    print_proj_bdu_test_restriction
      parameter
      error
      handler_kappa
      compiled
      result.store_proj_bdu_test_restriction
  in
  error
