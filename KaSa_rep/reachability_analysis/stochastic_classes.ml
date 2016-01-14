(**
  * stochastic_classes.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of March
  * Last modification: 
  * 
  * Compute the relations between sites in an agent.
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic_classes") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)   
(*TYPE*)

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif

type stochastic_class =
  {
    stochastic_class : int list AgentMap.t
  }

(************************************************************************************)   
(*RULE*)
            
let scan_rule parameter error handler rule classes =
  let error, store_result = AgentMap.create parameter error 0 in
  (*Compute the stochastic class in the case there is a new agent is created in the rhs*)
  let error, stochastic_class_rhs =
    List.fold_left
      (fun (error, store_result) (agent_id, agent_type) ->
	let error, agent =
          AgentMap.get
	    parameter
	    error
	    agent_id
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
	in
	match agent with
	  | None
	  | Some Cckappa_sig.Unknown_agent _ 
          | Some Cckappa_sig.Ghost -> error, store_result
	  | Some Cckappa_sig.Dead_agent (agent,_,_,_) 
          | Some Cckappa_sig.Agent agent ->
            let error, site_list = 
              Cckappa_sig.Site_map_and_set.Map.fold
                (fun site _ (error, current_list) ->
                  let site_list = site :: current_list in
                  error, site_list
                ) agent.Cckappa_sig.agent_interface (error, [])
            in
            let error, old_list =
              match AgentMap.unsafe_get parameter error agent_type store_result with
              | error, None -> error, []
              | error, Some l -> error, l
            in
            let new_list = List.concat [site_list; old_list] in
            let error, store_result =
              AgentMap.set
                parameter
                error
                agent_type
                new_list
                store_result
            in
            error, store_result
      )(error, store_result) rule.Cckappa_sig.actions.Cckappa_sig.creation
  in
  (*compute the stochastic class *)
  let error, stochastic_classes =
    Int_storage.Quick_Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error agent_id agent store_result ->
       match agent with
       | Cckappa_sig.Unknown_agent _  
       | Cckappa_sig.Ghost -> error, store_result
       | Cckappa_sig.Dead_agent (agent,_,_,_) 
       | Cckappa_sig.Agent agent ->
	  let agent_type = agent.Cckappa_sig.agent_name in
          let error, site_list =
            Cckappa_sig.Site_map_and_set.Map.fold
              (fun site _ (error, current_list) ->
                let site_list = site :: current_list in
                error, site_list
              ) agent.Cckappa_sig.agent_interface (error, [])
          in
          let error, old_list =
            match AgentMap.unsafe_get parameter error agent_type store_result with
            | error, None -> error, []
            | error, Some l -> error, l
          in
          let new_list = List.concat [site_list; old_list] in
          let error, store_result =
            AgentMap.set
              parameter
              error
              agent_type
              new_list
              store_result
          in
          error, store_result            
      ) rule.Cckappa_sig.rule_lhs.Cckappa_sig.views stochastic_class_rhs
  in
  error,
  {
    stochastic_class = stochastic_classes;
  }

(************************************************************************************)
(*RULES*)

(*------------------------------------------------------------------------------*)
(*return a number of site in each agent.
  For example: A(x,y,z,t) => the number of site of A is: 4*)

let get_nsites parameter error key handler =
  let error, get_nsites =
    Int_storage.Nearly_inf_Imperatif.unsafe_get
      parameter
      error
      key
      handler.Cckappa_sig.sites
  in
  let error, sites_dic =
    match get_nsites with
    | None -> warn parameter error (Some "line 143") Exit
                   (Ckappa_sig.Dictionary_of_sites.init())
    | Some dic -> error, dic
  in                  
  let error, nsites =
    Ckappa_sig.Dictionary_of_sites.last_entry
      parameter
      error
      sites_dic
  in nsites + 1

(*------------------------------------------------------------------------------*)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let nagents = handler.Cckappa_sig.nagents in
  let error, init_stochastic_class = AgentMap.create parameter error nagents in
  let error, init                  = AgentMap.create parameter error 0 in
  let init_stochastic =
    {
      stochastic_class = init_stochastic_class
    }
  in
  (*------------------------------------------------------------------------------*)
  let error, stochastic_class =
    Int_storage.Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule stochastic_class ->
       (*------------------------------------------------------------------------*)
       let error, map =
         scan_rule
           parameter
           error
           handler
           rule.Cckappa_sig.e_rule_c_rule
           init_stochastic
       in
       (*------------------------------------------------------------------------*)
       let error, store_result =
         AgentMap.fold
           parameter error
           (fun parameter error agent_type sites_list store_result ->
            let nsites = 
              get_nsites
                parameter 
                error 
                agent_type
                handler 
            in
            match sites_list with
            | [] | [_] -> error, store_result
            | _ ->
              (*getting an array in the old_result*)
               let error, get_array =
                 AgentMap.unsafe_get
                   parameter
                   error
                   agent_type
                   store_result
               in
               let array =
                 match get_array with
                 | None -> Union_find.create nsites
                 | Some a -> a
               in
               (*compute the union for the list of site*)
               let error, union_array =
                 Union_find.union_list
                   parameter
                   error
                   array
                   sites_list
               in
               (*store*)
               let error, store_result =
                 AgentMap.set
                   parameter
                   error
                   agent_type
                   union_array
                   store_result
               in error, store_result
           ) map.stochastic_class stochastic_class
       in error, store_result
      ) rules init
  in error, stochastic_class

(************************************************************************************)
(*PRINT*)

let sprintf_array parameter error handler agent_type array =
  let acc = ref "[|" in
  Array.iteri (fun i site_type ->
    let error, site_string = 
      try
        Handler.string_of_site parameter error handler agent_type site_type
      with
        _ -> warn parameter error (Some "line 231") Exit (string_of_int site_type)
    in
    acc := !acc ^
      if i <> 0
      then Printf.sprintf "; %d:%s" site_type site_string
      else Printf.sprintf "%d:%s" site_type site_string
  ) array;
  !acc ^ "|]"
  
let print_array parameter error handler agent_type array =
  let output = sprintf_array parameter error handler agent_type array in
  Printf.fprintf stdout "%s\n" output

let print_stochastic_class parameter error handler result =
  Int_storage.Quick_Nearly_inf_Imperatif.iter
    parameter
    error
    (fun parameter error agent_type array_site_type ->
      let _ =
        if Remanent_parameters.get_do_stochastic_flow_of_information parameter
        then
          let parameter =
            Remanent_parameters.update_prefix parameter ""
          in
          begin
            if Remanent_parameters.get_trace parameter
            then
              let _ =
                let error, agent_string =
                  try
                    Handler.string_of_agent parameter error handler agent_type
                  with
                    _ -> warn parameter error (Some "line 263") Exit 
                      (string_of_int agent_type)
                in
                Printf.fprintf stdout "agent_type:%i:%s\n" agent_type agent_string
              in
              print_string "site_type:";
              print_array parameter error handler agent_type array_site_type
            else ()
          end
      in
      error)
    result

(************************************************************************************)     
(*MAIN*)
   
let stochastic_classes parameter error handler cc_compil =
  let error, result = 
    scan_rule_set parameter error handler cc_compil.Cckappa_sig.rules
  in
  let _ = print_stochastic_class parameter error handler result in
  error, result
