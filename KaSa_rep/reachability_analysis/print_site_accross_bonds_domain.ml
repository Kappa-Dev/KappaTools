(**
   * site_accross_bonds_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
 Exception.warn parameters mh (Some "Site accross bonds") message exn (fun () -> default)

let local_trace = false

(*******************************************************************)
(*PRINT*)

let print_agents_site_state parameter error handler_kappa x =
let (agent_id, agent_type, site_type, state) = x in
let error, agent_string =
  try
    Handler.string_of_agent parameter error handler_kappa agent_type
  with
    _ -> warn parameter error (Some "line 23") Exit (Ckappa_sig.string_of_agent_name agent_type)
in
let error, site_string =
  try
    Handler.string_of_site parameter error handler_kappa
      agent_type site_type
  with
    _ -> warn parameter error (Some "line 30") Exit
           (Ckappa_sig.string_of_site_name site_type)
in
let error, state_string =
  try
    Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type site_type state
  with
    _ -> warn parameter error (Some "line 38") Exit
           (Ckappa_sig.string_of_state_index state)
in
error, (agent_string, site_string, state_string)

let print_pair_agents_site_state parameter error handler_kappa (x, y) =
  let error, (agent_string, site_string, state_string) =
    print_agents_site_state parameter error handler_kappa x
  in
  let error, (agent_string', site_string', state_string') =
    print_agents_site_state parameter error handler_kappa y
  in
  error, ((agent_string, site_string, state_string),
          (agent_string', site_string', state_string'))

let print_views_rhs parameter error handler_kappa log store_result =
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id set ->
       Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.iter
         (fun (agent_id, agent_type, site_type, state) ->
            let error, (agent_string, site_string, state_string) =
              print_agents_site_state parameter error handler_kappa
                (agent_id, agent_type, site_type, state)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%i:agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
                (Ckappa_sig.int_of_rule_id rule_id)
                (Ckappa_sig.int_of_agent_id agent_id)
                (Ckappa_sig.int_of_agent_name agent_type)
                agent_string
                (Ckappa_sig.int_of_site_name site_type)
                site_string
                (Ckappa_sig.int_of_state_index state)
                state_string
            in ()
         ) set
    ) store_result

let print_pair_sites_aux parameter error handler_kappa log store_result =
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id list ->
       List.iter (fun
                   ((agent_id,agent_type,site_type, state),
                   (agent_id',agent_type',site_type', state')
                   )
                 (*(agent_id',agent_type', site_type', state')*)->
           let () =
             Loggers.fprintf log
               "rule_id:%i:agent_id:%i:agent_type:%i:site_type:%i:state:%i=>>agent_id:%i:agent_type:%i:site_type:%i:state:%i\n"
               (Ckappa_sig.int_of_rule_id rule_id)
               (Ckappa_sig.int_of_agent_id agent_id)
               (Ckappa_sig.int_of_agent_name agent_type)
               (Ckappa_sig.int_of_site_name site_type)
               (Ckappa_sig.int_of_state_index state)

               (Ckappa_sig.int_of_agent_id agent_id')
               (Ckappa_sig.int_of_agent_name agent_type')
               (Ckappa_sig.int_of_site_name site_type')
               (Ckappa_sig.int_of_state_index state')

           in
           (*let () =
             Loggers.fprintf log
               "rule_id:%i:agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s; agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
               (Ckappa_sig.int_of_rule_id rule_id)
               (Ckappa_sig.int_of_agent_id agent_id)
               (Ckappa_sig.int_of_agent_name agent_type)
               agent_string
               (Ckappa_sig.int_of_site_name site_type)
               site_string
               (Ckappa_sig.int_of_state_index state)
               state_string
               (Ckappa_sig.int_of_agent_id agent_id')
               (Ckappa_sig.int_of_agent_name agent_type')
               agent_string'
               (Ckappa_sig.int_of_site_name site_type')
               site_string'
               (Ckappa_sig.int_of_state_index state')
               state_string'
           in*)
           ()
         )
         list
    ) store_result

let print_pair_sites parameter error handler_kappa log store_result =
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id list ->
       List.iter (fun ((agent_id,agent_type,site_type, site_type', state, state') ,
                      (agent_id1, agent_type1, site_type1, site_type1', state1, state1')) ->
               let error,
                   ((agent_string, site_string, state_string),
                    (agent_string1, site_string1, state_string1)) = print_pair_agents_site_state parameter error handler_kappa
                   ((agent_id,agent_type,site_type, state),                                                                                                              (agent_id1,agent_type1, site_type1, state1))
               in
               let error, site_string' =
                 try
                   Handler.string_of_site parameter error handler_kappa
                     agent_type site_type'
                 with
                   _ -> warn parameter error (Some "line 30") Exit
                          (Ckappa_sig.string_of_site_name site_type')
               in
               let error, state_string' =
                 try
                   Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type site_type' state'
                 with
                   _ -> warn parameter error (Some "line 38") Exit
                          (Ckappa_sig.string_of_state_index state')
               in
               let error, site_string1' =
                 try
                   Handler.string_of_site parameter error handler_kappa
                     agent_type1 site_type1'
                 with
                   _ -> warn parameter error (Some "line 30") Exit
                          (Ckappa_sig.string_of_site_name site_type1')
               in
               let error, state_string1' =
                 try
                   Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type1 site_type1' state1'
                 with
                   _ -> warn parameter error (Some "line 38") Exit
                          (Ckappa_sig.string_of_state_index state1')
               in
               let () =
                 Loggers.fprintf log
                   "rule_id:%i:agent_id:%i:agent_type:%i:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s\n; agent_id:%i:agent_type:%i:%s:site_type:%i:%s:site_type:%i:%s:state:%i:%s:state:%i:%s\n"
                   (Ckappa_sig.int_of_rule_id rule_id)
                   (Ckappa_sig.int_of_agent_id agent_id)
                   (Ckappa_sig.int_of_agent_name agent_type)
                   agent_string
                   (Ckappa_sig.int_of_site_name site_type)
                   site_string
                   (Ckappa_sig.int_of_site_name site_type')
                   site_string'
                   (Ckappa_sig.int_of_state_index state)
                   state_string
                   (Ckappa_sig.int_of_state_index state')
                   state_string'

                   (Ckappa_sig.int_of_agent_id agent_id1)
                   (Ckappa_sig.int_of_agent_name agent_type1)
                   agent_string1
                   (Ckappa_sig.int_of_site_name site_type')
                   site_string'
                   (Ckappa_sig.int_of_site_name site_type1')
                   site_string1'
                   (Ckappa_sig.int_of_state_index state')
                   state_string'
                   (Ckappa_sig.int_of_state_index state1')
                   state_string1'
               in
               ()
             )
             list
        ) store_result
