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

let print_agents_site parameter error handler_kappa x =
  let (_, agent_type, site_type) = x in
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
  error, (agent_string, site_string)

let print_pair_agents_site parameter error handler_kappa (x, y) =
  let error, (agent_string, site_string) =
    print_agents_site parameter error handler_kappa x
  in
  let error, (agent_string', site_string') =
    print_agents_site parameter error handler_kappa y
  in
  error, ((agent_string, site_string),
          (agent_string', site_string'))

let print_pair_agents_site_state parameter error handler_kappa (x, y) =
  let error, (agent_string, site_string, state_string) =
    print_agents_site_state parameter error handler_kappa x
  in
  let error, (agent_string', site_string', state_string') =
    print_agents_site_state parameter error handler_kappa y
  in
  error, ((agent_string, site_string, state_string),
          (agent_string', site_string', state_string'))

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

let print_pair_agents_sites_states parameter error handler_kappa log (x, y) =
let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
let error,
    ((agent_string, site_string, state_string),
     (agent_string', site_string', state_string')) = print_pair_agents_site_state parameter error handler_kappa
    ((agent_id,agent_type,site_type, state),                                                                                                              (agent_id', agent_type', site_type', state'))
in
let error, site_string2 =
  try
    Handler.string_of_site parameter error handler_kappa
      agent_type site_type2
  with
    _ -> warn parameter error (Some "line 30") Exit
           (Ckappa_sig.string_of_site_name site_type2)
in
let error, state_string2 =
  try
    Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type site_type2 state2
  with
    _ -> warn parameter error (Some "line 38") Exit
           (Ckappa_sig.string_of_state_index state2)
in
let error, site_string2' =
  try
    Handler.string_of_site parameter error handler_kappa
      agent_type' site_type2'
  with
    _ -> warn parameter error (Some "line 30") Exit
           (Ckappa_sig.string_of_site_name site_type2')
in
let error, state_string2' =
  try
    Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type' site_type2' state2'
  with
    _ -> warn parameter error (Some "line 38") Exit
           (Ckappa_sig.string_of_state_index state2')
in
error, ((agent_string, site_string, site_string2, state_string, state_string2),(agent_string', site_string', site_string2', state_string', state_string2'))

let print_pair_agents_sites parameter error handler_kappa log (x, y) =
  let (agent_id, agent_type, site_type, site_type2) = x in
  let (agent_id', agent_type', site_type', site_type2') = y in
  let error,
      ((agent_string, site_string),
       (agent_string', site_string')) =
    print_pair_agents_site parameter error handler_kappa
      ((agent_id,agent_type,site_type),                                                                                                              (agent_id', agent_type', site_type'))
  in
  let error, site_string2 =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type site_type2
    with
      _ -> warn parameter error (Some "line 30") Exit
             (Ckappa_sig.string_of_site_name site_type2)
  in
  let error, site_string2' =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type' site_type2'
    with
      _ -> warn parameter error (Some "line 30") Exit
             (Ckappa_sig.string_of_site_name site_type2')
  in
  error, ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2'))

let print_site_accross_bonds_domain parameter error handler_kappa log store_result =
  Loggers.fprintf (Remanent_parameters.get_logger parameter) "-Result sites:\n";
Ckappa_sig.Rule_map_and_set.Map.iter
  (fun rule_id set ->
     Loggers.fprintf (Remanent_parameters.get_logger parameter)
       "rule_id:%i\n"
       (Ckappa_sig.int_of_rule_id rule_id);
     let _ =
       Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.iter (fun (x, y) ->
           let (agent_id, agent_type, site_type2, state2) = x in
           let (agent_id', agent_type', site_type2', state2') = y in
           let error,
               ((agent_string, site_string, state_string),
                (agent_string', site_string', state_string')) = print_pair_agents_site_state parameter error handler_kappa
               ((agent_id,agent_type, site_type2, state2),                                                                                                              (agent_id',agent_type', site_type2', state2'))
           in
           Loggers.fprintf (Remanent_parameters.get_logger parameter)
             "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s -> agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
        (Ckappa_sig.int_of_agent_id agent_id)
        (Ckappa_sig.int_of_agent_name agent_type)
        agent_string
        (Ckappa_sig.int_of_site_name site_type2)
        site_string
        (Ckappa_sig.int_of_state_index state2)
        state_string

        (Ckappa_sig.int_of_agent_id agent_id')
        (Ckappa_sig.int_of_agent_name agent_type')
        agent_string'
        (Ckappa_sig.int_of_site_name site_type2')
        site_string'
        (Ckappa_sig.int_of_state_index state2')
        state_string'
         ) set
     in
     ()
  ) store_result

let print_first_site_y_modified parameter error handler_kappa log store_result =
  Loggers.fprintf log "-The second site of the first agent can be modified:\n";
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id set ->
       Loggers.fprintf log "rule_id:%i\n"
         (Ckappa_sig.int_of_rule_id rule_id);
       Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.iter (fun (x, y) ->
           let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
           let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
           let error, ((agent_string, site_string, site_string2, state_string, state_string2),(agent_string', site_string', site_string2', state_string', state_string2')) =
             print_pair_agents_sites_states parameter error handler_kappa log (x, y)
            in
            let () =
              Loggers.fprintf log
                "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
                (Ckappa_sig.int_of_agent_id agent_id)
                (Ckappa_sig.int_of_agent_name agent_type)
                agent_string
                (Ckappa_sig.int_of_site_name site_type2)
                site_string2
                (Ckappa_sig.int_of_state_index state2)
                state_string2
            in
            let () =
            Loggers.fprintf log
              "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s->agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              (Ckappa_sig.int_of_agent_name agent_type)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_string
              (Ckappa_sig.int_of_state_index state)
              state_string
              (Ckappa_sig.int_of_site_name site_type2)
              site_string2
              (Ckappa_sig.int_of_state_index state2)
              state_string2
(**)
(Ckappa_sig.int_of_agent_id agent_id')
(Ckappa_sig.int_of_agent_name agent_type')
agent_string'
(Ckappa_sig.int_of_site_name site_type')
site_string'
(Ckappa_sig.int_of_state_index state')
state_string'
(Ckappa_sig.int_of_site_name site_type2')
site_string2'
(Ckappa_sig.int_of_state_index state2')
state_string2'
            in
            ()
         ) set
    ) store_result

    let print_snd_site_t_modified parameter error handler_kappa log store_result =
      Loggers.fprintf log "-The second site of the second agent can be modified:\n";
      Ckappa_sig.Rule_map_and_set.Map.iter
        (fun rule_id set ->
           Loggers.fprintf log "rule_id:%i\n"
             (Ckappa_sig.int_of_rule_id rule_id);
           Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.iter (fun (x, y) ->
               let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
               let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
               let error, ((agent_string, site_string, site_string2, state_string, state_string2),(agent_string', site_string', site_string2', state_string', state_string2')) =
                 print_pair_agents_sites_states parameter error handler_kappa log (x, y)
                in
                let () =
                  Loggers.fprintf log
                    "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
                    (Ckappa_sig.int_of_agent_id agent_id')
                    (Ckappa_sig.int_of_agent_name agent_type')
                    agent_string'
                    (Ckappa_sig.int_of_site_name site_type2')
                    site_string2'
                    (Ckappa_sig.int_of_state_index state2')
                    state_string2'
                in
                let () =
                Loggers.fprintf log
                  "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s->agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s\n"
                  (Ckappa_sig.int_of_agent_id agent_id)
                  (Ckappa_sig.int_of_agent_name agent_type)
                  agent_string
                  (Ckappa_sig.int_of_site_name site_type)
                  site_string
                  (Ckappa_sig.int_of_state_index state)
                  state_string
                  (Ckappa_sig.int_of_site_name site_type2)
                  site_string2
                  (Ckappa_sig.int_of_state_index state2)
                  state_string2
    (**)
    (Ckappa_sig.int_of_agent_id agent_id')
    (Ckappa_sig.int_of_agent_name agent_type')
    agent_string'
    (Ckappa_sig.int_of_site_name site_type')
    site_string'
    (Ckappa_sig.int_of_state_index state')
    state_string'
    (Ckappa_sig.int_of_site_name site_type2')
    site_string2'
    (Ckappa_sig.int_of_state_index state2')
    state_string2'
                in
                ()
             ) set
        ) store_result

let print_first_site_is_bound parameter error handler_kappa log store_result =
Loggers.fprintf log "-The first site can be bound:\n";
Ckappa_sig.Rule_map_and_set.Map.iter
  (fun rule_id set ->
     Loggers.fprintf log "rule_id:%i\n"
       (Ckappa_sig.int_of_rule_id rule_id);
     Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.iter (fun (x, y) ->
         let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
         let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
         let _, ((agent_string, site_string, site_string2, state_string, state_string2),(agent_string', site_string', site_string2', state_string', state_string2')) =
           print_pair_agents_sites_states parameter error handler_kappa log (x, y)
          in
          let () =
            Loggers.fprintf log
              "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              (Ckappa_sig.int_of_agent_name agent_type)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_string
              (Ckappa_sig.int_of_state_index state)
              state_string
          in
          let () =
            Loggers.fprintf log
              "agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s->agent_id:%i:agent_type:%i:%s:site_type:%i:%s:state:%i:%s:site_type:%i:%s:state:%i:%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              (Ckappa_sig.int_of_agent_name agent_type)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_string
              (Ckappa_sig.int_of_state_index state)
              state_string
              (Ckappa_sig.int_of_site_name site_type2)
              site_string2
              (Ckappa_sig.int_of_state_index state2)
              state_string2
(**)
(Ckappa_sig.int_of_agent_id agent_id')
(Ckappa_sig.int_of_agent_name agent_type')
agent_string'
(Ckappa_sig.int_of_site_name site_type')
site_string'
(Ckappa_sig.int_of_state_index state')
state_string'
(Ckappa_sig.int_of_site_name site_type2')
site_string2'
(Ckappa_sig.int_of_state_index state2')
state_string2'
          in
          ()
       ) set
  ) store_result

(*------------------------------------------------------------*)
(*dynamic information*)
(*
let print_relation_mvbdu parameter error handler_kappa log store_result =
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.iter
    (fun (x, y) mvbdu ->
       Loggers.fprintf log "-Dynamic information: the relation between the state of the second sites when both agents are connected via its first site:\n";
       let () = print_tuple parameter error handler_kappa log (x, y) in
       let () = Ckappa_sig.Views_bdu.print parameter mvbdu in
       ()
    ) store_result

let print_range_site_first_agent parameter error handler_kappa log store_result =
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.iter
    (fun (x, y) mvbdu ->
       Loggers.fprintf log "The range of the second site of the first agent:\n";
       let () = print_tuple parameter error handler_kappa log (x, y) in
       let () = Ckappa_sig.Views_bdu.print parameter mvbdu in
       ()
    ) store_result

let print_range_site_second_agent parameter error handler_kappa log store_result =
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Map.iter
    (fun (x, y) mvbdu ->
       Loggers.fprintf log "The range of the second site of the second agent:\n";
       let () = print_tuple parameter error handler_kappa log (x, y) in
       let () = Ckappa_sig.Views_bdu.print parameter mvbdu in
       ()
    ) store_result*)

let print_implicit_rule parameter error handler_kappa log store_result =
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id set ->
       Loggers.fprintf log "Implicit static:\n";
       Loggers.fprintf log "rule_id:%i\n"
         (Ckappa_sig.int_of_rule_id rule_id)
    ) store_result

(*let print_tuple_has_first_site_bound_snd_site_different parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log "* A set of tuple that has the first site can be bound and the second site is different than the first one:\n";
  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
    (fun (x, y) ->
       let (_, _, site_type, site_type2) = x in
       let (_, _, site_type', site_type2') = y in
       let _,
           ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
         print_pair_agents_sites parameter error handler_kappa log
           (x, y)
       in
       let  () =
         Loggers.fprintf log
           "(%s,%i:%s,%i:%s, %s,%i:%s,%i:%s)\n"
           agent_string
           (Ckappa_sig.int_of_site_name site_type)
           site_string
           (Ckappa_sig.int_of_site_name site_type2)
           site_string2
           agent_string'
           (Ckappa_sig.int_of_site_name site_type')
           site_string'
           (Ckappa_sig.int_of_site_name site_type2')
           site_string2'
       in
       ()
    ) store_result*)

(****************************************************************)
(*PRINT*)
(****************************************************************)

let print_views_rhs parameter error handler_kappa log store_result =
  Loggers.fprintf log
    "\n* Views on the right hand site rule:\n";
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
                "rule_id:%i: %s(%i:%s:%s:%i)\n"
                (Ckappa_sig.int_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.int_of_site_name site_type)
                site_string
                state_string
                (Ckappa_sig.int_of_state_index state)
            in ()
         ) set
    ) store_result

(****************************************************************)

let print_bonds_rhs parameter error handler_kappa log store_result =
Loggers.fprintf log "------------------------------------------------------------\n";
Loggers.fprintf log
  "* Rule that has site can be bound:\n";
Ckappa_sig.Rule_map_and_set.Map.iter
  (fun rule_id set ->
     Site_accross_bonds_domain_type.PairAgentsSiteState_map_and_set.Set.iter
       (fun (x, y) ->
          let (_, _, site_type, state) = x in
          let (_, _, site_type', state') = y in
          let error, ((agent_string, site_string, state_string),
                      (agent_string', site_string', state_string')) =
            print_pair_agents_site_state parameter error handler_kappa (x, y)
          in
          let () =
            Loggers.fprintf log
              "rule_id:%i: %s(%i:%s:%s:%i), %s(%i:%s:%s:%i)\n"
              (Ckappa_sig.int_of_rule_id rule_id)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_string
              state_string
              (Ckappa_sig.int_of_state_index state)
              agent_string'
              (Ckappa_sig.int_of_site_name site_type')
              site_string'
              state_string'
              (Ckappa_sig.int_of_state_index state')
          in
          ()
       ) set
  ) store_result

(****************************************************************)

let print_modified_map parameter error handler_kappa log store_result =
Loggers.fprintf log "------------------------------------------------------------\n";
Loggers.fprintf log
  "* Rule that has site can be modified:\n";
Ckappa_sig.Rule_map_and_set.Map.iter
  (fun rule_id set ->
     Site_accross_bonds_domain_type.AgentsSiteState_map_and_set.Set.iter
       (fun x->
          let (_, _, site_type, state) = x in
          let error, (agent_string, site_string, state_string) =
            print_agents_site_state parameter error handler_kappa x
          in
          let () =
            Loggers.fprintf log
              "rule_id:%i: %s(%i:%s:%s:%i)\n"
              (Ckappa_sig.int_of_rule_id rule_id)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_string
              state_string
              (Ckappa_sig.int_of_state_index state)
          in
          ()
       ) set
  ) store_result

(****************************************************************)

let print_created_bond parameter error handler_kappa log store_result =
Loggers.fprintf log "------------------------------------------------------------\n";
Loggers.fprintf log
  "* Rule that created a site can be bound:\n";
Ckappa_sig.Rule_map_and_set.Map.iter
  (fun rule_id set ->
     Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
       (fun (x, y) ->
       let (_, _, site_type, site_type2) = x in
       let (_, _, site_type', site_type2') = y in
       let _,
           ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
         print_pair_agents_sites parameter error handler_kappa log
           (x, y)
       in
       let () =
         Loggers.fprintf log
           "rule_id:%i: %s(%i:%s,%i:%s), %s(%i:%s,%i:%s)\n"
           (Ckappa_sig.int_of_rule_id rule_id)
           agent_string
           (Ckappa_sig.int_of_site_name site_type)
           site_string
           (Ckappa_sig.int_of_site_name site_type2)
           site_string2
           agent_string'
           (Ckappa_sig.int_of_site_name site_type')
           site_string'
           (Ckappa_sig.int_of_site_name site_type2')
           site_string2'
       in
       ()
       ) set
  ) store_result

(****************************************************************)

let print_modified_internal_state_bond parameter error
    handler_kappa log store_result =
    Loggers.fprintf log "------------------------------------------------------------\n";
    Loggers.fprintf log
      "* Rule that has the first site can be bound, and the second site has internal state can be modified:\n";
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id set ->
         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
           (fun (x, y) ->
           let (_, _, site_type, site_type2) = x in
           let (_, _, site_type', site_type2') = y in
           let _,
               ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
             print_pair_agents_sites parameter error handler_kappa log
               (x, y)
           in
           let () =
             Loggers.fprintf log
               "rule_id:%i: %s(%i:%s,%i:%s), %s(%i:%s,%i:%s)\n"
               (Ckappa_sig.int_of_rule_id rule_id)
               agent_string
               (Ckappa_sig.int_of_site_name site_type)
               site_string
               (Ckappa_sig.int_of_site_name site_type2)
               site_string2
               agent_string'
               (Ckappa_sig.int_of_site_name site_type')
               site_string'
               (Ckappa_sig.int_of_site_name site_type2')
               site_string2'
           in
           ()
           ) set
      ) store_result

(****************************************************************)

let print_explicit_static parameter error
    handler_kappa log store_result =
    Loggers.fprintf log "------------------------------------------------------------\n";
    Loggers.fprintf log
      "* Explicit static information:\n";
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id set ->
         Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
           (fun (x, y) ->
           let (_, _, site_type, site_type2) = x in
           let (_, _, site_type', site_type2') = y in
           let _,
               ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
             print_pair_agents_sites parameter error handler_kappa log
               (x, y)
           in
           let () =
             Loggers.fprintf log
               "rule_id:%i: %s(%i:%s,%i:%s), %s(%i:%s,%i:%s)\n"
               (Ckappa_sig.int_of_rule_id rule_id)
               agent_string
               (Ckappa_sig.int_of_site_name site_type)
               site_string
               (Ckappa_sig.int_of_site_name site_type2)
               site_string2
               agent_string'
               (Ckappa_sig.int_of_site_name site_type')
               site_string'
               (Ckappa_sig.int_of_site_name site_type2')
               site_string2'
           in
           ()
           ) set
      ) store_result

(****************************************************************)

let print_implicit_static parameter error
    handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Implicit static information:\n";
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id set ->
       Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
         (fun (x, y) ->
            let (_, _, site_type, site_type2) = x in
            let (_, _, site_type', site_type2') = y in
            let _,
                ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
              print_pair_agents_sites parameter error handler_kappa log
                (x, y)
            in
            let () =
              Loggers.fprintf log
                "rule_id:%i: %s(%i:%s,%i:%s), %s(%i:%s,%i:%s)\n"
                (Ckappa_sig.int_of_rule_id rule_id)
                agent_string
                (Ckappa_sig.int_of_site_name site_type)
                site_string
                (Ckappa_sig.int_of_site_name site_type2)
                site_string2
                agent_string'
                (Ckappa_sig.int_of_site_name site_type')
                site_string'
                (Ckappa_sig.int_of_site_name site_type2')
                site_string2'
            in
            ()
         ) set
    ) store_result

(****************************************************************)

let print_question_marks_rhs parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log
    "* Question marks on the rhs:\n";
  Ckappa_sig.Rule_map_and_set.Map.iter
    (fun rule_id set ->
       Site_accross_bonds_domain_type.AgentsSites_map_and_set.Set.iter (fun (agent_id, agent_type, site_type, site_type') ->
           let error, (agent_string, site_string) =
             print_agents_site parameter error handler_kappa (agent_id, agent_type, site_type)
           in
           let error, site_string' =
             try
               Handler.string_of_site parameter error handler_kappa
                 agent_type site_type'
             with
               _ -> warn parameter error (Some "line 30") Exit
                      (Ckappa_sig.string_of_site_name site_type')
           in
           let () =
             Loggers.fprintf log
               "rule_id:%i: %s(%i:%s,%i:%s)\n"
               (Ckappa_sig.int_of_rule_id rule_id)
               agent_string
               (Ckappa_sig.int_of_site_name site_type)
               site_string
               (Ckappa_sig.int_of_site_name site_type')
               site_string'
           in
           ()
         ) set
    ) store_result

(****************************************************************)

let print_tuple parameter error handler_kappa log (x, y) =
  let (agent_id, agent_type, site_type, site_type2, state, state2) = x in
  let (agent_id', agent_type', site_type', site_type2', state', state2') = y in
  let _, ((agent_string, site_string, site_string2, state_string, state_string2),(agent_string', site_string', site_string2', state_string', state_string2')) =
    print_pair_agents_sites_states parameter error handler_kappa log (x, y)
  in
  Loggers.fprintf log
    "%s(%i:%s,%i:%s), %s(%i:%s,%i:%s) -> %s, %s\n"
    agent_string
    (Ckappa_sig.int_of_site_name site_type)
    site_string
    (*(Ckappa_sig.int_of_state_index state)*)
    (Ckappa_sig.int_of_site_name site_type2)
    site_string2
    (*(Ckappa_sig.int_of_state_index state2)*)
    (*state_string*)
    (**)
    agent_string'
    (Ckappa_sig.int_of_site_name site_type')
    site_string'
    (*(Ckappa_sig.int_of_state_index state')*)
    (Ckappa_sig.int_of_site_name site_type2')
    site_string2'
    (*(Ckappa_sig.int_of_state_index state2')*)
    (*state_string'*)
    state_string2
    state_string2'

let print_init parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";  Loggers.fprintf log "* Inititial states:\n";
  Site_accross_bonds_domain_type.PairAgentsSitesStates_map_and_set.Set.iter
    (fun (x, y) ->
       print_tuple parameter error handler_kappa log (x, y)
    ) store_result

(****************************************************************)

let print_tuple_pair parameter error handler_kappa log store_result =
  Loggers.fprintf log "------------------------------------------------------------\n";
  Loggers.fprintf log "A set of tuple\n ";
  Site_accross_bonds_domain_type.PairAgentsSites_map_and_set.Set.iter
    (fun (x, y) ->
    let (_, _, site_type, site_type2) = x in
    let (_, _, site_type', site_type2') = y in
    let _,
        ((agent_string, site_string, site_string2),(agent_string', site_string', site_string2')) =
      print_pair_agents_sites parameter error handler_kappa log
        (x, y)
    in
    let  () =
      Loggers.fprintf log
        "(%s,%i:%s,%i:%s, %s,%i:%s,%i:%s)\n"
        agent_string
        (Ckappa_sig.int_of_site_name site_type)
        site_string
        (Ckappa_sig.int_of_site_name site_type2)
        site_string2
        agent_string'
        (Ckappa_sig.int_of_site_name site_type')
        site_string'
        (Ckappa_sig.int_of_site_name site_type2')
        site_string2'
    in
    ()
    ) store_result

(****************************************************************)
