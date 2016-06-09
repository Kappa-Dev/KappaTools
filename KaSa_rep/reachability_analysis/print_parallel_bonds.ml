(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to detect whether when two sites of an agent are bound, they must be bound to the same agent.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Print parallel bonds") message exn
    (fun () -> default)

let local_trace = false

(*******************************************************************)
(*PRINT*)

let print_parallel_pair parameter error handler_kappa (x, y) =
  let (agent_id, agent_type, site_type1, site_type2, state1, state2) = x in
  let (agent_id', agent_type', site_type1', site_type2', state1', state2') = y in
  let error, site_type1_string =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type site_type1
    with
      _ -> warn parameter error (Some "line 1017") Exit
        (Ckappa_sig.string_of_site_name site_type1)
  in
  let error, state1_string =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type site_type1 state1
    with
      _ -> warn parameter error (Some "line 1025") Exit
        (Ckappa_sig.string_of_state_index state1)
  in
  let error, site_type2_string =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type site_type2
    with
      _ -> warn parameter error (Some "line 1033") Exit
        (Ckappa_sig.string_of_site_name site_type2)
  in
  let error, state2_string =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type site_type2 state2
    with
      _ -> warn parameter error (Some "line 1041") Exit
        (Ckappa_sig.string_of_state_index state2)
  in
  let error, state1_string' =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type' site_type1' state1'
    with
      _ -> warn parameter error (Some "line 1049") Exit
        (Ckappa_sig.string_of_state_index state1')
  in
  let error, site_type1_string' =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type' site_type1'
    with
      _ -> warn parameter error (Some "line 1057") Exit
        (Ckappa_sig.string_of_site_name site_type1')
  in
  let error, site_type2_string' =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type' site_type2'
    with
      _ -> warn parameter error (Some "line 1065") Exit
        (Ckappa_sig.string_of_site_name site_type2')
  in
  let error, state2_string' =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type' site_type2' state2'
    with
      _ -> warn parameter error (Some "line 1073") Exit
        (Ckappa_sig.string_of_state_index state2')
  in
  error, ((site_type1_string, site_type2_string, state1_string, state2_string),
          (site_type1_string', site_type2_string', state1_string', state2_string'))

(**************************************************************************)

let print_pair parameter error handler_kappa (x, y) =
  let (agent_id, agent_type, site_type, state) = x in
  let (agent_id', agent_type', site_type', state') = y in
  let error, site_type_string =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type site_type
    with
      _ -> warn parameter error (Some "line 1088") Exit
        (Ckappa_sig.string_of_site_name site_type)
  in
  let error, site_type_string' =
    try
      Handler.string_of_site parameter error handler_kappa
        agent_type' site_type'
    with
      _ -> warn parameter error (Some "line 1096") Exit
        (Ckappa_sig.string_of_site_name site_type')
  in
  let error, state_string =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type site_type state
    with
      _ -> warn parameter error (Some "line 1103") Exit
        (Ckappa_sig.string_of_state_index state)
  in
  let error, state_string' =
    try
      Handler.string_of_state_fully_deciphered parameter error handler_kappa
	agent_type' site_type' state'
    with
      _ -> warn parameter error (Some "line 1112") Exit
        (Ckappa_sig.string_of_state_index state')
  in
  error, (site_type_string, site_type_string', state_string, state_string')

(**************************************************************************)

let print_parallel_bonds parameter handler_kappa static dynamic error store_result =
  let _ =
    if Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.is_empty store_result
    then Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "empty set\n\n"
    else
      Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Set.iter
        (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2),
              (agent_id', agent_type', site_type1', site_type2', state1', state2')) ->
          let error, ((site_type1_string, site_type2_string, state1_string, state2_string),
                      (site_type1_string', site_type2_string', state1_string', state2_string')) =
            print_parallel_pair
              parameter error handler_kappa
              ((agent_id, agent_type, site_type1, site_type2, state1, state2),
               (agent_id', agent_type', site_type1', site_type2', state1', state2'))
          in
          let error, agent_string =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type
            with
              _ -> warn parameter error (Some "line 1067") Exit (Ckappa_sig.string_of_agent_name agent_type)
          in
          let error, agent_string' =
            try
              Handler.string_of_agent parameter error handler_kappa agent_type'
            with
              _ -> warn parameter error (Some "line 1072") Exit (Ckappa_sig.string_of_agent_name agent_type')
          in
          let _ =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_id:%i:%s:site_type:%i:%s.%s:site_type:%i:%s.%s->\
               agent_id:%i:%s:site_type:%i:%s.%s:site_type:%i:%s.%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              agent_string
              (Ckappa_sig.int_of_site_name site_type1)
              site_type1_string
              state1_string
              (Ckappa_sig.int_of_site_name site_type2)
              site_type2_string
              state2_string
              (Ckappa_sig.int_of_agent_id agent_id')
              agent_string'
              (Ckappa_sig.int_of_site_name site_type1')
              site_type1_string'
              state1_string'
              (Ckappa_sig.int_of_site_name site_type2')
              site_type2_string'
              state2_string'
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameter)
        ) store_result
  in
  ()

(**************************************************************************)

let print_parallel_bonds_rhs parameter handler_kappa store_parallel_bonds_rhs static dynamic error =
  (*let parameter = get_parameter static in
  let store_parallel_bonds_rhs = get_parallel_bonds_rhs static in*)
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "\nA set of parallel bonds in the rhs:\n";
  print_parallel_bonds parameter handler_kappa static dynamic error store_parallel_bonds_rhs

(**************************************************************************)

let print_rule_has_parallel_bonds parameter handler_kappa static dynamic error store_result =
  let _ =
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id set ->
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);
        let _ =
          print_parallel_bonds parameter handler_kappa static dynamic error set
        in
        ()
      ) store_result
  in
  ()

(**************************************************************************)

let print_rule_has_parallel_bonds_rhs parameter handler_kappa
    store_rule_has_parallel_bonds_rhs static dynamic error =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "\nA set of parallel bonds in the rhs correspond with its rule:\n";
  print_rule_has_parallel_bonds parameter handler_kappa static dynamic error store_rule_has_parallel_bonds_rhs

(**************************************************************************)

let print_action_binding parameter handler_kappa store_action_binding static dynamic error =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Binding action:\n";
  let _ =
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id set ->
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "rule_id:%i\n" (Ckappa_sig.int_of_rule_id rule_id);
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Set.iter
          (fun ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state')) ->
            let error, (site_type_string, site_type_string', state_string, state_string') =
              print_pair parameter error handler_kappa
                ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state'))
            in
            let error, agent_string =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type
              with
                _ -> warn parameter error (Some "line 1118") Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
            in
            let error, agent_string' =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type'
              with
                _ -> warn parameter error (Some "line 1125") Exit
                  (Ckappa_sig.string_of_agent_name agent_type')
            in
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_id:%i:%s:site_type:%i:%s -> agent_id:%i:%s:site_type:%i:%s\n\
                 agent_id:%i:%s:site_type:%i:%s -> agent_id:%i:%s:site_type:%i:%s\n"
              (Ckappa_sig.int_of_agent_id agent_id)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_type_string
              (Ckappa_sig.int_of_agent_id agent_id')
              agent_string'
              (Ckappa_sig.int_of_site_name site_type')
              site_type_string'
                (*reverse direction*)
              (Ckappa_sig.int_of_agent_id agent_id')
              agent_string'
              (Ckappa_sig.int_of_site_name site_type')
              site_type_string'
              (Ckappa_sig.int_of_agent_id agent_id)
              agent_string
              (Ckappa_sig.int_of_site_name site_type)
              site_type_string
            ;
            Loggers.print_newline (Remanent_parameters.get_logger parameter)
          ) set
      ) store_action_binding
  in
  ()

 (**************************************************************************)

let print_site_create_parallel parameter handler_kappa static dynamic error store_result =
  let _ =
    Ckappa_sig.Rule_map_and_set.Map.iter
      (fun rule_id map ->
        let _ =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "rule_id:%i\n"
            (Ckappa_sig.int_of_rule_id rule_id)
        in
        Parallel_bonds_type.PairAgentsSiteState_map_and_set.Map.iter
          (fun ((agent_id, agent_type, site_type, state),
                (agent_id', agent_type', site_type', state')) list ->
            let error, (site_type_string, site_type_string', state_string, state_string') =
              print_pair parameter error handler_kappa
                ((agent_id, agent_type, site_type, state),
                 (agent_id', agent_type', site_type', state'))
            in
            let error, agent_string =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type
              with
                _ -> warn parameter error (Some "line 1180") Exit
                  (Ckappa_sig.string_of_agent_name agent_type)
            in
            let error, agent_string' =
              try
                Handler.string_of_agent parameter error handler_kappa agent_type'
              with
                _ -> warn parameter error (Some "line 1187") Exit
                  (Ckappa_sig.string_of_agent_name agent_type')
            in
            let _ =
              Loggers.fprintf (Remanent_parameters.get_logger parameter)
                "Binding action:\n(agent_id:%i:%s:site_type:%i:%s.%s, agent_id:%i:%s:site_type:%i:%s.%s)\n\
                   (agent_id:%i:%s:site_type:%i:%s.%s, agent_id:%i:%s:site_type:%i:%s.%s)\n"
                (Ckappa_sig.int_of_agent_id agent_id)
                agent_string
                (Ckappa_sig.int_of_site_name site_type)
                site_type_string
                state_string
                (Ckappa_sig.int_of_agent_id agent_id')
                agent_string'
                (Ckappa_sig.int_of_site_name site_type')
                site_type_string'
                state_string'
                (*reverse binding*)
                (Ckappa_sig.int_of_agent_id agent_id')
                agent_string'
                (Ckappa_sig.int_of_site_name site_type')
                site_type_string'
                state_string'
                (Ckappa_sig.int_of_agent_id agent_id)
                agent_string
                (Ckappa_sig.int_of_site_name site_type)
                site_type_string
                state_string
            in
             (*--------------------------------------------------------------------*)
            List.iter
               (*A.x.y.B.z.t, B.z.t.A.x.y*)
              (fun ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                    (agent_id1', agent_type1', site_type1', site_type2', state1', state2')) ->
                let error, ((site_type1_string, site_type2_string, state1_string, state2_string),
                            (site_type1_string', site_type2_string', state1_string', state2_string')) =
                  print_parallel_pair parameter error handler_kappa
                    ((agent_id1, agent_type1, site_type1, site_type2, state1, state2),
                     (agent_id1', agent_type1', site_type1', site_type2', state1', state2'))
                in
                let error, agent_string1 =
                  try
                    Handler.string_of_agent parameter error handler_kappa agent_type1
                  with
                    _ -> warn parameter error (Some "line 1213") Exit
                      (Ckappa_sig.string_of_agent_name agent_type1)
                in
                let error, agent_string1' =
                  try
                    Handler.string_of_agent parameter error handler_kappa agent_type1'
                  with
                    _ -> warn parameter error (Some "line 1220") Exit
                      (Ckappa_sig.string_of_agent_name agent_type1')
                in
                Loggers.fprintf (Remanent_parameters.get_logger parameter)
                  "List of parallel bonds:\nagent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s->\
                    agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s\n\
                    agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s->\
                    agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s\n"
                  (Ckappa_sig.int_of_agent_id agent_id)
                  agent_string1
                  (Ckappa_sig.int_of_site_name site_type1)
                  site_type1_string
                  state1_string
                  (Ckappa_sig.int_of_site_name site_type2)
                  site_type2_string
                  state2_string
                  (Ckappa_sig.int_of_agent_id agent_id')
                  agent_string1'
                  (Ckappa_sig.int_of_site_name site_type1')
                  site_type1_string'
                  state1_string'
                  (Ckappa_sig.int_of_site_name site_type2')
                  site_type2_string'
                  state2_string'
                   (*reverse binding*)
                  (Ckappa_sig.int_of_agent_id agent_id1')
                  agent_string1'
                  (Ckappa_sig.int_of_site_name site_type1')
                  site_type1_string'
                  state1_string'
                  (Ckappa_sig.int_of_site_name site_type2')
                  site_type2_string'
                  state2_string'
                  (Ckappa_sig.int_of_agent_id agent_id1)
                  agent_string1
                  (Ckappa_sig.int_of_site_name site_type1)
                  site_type1_string
                  state1_string
                  (Ckappa_sig.int_of_site_name site_type2)
                  site_type2_string
                  state2_string
                ;
                Loggers.print_newline (Remanent_parameters.get_logger parameter)
              ) list
          ) map
      ) store_result
  in
  ()

 (**************************************************************************)

let print_fst_site_create_parallel_rhs parameter handler_kappa store_fst_site_create_parallel_bonds_rhs static dynamic error =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Rules can create the first pair of a parallel bonds in the rhs:\n";
  print_site_create_parallel parameter handler_kappa static dynamic error store_fst_site_create_parallel_bonds_rhs

 (**************************************************************************)

let print_snd_site_create_parallel_rhs parameter handler_kappa store_snd_site_create_parallel_bonds_rhs static dynamic error =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Rules can create the second pair of a parallel bonds in the rhs:\n";
  print_site_create_parallel parameter handler_kappa static dynamic error store_snd_site_create_parallel_bonds_rhs

(**************************************************************************)

let print_value parameter value =
  match value with
  | Usual_domains.Val b ->
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "Val %b\n" b
  | Usual_domains.Any ->
    Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "Any\n"
  | Usual_domains.Undefined -> () (*FIXME*)
  (*Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "Undefined\n"*)

(**************************************************************************)

let print_result' handler_kappa parameter error store_result =
  Parallel_bonds_type.PairAgentsSitesStates_map_and_set.Map.iter
    (fun ((agent_id, agent_type, site_type1, site_type2, state1, state2),
          (agent_id', agent_type', site_type1', site_type2', state1', state2')) value ->
      let error, ((site_type1_string, site_type2_string, state1_string, state2_string),
                  (site_type1_string', site_type2_string', state1_string', state2_string'))=
        print_parallel_pair parameter error handler_kappa
          ((agent_id, agent_type, site_type1, site_type2, state1, state2),
           (agent_id', agent_type', site_type1', site_type2', state1', state2'))
      in
      let error, agent_string =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type
        with
          _ -> warn parameter error (Some "line 1395") Exit (Ckappa_sig.string_of_agent_name agent_type)
      in
      let error, agent_string' =
        try
          Handler.string_of_agent parameter error handler_kappa agent_type'
        with
          _ -> warn parameter error (Some "line 1401") Exit (Ckappa_sig.string_of_agent_name agent_type')
      in
      if (Ckappa_sig.int_of_state_index state2) = 0 (*if it is free then do not print*)
      then
        ()
        (*Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "Undefined\n"*)
      else
        Loggers.fprintf (Remanent_parameters.get_logger parameter)
          "agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s->\
             agent_id:%i:%s:site_type:%i:%s%s:site_type:%i:%s%s\n"
          (Ckappa_sig.int_of_agent_id agent_id)
          agent_string
          (Ckappa_sig.int_of_site_name site_type1)
          site_type1_string
          state1_string
          (Ckappa_sig.int_of_site_name site_type2)
          site_type2_string
          state2_string
          (Ckappa_sig.int_of_agent_id agent_id')
          agent_string'
          (Ckappa_sig.int_of_site_name site_type1')
          site_type1_string'
          state1_string'
          (Ckappa_sig.int_of_site_name site_type2')
          site_type2_string'
          state2_string';
      print_value parameter value
    ) store_result

    let print_result handler_kappa parameter error store_result =
      Parallel_bonds_type.PairAgentSitesStates_map_and_set.Map.iter
        (fun ((agent_type, site_type1, site_type2, state1, state2),
              (agent_type', site_type1', site_type2', state1', state2')) value ->
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i->\
               agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
              (Ckappa_sig.int_of_agent_name agent_type)
              (Ckappa_sig.int_of_site_name site_type1)
              (Ckappa_sig.int_of_site_name site_type2)
              (Ckappa_sig.int_of_state_index state1)
              (Ckappa_sig.int_of_state_index state2)

              (Ckappa_sig.int_of_agent_name agent_type')
              (Ckappa_sig.int_of_site_name site_type1')
              (Ckappa_sig.int_of_site_name site_type2')
              (Ckappa_sig.int_of_state_index state1')
              (Ckappa_sig.int_of_state_index state2')
          ;
          print_value parameter value
        ) store_result

(**************************************************************************)
(*print result of parallel bonds in the initial state*)

let print_parallel_bonds_init parameter handler_kappa store_parallel_bonds_init static dynamic error =
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "\nResult of parallel bonds in the initial states:\n";
  print_result handler_kappa parameter error store_parallel_bonds_init;
  Loggers.print_newline (Remanent_parameters.get_logger parameter)

(******************************************************************)

let print_action_binding_test parameter error handler_kappa rule_id (x, y) =
  let (agent_id, agent_type, site_type, state) = x in
  let (agent_id', agent_type', site_type', state') = y in
  let error, (site_type_string, site_type_string', state_string, state_string')  =
    print_pair parameter error handler_kappa
      ((agent_id, agent_type, site_type, state),
       (agent_id', agent_type', site_type', state'))
  in
  let error, agent_string =
    try
      Handler.string_of_agent parameter error handler_kappa agent_type
    with
      _ -> warn parameter error (Some "line 1642") Exit (Ckappa_sig.string_of_agent_name agent_type)
  in
  let error, agent_string' =
    try
      Handler.string_of_agent parameter error handler_kappa agent_type'
    with
      _ -> warn parameter error (Some "line 1648") Exit (Ckappa_sig.string_of_agent_name agent_type')
  in
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "\nrule_id:%i: binding action appears\n"
    (Ckappa_sig.int_of_rule_id rule_id);
  Loggers.fprintf (Remanent_parameters.get_logger parameter)
    "agent_id:%i:%s:site_type:%i:%s%s-> agent_id:%i:%s:site_type:%i:%s%s\n\
       agent_id:%i:%s:site_type:%i:%s%s-> agent_id:%i:%s:site_type:%i:%s%s\n"
    (Ckappa_sig.int_of_agent_id agent_id)
    agent_string
    (Ckappa_sig.int_of_site_name site_type)
    site_type_string
    state_string
    (Ckappa_sig.int_of_agent_id agent_id')
    agent_string'
    (Ckappa_sig.int_of_site_name site_type')
    site_type_string'
    state_string'
    (*reverse direction*)
    (Ckappa_sig.int_of_agent_id agent_id')
    agent_string'
    (Ckappa_sig.int_of_site_name site_type')
    site_type_string'
    state_string'
    (Ckappa_sig.int_of_agent_id agent_id)
    agent_string
    (Ckappa_sig.int_of_site_name site_type)
    site_type_string
    state_string

(******************************************************************)
(*print non parallel bonds*)

let print_rule_has_non_parallel_bonds_rhs parameter store_result =
  Ckappa_sig.Rule_map_and_set.Map.iter (fun rule_id list ->
      let log = Remanent_parameters.get_logger parameter in
      Loggers.fprintf log "rule_id:%i\n"
      (Ckappa_sig.int_of_rule_id rule_id);
      List.iter (fun ((agent_id, agent_type, site_type, state),
                      (agent_id', agent_type', site_type', state'),
                      (agent_id1, agent_type1, site_type1, state1),
                      (agent_id1', agent_type1', site_type1', state1')) ->
                      Loggers.fprintf log
                        "agent_id:%i:agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i -> agent_id:%i:agent_id:%i:agent_type:%i:site_type:%i:site_type:%i:state:%i:state:%i\n"
                        (Ckappa_sig.int_of_agent_id agent_id)
                        (Ckappa_sig.int_of_agent_name agent_type)
                        (Ckappa_sig.int_of_site_name site_type)
                        (Ckappa_sig.int_of_site_name site_type')
                        (Ckappa_sig.int_of_state_index state)
                        (Ckappa_sig.int_of_state_index state')
                        (**)
                        (Ckappa_sig.int_of_agent_id agent_id1)
                        (Ckappa_sig.int_of_agent_id agent_id1')
                        (Ckappa_sig.int_of_agent_name agent_type1)
                        (Ckappa_sig.int_of_site_name site_type1)
                        (Ckappa_sig.int_of_site_name site_type1')
                        (Ckappa_sig.int_of_state_index state1)
                        (Ckappa_sig.int_of_state_index state1')
                ) list
    ) store_result
