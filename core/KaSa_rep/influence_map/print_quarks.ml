(**
 * translate_sig.ml
 * openkappa
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 *
 * Creation: March, the 8th 2011.
 * Last modification: Time-stamp: <Dec 30 2018>
 * *
 * Pretty printing of influence map
 *
 * Copyright 2010,2011,2012,2013,2014,2015 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let trace = false
let local_trace = false

let string_of_int_option_min parameters _error a =
  match a with
  | Some a -> string_of_int a
  | None -> Remanent_parameters.get_minus_infinity_symbol parameters

let string_of_int_option_max parameters _error a =
  match a with
  | Some a -> string_of_int a
  | None -> Remanent_parameters.get_plus_infinity_symbol parameters

let string_of_port parameters error port =
  "["
  ^ string_of_int_option_min parameters error
      port.Cckappa_sig.site_state.Cckappa_sig.min
  ^ ";"
  ^ string_of_int_option_max parameters error
      port.Cckappa_sig.site_state.Cckappa_sig.max
  ^ "]"

let string_of_rule_var parameters error handler compilation print_rule_dot
    print_var_dot get_label_of_rule_dot get_label_of_var_dot k =
  let s = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer s in
  let parameters =
    Remanent_parameters.set_logger parameters
      (Loggers.open_logger_from_formatter fmt)
  in
  let error, bool, () =
    Handler.print_rule_or_var parameters error handler compilation
      print_rule_dot print_var_dot get_label_of_rule_dot get_label_of_var_dot k
  in
  let _ = Format.pp_print_flush fmt () in
  let s = Buffer.contents s in
  error, bool, s

let print_agent_map parameters error handler map =
  let error =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
      error
      (fun parameters error key im ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
          error
          (fun parameters error key' im' ->
            let _ =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%sagent_type:%s,rule:%s->"
                (Remanent_parameters.get_prefix parameters)
                (Ckappa_sig.string_of_agent_name key)
                (Ckappa_sig.string_of_rule_id key')
            in
            let _ = Quark_type.Labels.dump parameters error handler im' in
            let _ =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error)
          im)
      map
  in
  error

let print_agent_var_map parameters error handler map =
  let error =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
      error
      (fun parameters error key im ->
        Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
          error
          (fun parameters error key' im' ->
            let _ =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%sagent_type:%s,var:%s->"
                (Remanent_parameters.get_prefix parameters)
                (Ckappa_sig.string_of_agent_name key)
                (Ckappa_sig.string_of_rule_id key')
            in
            let _ = Quark_type.Labels.dump parameters error handler im' in
            let _ =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in
            error)
          im)
      map
  in
  error

let print_string_map parameters error handler map =
  let error =
    Quark_type.StringMap.fold
      (fun key im error ->
        let error =
          Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
            error
            (fun parameters error key' im' ->
              let _ =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%sagent_type:%s,rule_id:%s->"
                  (Remanent_parameters.get_prefix parameters)
                  key
                  (Ckappa_sig.string_of_rule_id key')
              in
              let _ = Quark_type.Labels.dump parameters error handler im' in
              let _ =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            im
        in
        error)
      map error
  in
  error

let print_var_string_map parameters error handler map =
  let error =
    Quark_type.StringMap.fold
      (fun key im error ->
        let error =
          Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
            error
            (fun parameters error key' im' ->
              let _ =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%sagent_type:%s,var_id:%s->"
                  (Remanent_parameters.get_prefix parameters)
                  key
                  (Ckappa_sig.string_of_rule_id key')
              in
              let _ = Quark_type.Labels.dump parameters error handler im' in
              let _ =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            im
        in
        error)
      map error
  in
  error

let print_agents parameters error handler quark =
  let parameters_var =
    Remanent_parameters.update_prefix parameters "agent_var++**:"
  in
  let error =
    print_agent_var_map parameters_var error handler
      quark.Quark_type.agent_var_plus
  in
  let parameters_var =
    Remanent_parameters.update_prefix parameters "agent_var--**:"
  in
  let error =
    print_agent_var_map parameters_var error handler
      quark.Quark_type.agent_var_minus
  in
  let parameters_plus =
    Remanent_parameters.update_prefix parameters "agent_test**:"
  in
  let error =
    print_agent_map parameters_plus error handler quark.Quark_type.agent_test
  in
  let parameters_plus =
    Remanent_parameters.update_prefix parameters "agent_modif+:"
  in
  let error =
    print_agent_map parameters_plus error handler
      quark.Quark_type.agent_modif_plus
  in
  let parameters_minus =
    Remanent_parameters.update_prefix parameters "agent_modif-:"
  in
  let error =
    print_agent_map parameters_minus error handler
      quark.Quark_type.agent_modif_minus
  in
  error

let print_site_map parameter error handler map =
  Quark_type.SiteMap.iter parameter error
    (fun parameters error (agent_type, (site_type, state)) im ->
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
        error
        (fun parameters error rule im' ->
          let _ =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameter)
              "%sagent_type:%s,site_type:%s,state:%s,rule:%s->"
              (Remanent_parameters.get_prefix parameter)
              (Ckappa_sig.string_of_agent_name agent_type)
              (Ckappa_sig.string_of_site_name site_type)
              (Ckappa_sig.string_of_state_index state)
              (Ckappa_sig.string_of_rule_id rule)
          in
          let _ = Quark_type.Labels.dump parameter error handler im' in
          let _ =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        im)
    map

let print_site_var_map parameter error handler map =
  Quark_type.SiteMap.iter parameter error
    (fun parameters error (agent_type, (site_type, state)) im ->
      Ckappa_sig.Rule_quick_nearly_Inf_Int_storage_Imperatif.iter parameters
        error
        (fun parameters error rule im' ->
          let _ =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameter)
              "%sagent_type:%s,site_type:%s,state:%s,var:%s->"
              (Remanent_parameters.get_prefix parameter)
              (Ckappa_sig.string_of_agent_name agent_type)
              (Ckappa_sig.string_of_site_name site_type)
              (Ckappa_sig.string_of_state_index state)
              (Ckappa_sig.string_of_rule_id rule)
          in
          let _ = Quark_type.Labels.dump parameter error handler im' in
          let _ =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        im)
    map

let print_sites parameter error handler quark =
  let parameter_var =
    Remanent_parameters.update_prefix parameter "site_vars++**:"
  in
  let error =
    print_site_var_map parameter_var error handler
      quark.Quark_type.site_var_plus
  in
  let parameter_var =
    Remanent_parameters.update_prefix parameter "site_vars--**:"
  in
  let error =
    print_site_var_map parameter_var error handler
      quark.Quark_type.site_var_minus
  in
  let parameter_plus =
    Remanent_parameters.update_prefix parameter "site_test**:"
  in
  let error =
    print_site_map parameter_plus error handler quark.Quark_type.site_test
  in
  let parameter_plus =
    Remanent_parameters.update_prefix parameter "site_modif+:"
  in
  let error =
    print_site_map parameter_plus error handler quark.Quark_type.site_modif_plus
  in
  let parameter_minus =
    Remanent_parameters.update_prefix parameter "site_modif-:"
  in
  let error =
    print_site_map parameter_minus error handler
      quark.Quark_type.site_modif_minus
  in
  error

let print_dead_agents parameter error handler quark =
  let parameter_var =
    Remanent_parameters.update_prefix parameter "dead_agent**:"
  in
  let error =
    print_string_map parameter_var error handler quark.Quark_type.dead_agent
  in
  let parameter_var =
    Remanent_parameters.update_prefix parameter "dead_agent++**:"
  in
  let error =
    print_var_string_map parameter_var error handler
      quark.Quark_type.dead_agent_plus
  in
  let parameter_plus =
    Remanent_parameters.update_prefix parameter "dead_agent--**:"
  in
  let error =
    print_var_string_map parameter_plus error handler
      quark.Quark_type.dead_agent_minus
  in
  error

let print_quarks parameters error handler quark =
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "REMARKS: The notation [i] is a position of an agent in a rule/var. If a \
       position is a negative number [-i], then it refers an agent that is \
       connected to the agent at position (i-1) that is modified by side \
       effects."
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error = print_agents parameters error handler quark in
  let error = print_sites parameters error handler quark in
  let error = print_dead_agents parameters error handler quark in
  error

let print_maps ?(directives = []) parameters logger error handler compilation
    print_rule print_var get_label_of_rule get_label_of_var print_labels prefix
    _suffix map =
  let error =
    Ckappa_sig.PairRule_setmap.Map.fold
      (fun (a, b) couple error ->
        let error, _bool, s1 =
          string_of_rule_var parameters error handler compilation print_rule
            print_var get_label_of_rule get_label_of_var a
        in
        let error, _bool, s2 =
          string_of_rule_var parameters error handler compilation print_rule
            print_var get_label_of_rule get_label_of_var b
        in
        let error, s3 =
          let s = Buffer.create 0 in
          let fmt = Format.formatter_of_buffer s in
          let parameters =
            Remanent_parameters.set_logger parameters
              (Loggers.open_logger_from_formatter fmt)
          in
          let error = print_labels parameters error handler couple in
          let () = Format.pp_print_flush fmt () in
          let s = Buffer.contents s in
          error, s
        in
        let directives = Graph_loggers_sig.Label s3 :: directives in
        let () = Graph_loggers.print_edge logger ~directives ~prefix s1 s2 in
        error)
      map error
  in
  error

let print_wake_up_map parameters error handler compilation print_rule print_var
    print_label_rule print_label_var print_labels suffix map =
  let parameters =
    Remanent_parameters.update_prefix parameters "Wake_up_map:"
  in
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "Influence_map: The notation [i -> j] means an agent at position [i] of \
       the first rule/var has an influence to an agent at position [j] of the \
       second rule/var."
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  print_maps parameters
    (Graph_loggers_sig.extend_logger
       (Remanent_parameters.get_logger parameters))
    error handler compilation print_rule print_var print_label_rule
    print_label_var print_labels
    (Remanent_parameters.get_prefix parameters)
    suffix map

let print_inhibition_map parameters error handler compilation print_rule
    print_var print_label_rule print_label_var print_labels suffix map =
  let parameters =
    Remanent_parameters.update_prefix parameters "Inhibition_map:"
  in
  print_maps parameters
    (Graph_loggers_sig.extend_logger
       (Remanent_parameters.get_logger parameters))
    error handler compilation print_rule print_var print_label_rule
    print_label_var print_labels
    (Remanent_parameters.get_prefix parameters)
    suffix map

let dot_of_influence_map ?logger parameters error handler compilation
    (nodes, wake_up_map, inhibition_map) =
  let loggers = logger in
  let parameters_dot =
    match loggers with
    | None -> Remanent_parameters.open_influence_map_file parameters
    | Some loggers -> Remanent_parameters.set_logger parameters loggers
  in
  let logger =
    Graph_loggers_sig.extend_logger
      (Remanent_parameters.get_logger parameters_dot)
  in
  let () =
    Graph_loggers.print_graph_preamble logger
      ~header:(Headers.head parameters_dot @ Headers.head_influence_map_in_dot)
      "Influence_map"
  in
  let nrules = Handler.nrules parameters error handler in
  let error =
    List.fold_left
      (fun error k ->
        if Ckappa_sig.int_of_rule_id k < nrules then (
          let error, bool, s =
            string_of_rule_var parameters error handler compilation
              Handler.print_rule_dot Handler.print_var_dot
              Handler.get_label_of_rule_dot Handler.get_label_of_var_dot k
          in
          let _ =
            if bool then (
              let _ =
                Graph_loggers.print_node logger s
                  ~directives:
                    [
                      Graph_loggers_sig.Shape
                        (Remanent_parameters.get_rule_shape parameters_dot);
                      Graph_loggers_sig.FillColor
                        (Remanent_parameters.get_rule_color parameters_dot);
                    ]
              in
              ()
            )
          in
          error
        ) else (
          let error, bool, s =
            string_of_rule_var parameters error handler compilation
              Handler.print_rule_dot Handler.print_var_dot
              Handler.get_label_of_rule_dot Handler.get_label_of_var_dot k
          in
          let () =
            if Ckappa_sig.int_of_rule_id k = nrules then
              Loggers.print_newline (Graph_loggers_sig.lift logger)
          in
          let () =
            if bool then (
              let _ =
                Graph_loggers.print_node logger s
                  ~directives:
                    [
                      Graph_loggers_sig.Shape
                        (Remanent_parameters.get_variable_shape parameters_dot);
                      Graph_loggers_sig.FillColor
                        (Remanent_parameters.get_variable_color parameters_dot);
                    ]
              in
              ()
            )
          in
          error
        ))
      error nodes
  in
  let error =
    if Ckappa_sig.PairRule_setmap.Map.is_empty wake_up_map then
      error
    else (
      let error =
        print_maps
          ~directives:
            [
              Graph_loggers_sig.Color
                (Remanent_parameters.get_wake_up_color parameters_dot);
              Graph_loggers_sig.ArrowHead
                (Remanent_parameters.get_wake_up_arrow parameters_dot);
            ]
          parameters_dot logger error handler compilation Handler.print_rule_dot
          Handler.print_var_dot Handler.get_label_of_rule_dot
          Handler.get_label_of_var_dot Handler.print_labels "" " ;" wake_up_map
      in
      error
    )
  in
  let error =
    if Ckappa_sig.PairRule_setmap.Map.is_empty inhibition_map then
      error
    else (
      (* let () =
         Loggers.fprintf
         (Remanent_parameters.get_logger parameters_dot)
         "edge [color=%s, arrowhead=%s];"
         (Remanent_parameters.get_inhibition_color parameters_dot)
         (Remanent_parameters.get_inhibition_arrow parameters_dot)
         in*)
      (*  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters_dot) in*)
      let error =
        print_maps
          ~directives:
            [
              Graph_loggers_sig.Color
                (Remanent_parameters.get_inhibition_color parameters_dot);
              Graph_loggers_sig.ArrowHead
                (Remanent_parameters.get_inhibition_arrow parameters_dot);
            ]
          parameters_dot logger error handler compilation Handler.print_rule_dot
          Handler.print_var_dot Handler.get_label_of_rule_dot
          Handler.get_label_of_var_dot Handler.print_labels "" " ;"
          inhibition_map
      in

      error
    )
  in
  let _ = Graph_loggers.print_graph_foot logger in
  let () =
    match loggers with
    | None ->
      Loggers.close_logger (Remanent_parameters.get_logger parameters_dot)
    | Some _ ->
      Loggers.flush_logger (Remanent_parameters.get_logger parameters_dot)
  in
  error
