(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

type _ handle =
  | Nothing : unit handle
  | BigString : Bigbuffer.bigstring handle
  | String : string handle
  | Strings : string list handle
  | Catalog : Kfiles.catalog_item list handle
  | Info : (string * int) handle
  | Ast : Ast.parsing_compil handle
  | JSON : Yojson.Basic.t handle
  | Influence_map
      : (Public_data.accuracy_level
        * int
        * int option
        * int option
        * (Public_data.rule, Public_data.var) Public_data.influence_node option
        * Public_data.influence_map)
        handle
  | Short_influence_node : (int, int) Public_data.influence_node option handle
  | Influence_node
      : (Public_data.rule, Public_data.var) Public_data.influence_node option
        handle
  | Influence_nodes
      : (Public_data.accuracy_level
        * (Public_data.rule, Public_data.var) Public_data.influence_node list)
        handle
  | Rules_kasa : Public_data.rule list handle
  | Agents_kasa : Public_data.dead_agents handle
  | Transitions_kasa : (Public_data.rule * (string * string) list) list handle
  | Constraints_kasa
      : (string * Public_data.agent list Public_data.lemma list) list handle
  | Polymers_kasa
      : (Public_data.accuracy_level
        * Public_data.accuracy_level
        * Public_data.scc)
        handle
  | DIN : Data.din handle
  | Plot : Data.plot handle
  | Snapshot : Data.snapshot handle
  | Simulation_efficiency : Counter.Efficiency.t handle
  | Simulation_info : Api_types_t.simulation_info handle
  | Simulation_parameter : Api_types_t.simulation_parameter handle
  | Simulation_artifact : Api_types_t.simulation_artifact handle

type box =
  | B : 'a handle * int * ('a, Result_util.message list) Result_util.t -> box

let reply post write_v id v =
  let message =
    JsonUtil.string_of_write
      (fun b () ->
        JsonUtil.write_sequence b
          [
            (fun b -> Yojson.Basic.write_int b id);
            (fun b ->
              Result_util.write_t write_v
                (JsonUtil.write_list Result_util.write_message)
                b v);
          ])
      ()
  in
  post message

let on_message exec_command message_delimiter =
  let post message =
    Lwt_io.atomic
      (fun f ->
        Lwt_io.write f message >>= fun () ->
        Lwt_io.write_char f message_delimiter)
      Lwt_io.stdout
  in
  let manager = new Agents_client.t exec_command message_delimiter in
  let () = at_exit (fun () -> manager#terminate) in
  let current_id = ref None in
  fun text ->
    try
      Lwt.bind
        (JsonUtil.read_of_string
           (JsonUtil.read_variant Yojson.Basic.read_int (fun st b msg_id ->
                let () = current_id := Some msg_id in
                JsonUtil.read_next_item
                  (JsonUtil.read_variant Yojson.Basic.read_string (fun st b ->
                       function
                     (* KaMoHa *)
                     | "FileCatalog" ->
                       manager#file_catalog >>= fun out ->
                       Lwt.return (B (Catalog, msg_id, out))
                     | "FileCreate" ->
                       let position =
                         JsonUtil.read_next_item Yojson.Basic.read_int st b
                       in
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#file_create position id content >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "FileGet" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#file_get id >>= fun out ->
                       Lwt.return (B (Info, msg_id, out))
                     | "FileMove" ->
                       let position =
                         JsonUtil.read_next_item Yojson.Basic.read_int st b
                       in
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#file_move position id >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "FileUpdate" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#file_update id content >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "FileDelete" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#file_delete id >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "ProjectOverwrite" ->
                       let id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let content =
                         JsonUtil.read_next_item Ast.read_parsing_compil st b
                       in
                       manager#project_overwrite id content >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     (* KaSa *)
                     | "INIT" ->
                       let compil =
                         JsonUtil.read_next_item Ast.read_parsing_compil st b
                       in
                       manager#init_static_analyser compil >>= fun out ->
                       Lwt.return
                         (B (Nothing, msg_id, Api_common.result_kasa out))
                     | "CONTACT_MAP" ->
                       let acc =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let accuracy =
                         JsonUtil.to_option Public_data.accuracy_of_json acc
                       in
                       manager#get_contact_map accuracy >>= fun out ->
                       Lwt.return (B (JSON, msg_id, Api_common.result_kasa out))
                     | "LOCAL_INFLUENCE_MAP" ->
                       let acc =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let fwd =
                         JsonUtil.read_next_item
                           (JsonUtil.read_option Yojson.Basic.read_int)
                           st b
                       in
                       let bwd =
                         JsonUtil.read_next_item
                           (JsonUtil.read_option Yojson.Basic.read_int)
                           st b
                       in
                       let total =
                         JsonUtil.read_next_item Yojson.Basic.read_int st b
                       in
                       let origin =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let accuracy =
                         JsonUtil.to_option Public_data.accuracy_of_json acc
                       in
                       let origin =
                         JsonUtil.to_option
                           Public_data.short_influence_node_of_json origin
                       in
                       manager#get_local_influence_map accuracy ?fwd ?bwd
                         ?origin ~total
                       >>= fun out ->
                       Lwt.return
                         (B (Influence_map, msg_id, Api_common.result_kasa out))
                     | "INFLUENCE_MAP" ->
                       let acc =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let accuracy =
                         JsonUtil.to_option Public_data.accuracy_of_json acc
                       in
                       manager#get_influence_map_raw accuracy >>= fun out ->
                       Lwt.return
                         (B (String, msg_id, Api_common.result_kasa out))
                     | "INFLUENCE_MAP_ORIGINAL_NODE" ->
                       manager#get_initial_node >>= fun out ->
                       Lwt.return
                         (B (Influence_node, msg_id, Api_common.result_kasa out))
                     | "INFLUENCE_MAP_NEXT_NODE" ->
                       let origin =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let origin =
                         JsonUtil.to_option
                           Public_data.short_influence_node_of_json origin
                       in
                       manager#get_next_node origin >>= fun out ->
                       Lwt.return
                         (B (Influence_node, msg_id, Api_common.result_kasa out))
                     | "INFLUENCE_MAP_PREVIOUS_NODE" ->
                       let origin =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let origin =
                         JsonUtil.to_option
                           Public_data.short_influence_node_of_json origin
                       in
                       manager#get_next_node origin >>= fun out ->
                       Lwt.return
                         (B (Influence_node, msg_id, Api_common.result_kasa out))
                     | "INFLUENCE_MAP_NODE_AT" ->
                       let filename =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       let pos =
                         JsonUtil.read_next_item Locality.read_position st b
                       in
                       manager#get_influence_map_node_at ~filename pos
                       >>= fun out ->
                       Lwt.return (B (Short_influence_node, msg_id, out))
                     | "INFLUENCE_MAP_ALL_NODES" ->
                       let accuracy_level =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let accuracy =
                         JsonUtil.to_option Public_data.accuracy_of_json
                           accuracy_level
                       in
                       manager#get_nodes_of_influence_map accuracy
                       >>= fun out ->
                       Lwt.return
                         (B (Influence_nodes, msg_id, Api_common.result_kasa out))
                     | "DEAD_RULES" ->
                       manager#get_dead_rules >>= fun out ->
                       Lwt.return
                         (B (Rules_kasa, msg_id, Api_common.result_kasa out))
                     | "DEAD_AGENTS" ->
                       manager#get_dead_agents >>= fun out ->
                       Lwt.return
                         (B (Agents_kasa, msg_id, Api_common.result_kasa out))
                     | "NON_WEAKLY_REVERSIBLE_TRANSITIONS" ->
                       manager#get_non_weakly_reversible_transitions
                       >>= fun out ->
                       Lwt.return
                         (B
                            ( Transitions_kasa,
                              msg_id,
                              Api_common.result_kasa out ))
                     | "CONSTRAINTS" ->
                       manager#get_constraints_list >>= fun out ->
                       Lwt.return
                         (B
                            ( Constraints_kasa,
                              msg_id,
                              Api_common.result_kasa out ))
                     | "POLYMERS" ->
                       let acc_cm =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let acc_scc =
                         JsonUtil.read_next_item Yojson.Basic.read_json st b
                       in
                       let accuracy_cm =
                         JsonUtil.to_option Public_data.accuracy_of_json acc_cm
                       in
                       let accuracy_scc =
                         JsonUtil.to_option Public_data.accuracy_of_json acc_scc
                       in
                       manager#get_potential_polymers accuracy_cm accuracy_scc
                       >>= fun out ->
                       Lwt.return
                         (B (Polymers_kasa, msg_id, Api_common.result_kasa out))
                     (* KaSim *)
                     | "ProjectParse" ->
                       let patternSharing =
                         JsonUtil.read_next_item
                           Kappa_terms.Pattern.read_sharing_level st b
                       in
                       let overwrites =
                         JsonUtil.read_next_item
                           (Yojson.Basic.read_list
                              (JsonUtil.read_compact_pair
                                 Yojson.Basic.read_string Nbr.read_t))
                           st b
                       in
                       manager#project_parse ~patternSharing overwrites
                       >>= fun out -> Lwt.return (B (Nothing, msg_id, out))
                     | "SimulationContinue" ->
                       let simulation_parameter =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#simulation_continue simulation_parameter
                       >>= fun out -> Lwt.return (B (Nothing, msg_id, out))
                     | "SimulationDelete" ->
                       manager#simulation_delete >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "SimulationDetailFileLine" ->
                       let file_line_id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#simulation_detail_file_line file_line_id
                       >>= fun out -> Lwt.return (B (Strings, msg_id, out))
                     | "SimulationDetailDIN" ->
                       let din_id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#simulation_detail_din din_id >>= fun out ->
                       Lwt.return (B (DIN, msg_id, out))
                     | "SimulationDetailLogMessage" ->
                       manager#simulation_detail_log_message >>= fun out ->
                       Lwt.return (B (String, msg_id, out))
                     | "SimulationDetailPlot" ->
                       let plot_parameter =
                         JsonUtil.read_next_item Api_types_j.read_plot_parameter
                           st b
                       in
                       manager#simulation_detail_plot plot_parameter
                       >>= fun out -> Lwt.return (B (Plot, msg_id, out))
                     | "SimulationDetailSnapshot" ->
                       let snapshot_id =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#simulation_detail_snapshot snapshot_id
                       >>= fun out -> Lwt.return (B (Snapshot, msg_id, out))
                     | "SimulationInfo" ->
                       manager#simulation_info >>= fun out ->
                       Lwt.return (B (Simulation_info, msg_id, out))
                     | "SimulationEfficiency" ->
                       manager#simulation_efficiency >>= fun out ->
                       Lwt.return (B (Simulation_efficiency, msg_id, out))
                     | "SimulationCatalogFileLine" ->
                       manager#simulation_catalog_file_line >>= fun out ->
                       Lwt.return (B (Strings, msg_id, out))
                     | "SimulationCatalogDIN" ->
                       manager#simulation_catalog_din >>= fun out ->
                       Lwt.return (B (Strings, msg_id, out))
                     | "SimulationCatalogSnapshot" ->
                       manager#simulation_catalog_snapshot >>= fun out ->
                       Lwt.return (B (Strings, msg_id, out))
                     | "SimulationParameter" ->
                       manager#simulation_parameter >>= fun out ->
                       Lwt.return (B (Simulation_parameter, msg_id, out))
                     | "SimulationTrace" ->
                       manager#simulation_raw_trace >>= fun out ->
                       Lwt.return (B (String, msg_id, out))
                     | "SimulationOutputsZip" ->
                       manager#simulation_outputs_zip >>= fun out ->
                       Lwt.return (B (BigString, msg_id, out))
                       (*(handler (fun result -> `SimulationOutputsZip (Base64.encode result)))*)
                     | "SimulationPause" ->
                       manager#simulation_pause >>= fun out ->
                       Lwt.return (B (Nothing, msg_id, out))
                     | "SimulationIntervention" ->
                       let intervention =
                         JsonUtil.read_next_item Yojson.Basic.read_string st b
                       in
                       manager#simulation_intervention intervention
                       >>= fun out -> Lwt.return (B (String, msg_id, out))
                     | "SimulationStart" ->
                       let simulation_parameter =
                         JsonUtil.read_next_item
                           Api_types_j.read_simulation_parameter st b
                       in
                       manager#simulation_start simulation_parameter
                       >>= fun out ->
                       Lwt.return (B (Simulation_artifact, msg_id, out))
                     (* Errors *)
                     | x ->
                       Lwt.return
                         (B
                            ( Nothing,
                              msg_id,
                              Result_util.error
                                [
                                  {
                                    Result_util.severity = Logs.Error;
                                    range = None;
                                    text = "Invalid directive: " ^ x;
                                  };
                                ] ))))
                  st b))
           text)
        (fun answer ->
          let () = current_id := None in
          match answer with
          | B (Catalog, msg_id, x) ->
            reply post (JsonUtil.write_list Kfiles.write_catalog_item) msg_id x
          | B (Nothing, msg_id, x) ->
            reply post Yojson.Basic.write_null msg_id x
          | B (BigString, msg_id, x) ->
            let x =
              Result_util.map (Base64.encode ?pad:None ?alphabet:None) x
            in
            reply post Yojson.Basic.write_string msg_id x
          | B (String, msg_id, x) ->
            reply post Yojson.Basic.write_string msg_id x
          | B (Strings, msg_id, x) ->
            reply post (JsonUtil.write_list Yojson.Basic.write_string) msg_id x
          | B (Ast, msg_id, x) -> reply post Ast.write_parsing_compil msg_id x
          | B (JSON, msg_id, x) -> reply post Yojson.Basic.write_json msg_id x
          | B (Info, msg_id, x) ->
            reply post
              (JsonUtil.write_compact_pair Yojson.Basic.write_string
                 Yojson.Basic.write_int)
              msg_id x
          | B (Influence_map, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b
                  (Public_data.local_influence_map_to_json n))
              msg_id x
          | B (Short_influence_node, msg_id, x) ->
            reply post
              (JsonUtil.write_option (fun b n ->
                   Yojson.Basic.write_json b
                     (Public_data.short_influence_node_to_json n)))
              msg_id x
          | B (Influence_node, msg_id, x) ->
            reply post
              (JsonUtil.write_option (fun b n ->
                   Yojson.Basic.write_json b
                     (Public_data.refined_influence_node_to_json n)))
              msg_id x
          | B (Influence_nodes, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b
                  (Public_data.nodes_of_influence_map_to_json n))
              msg_id x
          | B (Rules_kasa, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b (Public_data.dead_rules_to_json n))
              msg_id x
          | B (Agents_kasa, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b (Public_data.json_of_dead_agents n))
              msg_id x
          | B (Transitions_kasa, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b
                  (Public_data.separating_transitions_to_json n))
              msg_id x
          | B (Constraints_kasa, msg_id, x) ->
            reply post
              (fun b n ->
                Yojson.Basic.write_json b (Public_data.lemmas_list_to_json n))
              msg_id x
          | B (Polymers_kasa, msg_id, x) ->
            reply post
              (fun b n -> Yojson.Basic.write_json b (Public_data.scc_to_json n))
              msg_id x
          | B (DIN, msg_id, x) -> reply post Data.write_din msg_id x
          | B (Plot, msg_id, x) -> reply post Data.write_plot msg_id x
          | B (Snapshot, msg_id, x) -> reply post Data.write_snapshot msg_id x
          | B (Simulation_efficiency, msg_id, x) ->
            reply post Counter.Efficiency.write_t msg_id x
          | B (Simulation_info, msg_id, x) ->
            reply post Api_types_j.write_simulation_info msg_id x
          | B (Simulation_parameter, msg_id, x) ->
            reply post Api_types_j.write_simulation_parameter msg_id x
          | B (Simulation_artifact, msg_id, x) ->
            reply post Api_types_j.write_simulation_artifact msg_id x)
    with e ->
      (match !current_id with
      | Some msg_id ->
        reply post Yojson.Basic.write_null msg_id
          (Result_util.error
             [
               {
                 Result_util.severity = Logs.Error;
                 range = None;
                 text = "Exception raised: " ^ Printexc.to_string e;
               };
             ])
      | None ->
        (match e with
        | Yojson.Json_error x ->
          post
            (Yojson.to_string
               (`String
                 (x ^ "\nMessage format must be [ id, [\"Request\", ... ] ]")))
        | e ->
          post
            (Yojson.to_string
               (`String
                 ("unexpected exception: " ^ Printexc.to_string e
                ^ "\nMessage format must be [ id, [\"Request\", ... ] ]")))))

(* start server *)
let () =
  let common_args = Common_args.default in
  let stdsim_args = Agent_args.default in
  let options =
    Common_args.options common_args @ Agent_args.options stdsim_args
  in
  let usage_msg = "Kappa Model Handler" in
  let () =
    Arg.parse options
      (fun x -> raise (Arg.Bad ("Don't know what to do of " ^ x)))
      usage_msg
  in
  let () = Printexc.record_backtrace common_args.Common_args.backtrace in
  Lwt_main.run
    (Agent_common.serve Lwt_io.stdin stdsim_args.Agent_args.delimiter
       (on_message Sys.executable_name stdsim_args.Agent_args.delimiter))
