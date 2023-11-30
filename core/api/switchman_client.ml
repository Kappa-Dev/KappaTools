(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type _ handle =
  | Nothing : unit handle
  | BigString : Bigbuffer.bigstring handle
  | String : string handle
  | Strings : string list handle
  | Catalog : Kfiles.catalog_item list handle
  | Info : (string * int) handle
  (*  | Ast : Ast.parsing_compil handle*)
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
  | B : 'a handle * ('a, Result_util.message list) Result_util.t Lwt.u -> box

type mailbox = (int, box) Hashtbl.t

let new_mailbox () = Hashtbl.create 2

let read_result f p lb =
  JsonUtil.read_next_item
    (Result_util.read_t f (Yojson.Basic.read_list Result_util.read_message))
    p lb

let receive mailbox x =
  JsonUtil.read_of_string
    (JsonUtil.read_variant Yojson.Basic.read_int (fun p lb id ->
         let () =
           match Hashtbl.find mailbox id with
           | B (Nothing, thread) ->
             Lwt.wakeup thread (read_result Yojson.Basic.read_null p lb)
           | B (BigString, thread) ->
             let out = read_result Yojson.Basic.read_string p lb in
             Lwt.wakeup thread
               (Result_util.map (Base64.decode ?alphabet:None) out)
           | B (String, thread) ->
             Lwt.wakeup thread (read_result Yojson.Basic.read_string p lb)
           | B (Strings, thread) ->
             Lwt.wakeup thread
               (read_result
                  (Yojson.Basic.read_list Yojson.Basic.read_string)
                  p lb)
           | B (Catalog, thread) ->
             Lwt.wakeup thread
               (read_result
                  (Yojson.Basic.read_list Kfiles.read_catalog_item)
                  p lb)
           | B (Info, thread) ->
             Lwt.wakeup thread
               (read_result
                  (JsonUtil.read_compact_pair Yojson.Basic.read_string
                     Yojson.Basic.read_int)
                  p lb)
             (* | B (Ast, thread) ->
                Lwt.wakeup thread
                        (read_result Ast.read_parsing_compil p lb)*)
           | B (JSON, thread) ->
             Lwt.wakeup thread (read_result Yojson.Basic.read_json p lb)
           | B (Influence_map, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.local_influence_map_of_json json)
           | B (Short_influence_node, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map
                  (JsonUtil.to_option Public_data.short_influence_node_of_json)
                  json)
           | B (Influence_node, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map
                  (JsonUtil.to_option Public_data.refined_influence_node_of_json)
                  json)
           | B (Influence_nodes, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.nodes_of_influence_map_of_json json)
           | B (Rules_kasa, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.dead_rules_of_json json)
           | B (Agents_kasa, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.json_to_dead_agents json)
           | B (Transitions_kasa, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.separating_transitions_of_json json)
           | B (Constraints_kasa, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread
               (Result_util.map Public_data.lemmas_list_of_json json)
           | B (Polymers_kasa, thread) ->
             let json = read_result Yojson.Basic.read_json p lb in
             Lwt.wakeup thread (Result_util.map Public_data.scc_of_json json)
           | B (DIN, thread) ->
             Lwt.wakeup thread (read_result Data.read_din p lb)
           | B (Plot, thread) ->
             Lwt.wakeup thread (read_result Data.read_plot p lb)
           | B (Snapshot, thread) ->
             Lwt.wakeup thread (read_result Data.read_snapshot p lb)
           | B (Simulation_efficiency, thread) ->
             Lwt.wakeup thread (read_result Counter.Efficiency.read_t p lb)
           | B (Simulation_info, thread) ->
             Lwt.wakeup thread
               (read_result Api_types_j.read_simulation_info p lb)
           | B (Simulation_parameter, thread) ->
             Lwt.wakeup thread
               (read_result Api_types_j.read_simulation_parameter p lb)
           | B (Simulation_artifact, thread) ->
             Lwt.wakeup thread
               (read_result Api_types_j.read_simulation_artifact p lb)
         in
         Hashtbl.remove mailbox id))
    x

let is_computing mailbox = Hashtbl.length mailbox <> 0

class virtual new_client ~is_running ~post mailbox =
  object (self)
    val mutable id = 0

    method private message : type a.
        a handle ->
        (Buffer.t -> unit) ->
        (a, Result_util.message list) Result_util.t Lwt.t =
      fun handle request ->
        if is_running () then (
          let result, feeder = Lwt.task () in
          let message =
            JsonUtil.string_of_write
              (fun b () ->
                JsonUtil.write_sequence b
                  [ (fun b -> Yojson.Basic.write_int b id); request ])
              ()
          in
          let () = post message in
          let () = Hashtbl.replace mailbox id (B (handle, feeder)) in
          let () = id <- succ id in
          result
        ) else
          Lwt.return
            (Result_util.error
               [
                 {
                   Result_util.severity = Logs.Error;
                   Result_util.range = None;
                   Result_util.text = "kamoha agent has died";
                 };
               ])

    (* KaMoHa *)
    method file_delete file_id =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileDelete");
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_update file_id file_content =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileUpdate");
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Yojson.Basic.write_string b file_content);
            ])

    method file_move file_position file_id =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileMove");
              (fun b -> Yojson.Basic.write_int b file_position);
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_get file_id =
      self#message Info (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileGet");
              (fun b -> Yojson.Basic.write_string b file_id);
            ])

    method file_create file_position file_id file_content =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "FileCreate");
              (fun b -> Yojson.Basic.write_int b file_position);
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Yojson.Basic.write_string b file_content);
            ])

    method file_catalog =
      self#message Catalog (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "FileCatalog") ])

    method secret_project_parse : Ast.parsing_compil Api.result Lwt.t =
      Lwt.return
        (Api_common.result_error_msg "low level project_parse mustn't be used")

    method secret_get_pos_of_rules_and_vars
        : Public_data.pos_of_rules_and_vars Api.result Lwt.t =
      Lwt.return
        (Api_common.result_error_msg
           "low level get_pos_of_rules_and_vars mustn't be used")

    method project_overwrite file_id ast =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "ProjectOverwrite");
              (fun b -> Yojson.Basic.write_string b file_id);
              (fun b -> Ast.write_parsing_compil b ast);
            ])

    (* KaSa *)
    method init_static_analyser_raw compil =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "INIT");
              (fun b -> Yojson.Basic.write_string b compil);
            ])

    method init_static_analyser compil =
      self#init_static_analyser_raw
        (Yojson.Basic.to_string (Ast.compil_to_json compil))

    method get_contact_map accuracy =
      self#message JSON (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "CONTACT_MAP");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy));
            ])

    method get_influence_map_raw accuracy =
      self#message String (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "INFLUENCE_MAP");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy));
            ])

    method get_local_influence_map ?fwd ?bwd ?origin ~total accuracy =
      self#message Influence_map (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "LOCAL_INFLUENCE_MAP");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy));
              (fun b -> JsonUtil.write_option Yojson.Basic.write_int b fwd);
              (fun b -> JsonUtil.write_option Yojson.Basic.write_int b bwd);
              (fun b -> Yojson.Basic.write_int b total);
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.short_influence_node_to_json
                     origin));
            ])

    method get_initial_node =
      self#message Influence_node (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b ->
                Yojson.Basic.write_string b "INFLUENCE_MAP_ORIGINAL_NODE");
            ])

    method get_next_node json =
      self#message Influence_node (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "INFLUENCE_MAP_NEXT_NODE");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.short_influence_node_to_json
                     json));
            ])

    method get_previous_node json =
      self#message Influence_node (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b ->
                Yojson.Basic.write_string b "INFLUENCE_MAP_PREVIOUS_NODE");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.short_influence_node_to_json
                     json));
            ])

    method get_influence_map_node_at ~filename pos =
      self#message Short_influence_node (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "INFLUENCE_MAP_NODE_AT");
              (fun b -> Yojson.Basic.write_string b filename);
              (fun b -> Loc.write_position b pos);
            ])

    method get_nodes_of_influence_map accuracy =
      self#message Influence_nodes (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "INFLUENCE_MAP_ALL_NODE");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy));
            ])

    method get_dead_rules =
      self#message Rules_kasa (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "DEAD_RULES") ])

    method get_dead_agents =
      self#message Agents_kasa (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "DEAD_AGENTS") ])

    method get_non_weakly_reversible_transitions =
      self#message Transitions_kasa (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b ->
                Yojson.Basic.write_string b "NON_WEAKLY_REVERSIBLE_TRANSITIONS");
            ])

    method get_constraints_list =
      self#message Constraints_kasa (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "CONSTRAINTS") ])

    method get_potential_polymers accuracy_cm accuracy_scc =
      self#message Polymers_kasa (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "POLYMERS");
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy_cm));
              (fun b ->
                Yojson.Basic.write_json b
                  (JsonUtil.of_option Public_data.accuracy_to_json accuracy_scc));
            ])

    (* KaSim *)
    method secret_simulation_load (_ : Pattern.sharing_level)
        (_ : Ast.parsing_compil) (_ : (string * Nbr.t) list)
        : unit Api.result Lwt.t =
      Lwt.return
        (Api_common.result_error_msg "low level simulation_load mustn't be used")

    method project_parse ~patternSharing overwrites =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "ProjectParse");
              (fun b -> Pattern.write_sharing_level b patternSharing);
              (fun b ->
                JsonUtil.write_list
                  (JsonUtil.write_compact_pair Yojson.Basic.write_string
                     Nbr.write_t)
                  b overwrites);
            ])

    method simulation_continue pause =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationContinue");
              (fun b -> Yojson.Basic.write_string b pause);
            ])

    method simulation_delete =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationDelete") ])

    method simulation_detail_din id =
      self#message DIN (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationDetailDIN");
              (fun b -> Yojson.Basic.write_string b id);
            ])

    method simulation_detail_file_line id =
      self#message Strings (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationDetailFileLine");
              (fun b -> Yojson.Basic.write_string b id);
            ])

    method simulation_detail_snapshot id =
      self#message Snapshot (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationDetailSnapshot");
              (fun b -> Yojson.Basic.write_string b id);
            ])

    method simulation_detail_log_message =
      self#message String (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b ->
                Yojson.Basic.write_string b "SimulationDetailLogMessage");
            ])

    method simulation_detail_plot parameter =
      self#message Plot (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationDetailPlot");
              (fun b -> Api_types_j.write_plot_parameter b parameter);
            ])

    method simulation_catalog_din =
      self#message Strings (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationCatalogDIN") ])

    method simulation_catalog_file_line =
      self#message Strings (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationCatalogFileLine");
            ])

    method simulation_catalog_snapshot =
      self#message Strings (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationCatalogSnapshot");
            ])

    method simulation_efficiency =
      self#message Simulation_efficiency (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationEfficiency") ])

    method simulation_info =
      self#message Simulation_info (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationInfo") ])

    method simulation_intervention intervention =
      self#message String (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationIntervention");
              (fun b -> Yojson.Basic.write_string b intervention);
            ])

    method simulation_outputs_zip =
      self#message BigString (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationOutputsZip") ])

    method simulation_pause =
      self#message Nothing (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationPause") ])

    method simulation_parameter =
      self#message Simulation_parameter (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationParameter") ])

    method simulation_start parameter =
      self#message Simulation_artifact (fun b ->
          JsonUtil.write_sequence b
            [
              (fun b -> Yojson.Basic.write_string b "SimulationStart");
              (fun b -> Api_types_j.write_simulation_parameter b parameter);
            ])

    method simulation_raw_trace =
      self#message String (fun b ->
          JsonUtil.write_sequence b
            [ (fun b -> Yojson.Basic.write_string b "SimulationTrace") ])

    (* method project_parse overwrite = ()

       method raw_launch_story_computation = ()
       method config_story_computation modes = Lwt_result.return ()
       method story_is_computing = false
       method story_list = ()
       method story_log = ()
       method story_progress = ()

       method is_computing = true
         method terminate = ()*)
  end
