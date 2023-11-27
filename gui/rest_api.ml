(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception BadResponse of Mpi_message_j.rest_response_content
exception BadResponseCode of int
exception TimeOut

open Lwt.Infix

let request_up v = incr v
let request_down v = decr v
let is_computing v = !v <> 0

let send ?(timeout : float option) request_count (url : string)
    (meth : Common.meth) ?(data : string option) (hydrate : string -> 'a) :
    'a Api.result Lwt.t =
  let reply, feeder = Lwt.task () in
  let handler status response_text =
    let result_code : Result_util.status option =
      match status with
      | 200 -> Some `OK
      | 201 -> Some `Created
      | 202 -> Some `Accepted
      | 400 -> Some `Bad_request
      | 404 -> Some `Not_found
      | 408 -> Some `Request_timeout
      | 409 -> Some `Conflict
      | _ -> None
    in
    let result =
      match result_code with
      | None -> Api_common.result_error_exception (BadResponseCode status)
      | Some result_code ->
        if 400 <= status && status < 500 then
          Api_common.result_messages ~result_code
            (Yojson.Basic.read_list Result_util.read_message
               (Yojson.Safe.init_lexer ())
               (Lexing.from_string response_text))
        else (
          let response = hydrate response_text in
          Result_util.ok response
        )
    in
    let () = request_down request_count in
    let () = Lwt.wakeup feeder result in
    ()
  in
  let () = request_up request_count in
  let () = Common.ajax_request ~url ~meth ?timeout ?data ~handler in
  reply

let kasa_error l =
  Lwt.return_error
    (List.fold_left
       (fun acc m ->
         Exception_without_parameter.add_uncaught_error
           (Exception_without_parameter.build_uncaught_exception
              ~file_name:"rest_api" ~message:m.Result_util.text Exit)
           acc)
       Exception_without_parameter.empty_error_handler l)

class manager ~(timeout : float option) ~url ~project_id : Api.rest_manager =
  let request_count = ref 0 in
  object (self)
    method private message
        : Mpi_message_j.request -> Mpi_message_j.response Lwt.t =
      function
      | `ProjectLoad _ ->
        Lwt.return
          (Api_common.result_error_msg ~result_code:`Bad_request
             "low level project_load mustn't be used over HTTP")
      | `SimulationContinue pause_condition ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/continue" url project_id)
          `PUT
          ~data:(Yojson.Safe.to_string (`String pause_condition))
          (fun _ -> `SimulationContinue)
      | `SimulationDelete ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation" url project_id) `DELETE
          (fun _ -> `SimulationDelete)
      | `SimulationDetailFileLine file_line_id ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/filelines/%s" url
             project_id file_line_id) `GET (fun result ->
            let lines =
              Yojson.Safe.read_list Yojson.Safe.read_string
                (Yojson.Safe.init_lexer ())
                (Lexing.from_string result)
            in
            `SimulationDetailFileLine lines)
      | `SimulationDetailDIN flux_map_id ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/DIN/%s" url project_id
             flux_map_id) `GET (fun result ->
            `SimulationDetailDIN (Mpi_message_j.din_of_string result))
      | `SimulationDetailLogMessage ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/logmessages" url
             project_id) `GET (fun result ->
            `SimulationDetailLogMessage
              (Mpi_message_j.log_message_of_string result))
      | `SimulationDetailPlot plot_limit ->
        let args =
          String.concat "&"
            (List.map
               (fun (key, value) -> Format.sprintf "%s=%s" key value)
               ((match plot_limit.Api_types_j.plot_limit_offset with
                | None -> []
                | Some plot_limit_offset ->
                  [ "plot_limit_offset", string_of_int plot_limit_offset ])
               @
               match plot_limit.Api_types_j.plot_limit_points with
               | None -> []
               | Some plot_limit_points ->
                 [ "plot_limit_points", string_of_int plot_limit_points ]))
        in
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/plot" url project_id)
          `GET ~data:args (fun result ->
            `SimulationDetailPlot (Mpi_message_j.plot_of_string result))
      | `SimulationDetailSnapshot snapshot_id ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/snapshots/%s" url
             project_id snapshot_id) `GET (fun result ->
            `SimulationDetailSnapshot
              (Mpi_message_j.snapshot_detail_of_string result))
      | `SimulationInfo ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation" url project_id) `GET
          (fun result ->
            `SimulationInfo (Mpi_message_j.simulation_info_of_string result))
      | `SimulationEfficiency ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/efficiency" url
             project_id) `GET (fun result ->
            `SimulationEfficiency
              (Mpi_message_j.simulation_efficiency_of_string result))
      | `SimulationTrace ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/trace" url project_id)
          `GET (fun s -> `SimulationTrace s)
      | `SimulationOutputsZip ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/outputs" url project_id)
          `GET (fun s -> `SimulationOutputsZip s)
      | `SimulationCatalogFileLine ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/filelines" url
             project_id) `GET (fun result ->
            `SimulationCatalogFileLine
              (Mpi_message_j.file_line_catalog_of_string result))
      | `SimulationCatalogDIN ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/DIN" url project_id)
          `GET (fun result ->
            `SimulationCatalogDIN (Mpi_message_j.din_catalog_of_string result))
      | `SimulationCatalogSnapshot ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/snapshots" url
             project_id) `GET (fun result ->
            `SimulationCatalogSnapshot
              (Mpi_message_j.snapshot_catalog_of_string result))
      | `SimulationPause ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/pause" url project_id)
          `PUT (fun _ -> `SimulationPause)
      | `SimulationParameter ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/parameter" url
             project_id) `GET (fun result ->
            `SimulationParameter
              (Mpi_message_j.simulation_parameter_of_string result))
      | `SimulationIntervention simulation_intervention ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation/intervention" url
             project_id) `PUT
          ~data:
            (Api_types_j.string_of_simulation_intervention
               simulation_intervention) (fun result ->
            `SimulationIntervention
              (Yojson.Safe.read_string
                 (Yojson.Safe.init_lexer ())
                 (Lexing.from_string result)))
      | `SimulationStart simulation_parameter ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s/simulation" url project_id) `POST
          ~data:
            (Api_types_j.string_of_simulation_parameter simulation_parameter)
          (fun result ->
            `SimulationStart
              (Mpi_message_j.simulation_artifact_of_string result))

    inherit Mpi_api.manager_base ()

    method private rest_message =
      function
      | `EnvironmentInfo ->
        send ?timeout request_count (Format.sprintf "%s/v2" url) `GET
          (fun result ->
            `EnvironmentInfo (Mpi_message_j.environment_info_of_string result))
      | `ProjectCatalog ->
        send ?timeout request_count (Format.sprintf "%s/v2/projects" url) `GET
          (fun result ->
            let projects =
              Yojson.Safe.read_list Yojson.Safe.read_string
                (Yojson.Safe.init_lexer ())
                (Lexing.from_string result)
            in
            `ProjectCatalog projects)
      | `ProjectCreate project_parameter ->
        send ?timeout request_count (Format.sprintf "%s/v2/projects" url) `POST
          ~data:(Api_types_j.string_of_project_parameter project_parameter)
          (fun _ -> `ProjectCreate)
      | `ProjectDelete project_id ->
        send ?timeout request_count
          (Format.sprintf "%s/v2/projects/%s" url project_id) `DELETE (fun _ ->
            `ProjectDelete)

    method environment_info () : Api_types_j.environment_info Api.result Lwt.t =
      self#rest_message `EnvironmentInfo
      >>= Api_common.result_bind_lwt ~ok:(function
            | `EnvironmentInfo (result : Mpi_message_t.environment_info) ->
              Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method project_delete project_id : unit Api.result Lwt.t =
      self#rest_message (`ProjectDelete project_id)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `ProjectDelete -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method project_catalog : string list Api.result Lwt.t =
      self#rest_message `ProjectCatalog
      >>= Api_common.result_bind_lwt ~ok:(function
            | `ProjectCatalog result -> Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method project_create (project_parameter : Api_types_j.project_parameter)
        : unit Api.result Lwt.t =
      self#rest_message (`ProjectCreate project_parameter)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `ProjectCreate -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method secret_project_parse =
      Lwt.return
        (Api_common.result_error_msg ~result_code:`Bad_request
           "low level project_parse mustn't be used over HTTP")

    method secret_get_pos_of_rules_and_vars =
      Lwt.return
        (Api_common.result_error_msg ~result_code:`Bad_request
           "low level get_pos_of_rules_and_vars mustn't be used over HTTP")

    method project_parse ~patternSharing overwrite =
      send ?timeout request_count
        (Format.asprintf "%s/v2/projects/%s/parse/%s%t" url project_id
           (match patternSharing with
           | Pattern.No_sharing -> "no_sharing"
           | Pattern.Compatible_patterns -> "compatible_patterns"
           | Pattern.Max_sharing -> "max_sharing")
           (fun f ->
             match overwrite with
             | [] -> ()
             | l ->
               Format.fprintf f "?%a"
                 (Pp.list
                    (fun f -> Format.pp_print_string f "&")
                    (fun f (vr, va) -> Format.fprintf f "%s=%a" vr Nbr.print va))
                 l))
        `POST
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method terminate = Lwt.ignore_result (self#project_delete project_id)
    method is_running = true (*TODO*)

    method file_catalog =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `GET
        (JsonUtil.read_of_string
           (Yojson.Basic.read_list Kfiles.read_catalog_item))

    method file_create pos id content =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s/position/%i" url project_id
           id pos)
        `PUT
        ~data:(Yojson.Basic.to_string (`String content))
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method file_get id =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id id)
        `GET
        (JsonUtil.read_of_string
           (JsonUtil.read_compact_pair Yojson.Basic.read_string
              Yojson.Basic.read_int))

    method file_update id content =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id id)
        `POST
        ~data:(Yojson.Basic.to_string (`String content))
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method file_move pos id =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s/position/%i" url project_id
           id pos)
        `POST
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method file_delete id =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id id)
        `DELETE
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method project_overwrite file_id ast =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/overwrite/%s" url project_id file_id)
        `POST
        ~data:(Yojson.Basic.to_string (Ast.compil_to_json ast))
        (JsonUtil.read_of_string Yojson.Basic.read_null)

    method init_static_analyser_raw data =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses" url project_id) `PUT ~data
        (fun x ->
          match Yojson.Basic.from_string x with
          | `Null -> ()
          | x ->
            raise
              (Yojson.Basic.Util.Type_error ("Not a KaSa INIT response: ", x)))

    method init_static_analyser compil =
      self#init_static_analyser_raw
        (Yojson.Basic.to_string (Ast.compil_to_json compil))

    method get_contact_map accuracy =
      send ?timeout request_count
        (match accuracy with
        | Some accuracy ->
          Format.sprintf "%s/v2/projects/%s/analyses/contact_map?accuracy=%s"
            url project_id
            (Public_data.accuracy_to_string accuracy)
        | None ->
          Format.sprintf "%s/v2/projects/%s/analyses/contact_map" url project_id)
        `GET
        (fun x -> Yojson.Basic.from_string x)

    method get_influence_map_raw accuracy =
      send ?timeout request_count
        (match accuracy with
        | Some accuracy ->
          Format.sprintf "%s/v2/projects/%s/analyses/influence_map?accuracy=%s"
            url project_id
            (Public_data.accuracy_to_string accuracy)
        | None -> Format.sprintf "%s/v2/analyses/influence_map" url)
        `GET
        (fun x -> x)

    method get_local_influence_map ?fwd ?bwd ?origin ~total accuracy =
      send ?timeout request_count
        (let s =
           match accuracy with
           | Some accuracy ->
             "&accuracy=" ^ Public_data.accuracy_to_string accuracy
           | None -> ""
         in
         Format.sprintf
           "%s/v2/projects/%s/analyses/influence_map?total=%i%s%s%s%s" url
           project_id total
           (match origin with
           | Some (Public_data.Rule i) -> "&origin=_rule_" ^ string_of_int i
           | Some (Public_data.Var i) -> "&origin=_var_" ^ string_of_int i
           | None -> "")
           s
           (match fwd with
           | None -> ""
           | Some i -> "&fwd=" ^ string_of_int i)
           (match bwd with
           | None -> ""
           | Some i -> "&bwd=" ^ string_of_int i))
        `GET
        (fun x ->
          Public_data.local_influence_map_of_json (Yojson.Basic.from_string x))

    method get_initial_node =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/influence_map/initial_node"
           url project_id) `GET (fun x ->
          JsonUtil.to_option Public_data.refined_influence_node_of_json
            (Yojson.Basic.from_string x))

    method get_next_node short_id_opt =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/influence_map/next_node%s"
           url project_id
           (match short_id_opt with
           | Some (Public_data.Rule i) -> "_rule_" ^ string_of_int i
           | Some (Public_data.Var i) -> "_var_" ^ string_of_int i
           | None -> ""))
        `GET
        (fun x ->
          JsonUtil.to_option Public_data.refined_influence_node_of_json
            (Yojson.Basic.from_string x))

    method get_previous_node short_id_opt =
      send ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/analyses/influence_map/previous_node%s" url
           project_id
           (match short_id_opt with
           | Some (Public_data.Rule i) -> "_rule_" ^ string_of_int i
           | Some (Public_data.Var i) -> "_var_" ^ string_of_int i
           | None -> ""))
        `GET
        (fun x ->
          JsonUtil.to_option Public_data.refined_influence_node_of_json
            (Yojson.Basic.from_string x))

    method get_influence_map_node_at ~filename { Loc.line; Loc.chr } =
      send ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/analyses/influence_map/node_at?file=%s&line=%i&chr=%i"
           url project_id filename line chr) `GET (fun x ->
          JsonUtil.to_option Public_data.short_influence_node_of_json
            (Yojson.Basic.from_string x))

    method get_nodes_of_influence_map accuracy =
      send ?timeout request_count
        (match accuracy with
        | Some accuracy ->
          Format.sprintf
            "%s/v2/projects/%s/analyses/all_nodes_of_influence_map?accuracy=%s"
            url project_id
            (Public_data.accuracy_to_string accuracy)
        | None -> Format.sprintf "%s/v2/analyses/all_nodes_of_influence_map" url)
        `GET
        (fun x ->
          Public_data.nodes_of_influence_map_of_json
            (Yojson.Basic.from_string x))

    method get_dead_rules =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/dead_rules" url project_id)
        `GET (fun x ->
          Public_data.dead_rules_of_json (Yojson.Basic.from_string x))

    method get_dead_agents =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/dead_agents" url project_id)
        `GET (fun x ->
          Public_data.json_to_dead_agents (Yojson.Basic.from_string x))

    method get_non_weakly_reversible_transitions =
      send ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/analyses/non_weakly_reversible_transitions" url
           project_id) `GET (fun x ->
          Public_data.separating_transitions_of_json
            (Yojson.Basic.from_string x))

    method get_constraints_list =
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/constraints" url project_id)
        `GET (fun x ->
          Public_data.lemmas_list_of_json (Yojson.Basic.from_string x))

    method get_potential_polymers accuracy_cm accuracy_scc =
      let options =
        match accuracy_cm, accuracy_scc with
        | None, None -> ""
        | Some a, None -> "?accuracy_cm=" ^ Public_data.accuracy_to_string a
        | None, Some a -> "?accuracy_scc=" ^ Public_data.accuracy_to_string a
        | Some a, Some b ->
          "?accuracy_cm="
          ^ Public_data.accuracy_to_string a
          ^ "&accuracy_scc="
          ^ Public_data.accuracy_to_string b
      in
      send ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/analyses/potential_polymers%s" url
           project_id options) `GET (fun x ->
          Public_data.scc_of_json (Yojson.Basic.from_string x))

    method is_computing = is_computing request_count

    method config_story_computation { Api.causal; Api.weak; Api.strong }
        : (unit, string) Lwt_result.t =
      let _dontcare = causal || weak || strong in
      Lwt.return_error "KaStor in not available through HTTP"

    method raw_launch_story_computation (_ : string)
        : (unit, string) Lwt_result.t =
      Lwt.return_error "KaStor in not available through HTTP"

    method story_log : string list = []
    method story_is_computing = false
    method story_progress : Story_json.progress_bar option = None

    method story_list
        : (Api.compression_modes
          * unit Trace.Simulation_info.t list list
          * Graph_loggers_sig.graph)
          Mods.IntMap.t =
      Mods.IntMap.empty
  end
