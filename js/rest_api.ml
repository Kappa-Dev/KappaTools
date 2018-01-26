(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
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

let send
    ?(timeout : float option)
    request_count
    (url : string)
    (meth : Common.meth)
    ?(data : string option)
    (hydrate : string -> 'a)
  : 'a Api.result Lwt.t =
  let reply,feeder = Lwt.task () in
  let handler status response_text =
    let result_code : Api.manager_code option =
      match status with
      | 200 -> Some `OK
      | 201 -> Some `Created
      | 202 -> Some `Accepted
      | 400 -> Some `Bad_request
      | 404 -> Some `Not_found
      | 408 -> Some `Request_timeout
      | 409 -> Some `Conflict
      | _ -> None in
    let result =
      match result_code with
      | None ->
        Api_common.result_error_exception (BadResponseCode status)
      | Some result_code ->
        if (400 <= status) && (status < 500) then
          Api_common.result_messages
            ~result_code
            (Api_types_j.errors_of_string response_text)
        else
          let response = hydrate response_text in
          Api_common.result_ok response in
    let () = request_down request_count in
    let () = Lwt.wakeup feeder result in ()
  in
  let () = request_up request_count in
  let () = Common.ajax_request ~url ~meth ?timeout ?data ~handler in
  reply

class manager
    ?(timeout:float option)
    ~url ~project_id =
  let request_count = ref 0 in
  object(self)
    method message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t =
    function
    | `FileCreate file ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `POST
        ~data:(Api_types_j.string_of_file file)
        (fun result ->
           (`FileCreate (Mpi_message_j.file_metadata_of_string result)))
    | `FileDelete file_id ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `DELETE
        (fun _ -> `FileDelete)
    | `FileGet file_id ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `GET
        (fun result ->
           (`FileGet (Mpi_message_j.file_of_string result)))
    | `FileCatalog ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `GET
        (fun result ->
           (`FileCatalog (Mpi_message_j.file_catalog_of_string result)))
    | `FileUpdate (file_id,file_modification) ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `PUT
        ~data:(Api_types_j.string_of_file_modification file_modification)
        (fun result ->
             (`FileUpdate (Mpi_message_j.file_metadata_of_string result)))
    | `ProjectParse (overwrite) ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s/parse" url project_id)
        `POST
        ~data:(Yojson.Safe.to_string
                 (`List (List.map (fun x ->
                      `Assoc ["var",`String x.Api_types_j.overwrite_var;
                              "val",(Nbr.to_yojson x.Api_types_j.overwrite_val
                                     :> Yojson.Safe.json)])
                           overwrite)))
        (fun result ->
             (`ProjectParse (Mpi_message_j.project_parse_of_string result)))
    | `SimulationContinue pause_condition ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/continue"
           url project_id)
        `PUT
        ~data:(Yojson.Safe.to_string (`String pause_condition))
        (fun _ -> (`SimulationContinue))
    | `SimulationDelete ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `DELETE
        (fun _ -> (`SimulationDelete))
    | `SimulationDetailFileLine file_line_id ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines/%s"
           url
           project_id
           file_line_id)
        `GET
        (fun result ->
           let lines =
             Yojson.Safe.read_list
               Yojson.Safe.read_string
               (Yojson.Safe.init_lexer ()) (Lexing.from_string result) in
           (`SimulationDetailFileLine lines))
    | `SimulationDetailDIN flux_map_id ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/DIN/%s"
           url
           project_id
           flux_map_id)
        `GET
        (fun result ->
             (`SimulationDetailDIN (Mpi_message_j.din_of_string result)))
    | `SimulationDetailLogMessage ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/logmessages"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationDetailLogMessage
                        (Mpi_message_j.log_message_of_string result)))
    | `SimulationDetailPlot plot_limit ->
      let args =
        String.concat
          "&"
          (List.map
             (fun (key,value) -> Format.sprintf "%s=%s" key value)
             ((match plot_limit.Api_types_j.plot_limit_offset with
                 | None -> []
                 | Some plot_limit_offset -> [("plot_limit_offset",string_of_int plot_limit_offset)])
              @
              (match plot_limit.Api_types_j.plot_limit_points with
               | None -> []
               | Some plot_limit_points -> [("plot_limit_points",string_of_int plot_limit_points)])
             )) in
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/plot"
           url
           project_id)
        `GET
        ~data:args
        (fun result ->
             (`SimulationDetailPlot (Mpi_message_j.plot_of_string result)))
    | `SimulationDetailSnapshot snapshot_id ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/snapshots/%s"
           url
           project_id
           snapshot_id)
        `GET
        (fun result ->
           (`SimulationDetailSnapshot
              (Mpi_message_j.snapshot_detail_of_string result)))
    | `SimulationInfo ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `GET
        (fun result ->
             (`SimulationInfo (Mpi_message_j.simulation_info_of_string result)))
    | `SimulationEfficiency ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/efficiency"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationEfficiency
                        (Mpi_message_j.simulation_efficiency_of_string result)))
    | `SimulationTrace ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/trace"
           url
           project_id)
        `GET
        (fun s -> (`SimulationTrace s))
    | `SimulationOutputsZip ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/outputs"
           url
           project_id)
        `GET
        (fun s -> (`SimulationOutputsZip s))
    | `SimulationCatalogFileLine ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationCatalogFileLine
                        (Mpi_message_j.file_line_catalog_of_string result)))
    | `SimulationCatalogDIN ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/DIN"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationCatalogDIN
                        (Mpi_message_j.din_catalog_of_string result)))
    | `SimulationCatalogSnapshot ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/snapshots"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationCatalogSnapshot
                        (Mpi_message_j.snapshot_catalog_of_string result)))
    | `SimulationPause ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/pause"
           url
           project_id)
        `PUT
        (fun _ -> `SimulationPause)
    | `SimulationParameter ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/parameter"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationParameter
                        (Mpi_message_j.simulation_parameter_of_string result)))
    | `SimulationIntervention simulation_intervention ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation/intervention"
           url
           project_id)
        `PUT
        ~data:(Api_types_j.string_of_simulation_intervention
                 simulation_intervention)
        (fun result ->
           `SimulationIntervention
             (Yojson.Safe.read_string
                (Yojson.Safe.init_lexer ()) (Lexing.from_string result)))
    | `SimulationStart simulation_parameter ->
      send
        ?timeout request_count
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `POST
        ~data:(Api_types_j.string_of_simulation_parameter simulation_parameter)
        (fun result ->
           (`SimulationStart
                        (Mpi_message_j.simulation_artifact_of_string result)))

  inherit Mpi_api.manager_base ()

  method rest_message = function
    | `EnvironmentInfo ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2" url)
        `GET
        (fun result ->
           (`EnvironmentInfo (Mpi_message_j.environment_info_of_string result)))
    | `ProjectCatalog ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects" url)
        `GET
        (fun result ->
           let projects =
             Yojson.Safe.read_list
               Yojson.Safe.read_string
               (Yojson.Safe.init_lexer ()) (Lexing.from_string result) in
           `ProjectCatalog projects)
    | `ProjectCreate project_parameter ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects" url)
        `POST
        ~data:(Api_types_j.string_of_project_parameter project_parameter)
        (fun _ -> `ProjectCreate)
    | `ProjectDelete project_id ->
      send
        ?timeout request_count
        (Format.sprintf "%s/v2/projects/%s" url project_id)
        `DELETE
        (fun _ -> `ProjectDelete)

  method environment_info () :
    Api_types_j.environment_info Api.result Lwt.t =
    self#rest_message `EnvironmentInfo >>=
    Api_common.result_bind_lwt
      ~ok:(function
          | `EnvironmentInfo
              (result : Mpi_message_t.environment_info) ->
            Lwt.return (Api_common.result_ok result)
          | response ->
            Lwt.return
              (Api_common.result_error_exception
                 (BadResponse response)))

    method project_delete project_id: unit Api.result Lwt.t =
      self#rest_message (`ProjectDelete project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectDelete ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response))

          )

    method project_catalog : string list Api.result Lwt.t =
      self#rest_message `ProjectCatalog >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectCatalog result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method project_create
        (project_parameter : Api_types_j.project_parameter) : unit Api.result Lwt.t =
      self#rest_message (`ProjectCreate project_parameter) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectCreate ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

  method terminate =
    Lwt.ignore_result (self#project_delete project_id)

  method is_running = true (*TODO*)
  method init_static_analyser_raw data =
    send
      ?timeout request_count
      (Format.sprintf "%s/v2/projects/%s/analyses" url project_id)
      `PUT ~data
      (fun x ->
         match Yojson.Basic.from_string x with
         | `Null -> ()
         | x ->
           raise
             (Yojson.Basic.Util.Type_error ("Not a KaSa INIT response: ", x)))
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method init_static_analyser compil =
    self#init_static_analyser_raw
      (Yojson.Basic.to_string (Ast.compil_to_json compil))

  method get_contact_map accuracy =
    send
      ?timeout request_count
      (match accuracy with
       | Some accuracy ->
         Format.sprintf "%s/v2/projects/%s/analyses/contact_map?accuracy=%s"
           url project_id (Public_data.accuracy_to_string accuracy)
       | None ->
         Format.sprintf "%s/v2/projects/%s/analyses/contact_map" url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_influence_map accuracy =
    send
      ?timeout request_count
      (match accuracy with
       | Some accuracy ->
         Format.sprintf "%s/v2/projects/%s/analyses/influence_map?accuracy=%s"
           url project_id (Public_data.accuracy_to_string accuracy)
       | None -> Format.sprintf "%s/v2/analyses/influence_map" url)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_local_influence_map accuracy ?fwd ?bwd ?origin ~total =
    send
      ?timeout request_count
      (let s  =
         match accuracy with
         | Some accuracy ->
           "&accuracy="^(Public_data.accuracy_to_string accuracy)
         | None -> ""
       in
       Format.sprintf
         "%s/v2/projects/%s/analyses/influence_map?total=%i%s%s%s%s"
         url project_id total
         (
           match origin with
           | Some (Public_data.Rule i) -> "&origin=_rule_"^(string_of_int i)
           | Some (Public_data.Var i) -> "&origin=_var_"^(string_of_int i)
           | None -> ""
         ) s
         (match fwd with None -> "" | Some i -> "&fwd="^string_of_int i)
         (match bwd with None -> "" | Some i -> "&bwd="^string_of_int i)
      )
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_initial_node =
    send
      ?timeout request_count
      (
       Format.sprintf
         "%s/v2/projects/%s/analyses/influence_map/initial_node"
         url
         project_id
      )
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_next_node short_id_opt =
    send
      ?timeout request_count
      (
        Format.sprintf
          "%s/v2/projects/%s/analyses/influence_map/next_node%s"
          url
          project_id
          (
            match short_id_opt with
            | Some (Public_data.Rule i) -> "_rule_"^(string_of_int i)
            | Some (Public_data.Var i) -> "_var_"^(string_of_int i)
            | None -> ""
          )
      )
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_previous_node short_id_opt =
    send
      ?timeout request_count
      (
        Format.sprintf
          "%s/v2/projects/%s/analyses/influence_map/previous_node%s"
          url
          project_id
          (
            match short_id_opt with
             | Some (Public_data.Rule i) -> "_rule_"^(string_of_int i)
             | Some (Public_data.Var i) -> "_var_"^(string_of_int i)
             | None -> "")
           )
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")


  method get_dead_rules =
    send
      ?timeout request_count
      (Format.sprintf "%s/v2/projects/%s/analyses/dead_rules" url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_non_weakly_reversible_transitions =
    send
      ?timeout request_count
      (Format.sprintf
         "%s/v2/projects/%s/analyses/non_weakly_reversible_transitions"
         url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_constraints_list =
    send
      ?timeout request_count
      (Format.sprintf "%s/v2/projects/%s/analyses/constraints" url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_potential_polymers accuracy_cm accuracy_scc =
    let options =
      match accuracy_cm, accuracy_scc with
      | None, None -> ""
      | Some a, None -> "?accuracy_cm="^(Public_data.accuracy_to_string a)
      | None, Some a -> "?accuracy_scc="^(Public_data.accuracy_to_string a)
      | Some a, Some b -> "?accuracy_cm="^(Public_data.accuracy_to_string a)^"&accuracy_scc="^(Public_data.accuracy_to_string b)
    in
    send
      ?timeout request_count
      (Format.sprintf "%s/v2/projects/%s/analyses/potential_polymers%s" url project_id options )
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method is_computing = is_computing request_count

  method config_story_computation ~none ~weak ~strong : (unit,string) Lwt_result.t =
    let _dontcare = none || weak || strong in
    Lwt.return_error "KaStor in not available through HTTP"

  method raw_launch_story_computation (_:string) : (unit,string) Lwt_result.t =
    Lwt.return_error "KaStor in not available through HTTP"

  method story_log : string list = []
  method story_is_computing = false
  method story_progress : Story_json.progress_bar option = None
end
