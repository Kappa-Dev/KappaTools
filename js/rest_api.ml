(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception BadResponseCode of int
exception TimeOut


let send
    ?(timeout : float option)
    (url : string)
    (meth : Common.meth)
    ?(data : string option)
    (hydrate : string -> 'a)
    (wrap : 'a Api.result -> Mpi_message_j.response)
  : Mpi_message_j.response Api.result Lwt.t =
  let reply,feeder = Lwt.task () in
  let handler status response_text =
    let result_code : Api.manager_code option =
      match status with
      | 200 -> Some `OK
      | 201 -> Some `CREATED
      | 202 -> Some `ACCEPTED
      | 400 -> Some `ERROR
      | 404 -> Some `NOT_FOUND
      | 409 -> Some `CONFLICT
      | _ -> None in
    let result : Mpi_message_j.response Api.result =
      match result_code with
      | None ->
        Api_common.result_error_exception (BadResponseCode status)
      | Some result_code ->
        let result : 'a Api.result =
          if (400 <= status) && (status < 500) then
            Api_common.result_messages
              ~result_code:result_code
              (Api_types_j.errors_of_string response_text)
          else
            let response = hydrate response_text in
            let () = Common.debug response in
            Api_common.result_ok
              ~result_code:result_code
              response
        in
        let response : Mpi_message_j.response = wrap result in
        let () = Common.debug response in
        Api_common.result_ok response
    in
    let () = Lwt.wakeup feeder result in ()
  in
  let () =
    Common.ajax_request
      ~url:url
      ~meth:meth
      ?timeout
      ?data
      ~handler:handler in
   reply

class manager
    ?(timeout:float = 30.0)
    (url:string) =
  object
  method message :
      Mpi_message_j.request ->
      Mpi_message_j.response Api.result Lwt.t =
    function
    | `EnvironmentInfo  () ->
      send
        ~timeout
        (Format.sprintf "%s/v2" url)
        `GET
        Mpi_message_j.environment_info_of_string
        (fun result -> `EnvironmentInfo result)
    | `FileCreate (project_id,file) ->
      send
        ~timeout
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `POST
        ~data:(Api_types_j.string_of_file file)
        (fun result ->
           Mpi_message_j.file_metadata_of_string
             result)
        (fun result -> `FileCreate result)
    | `FileDelete (project_id,file_id) ->
      send
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `DELETE
        (fun result ->
           Api_types_j.unit_t_of_string
             result)
        (fun result -> `FileDelete result)
    | `FileGet (project_id,file_id) ->
      send
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `GET
        Mpi_message_j.file_of_string
        (fun result -> `FileGet result)
    | `FileCatalog project_id ->
      send
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `GET
        Mpi_message_j.file_catalog_of_string
        (fun result -> `FileCatalog result)
    | `FileUpdate (project_id,file_id,file_modification) ->
      send
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `PUT
        ~data:(Api_types_j.string_of_file_modification file_modification)
        (fun result ->
           Mpi_message_j.file_metadata_of_string
             result)
        (fun result -> `FileUpdate result)
    | `ProjectCatalog () ->
      send
        (Format.sprintf "%s/v2/projects" url)
        `GET
        Mpi_message_j.project_catalog_of_string
        (fun result -> `ProjectCatalog result)
    | `ProjectCreate project_parameter ->
      send
        (Format.sprintf "%s/v2/projects" url)
        `POST
        ~data:(Api_types_j.string_of_project_parameter project_parameter)
        Api_types_j.unit_t_of_string
        (fun result -> `ProjectCreate result)
    | `ProjectDelete project_id ->
      send
        (Format.sprintf "%s/v2/projects/%s" url project_id)
        `DELETE
        (fun _ -> ())
        (fun result -> `ProjectDelete result)

    | `ProjectParse project_id ->
      send
        (Format.sprintf "%s/v2/projects/%s/parse" url project_id)
        `GET
        Mpi_message_j.project_parse_of_string
        (fun result -> `ProjectParse result)

    | `ProjectDeadRules project_id ->
      send
        (Format.sprintf "%s/v2/projects/%s/dead_rules" url project_id)
        `GET
        (fun s -> Yojson.Safe.read_list
           Yojson.Safe.read_string
           (Yojson.Safe.init_lexer ()) (Lexing.from_string s))
        (fun result -> `ProjectDeadRules result)

    | `ProjectGet project_id ->
      send
        (Format.sprintf "%s/v2/projects/%s" url project_id)
        `GET
        Mpi_message_j.project_of_string
        (fun result -> `ProjectGet result)

    | `SimulationContinue (project_id,simulation_parameter) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/continue"
           url project_id)
        `PUT
        ~data:(Api_types_j.string_of_simulation_parameter
                 simulation_parameter)
        (fun _ -> ())
        (fun result -> `SimulationContinue result)
    | `SimulationDelete project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `DELETE
        (fun _ -> ())
        (fun result -> `SimulationDelete result)
    | `SimulationDetailFileLine (project_id,file_line_id) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines/%s"
           url
           project_id
           (match file_line_id with
              None -> ""
            |Some file_line_id -> file_line_id
           ))
        `GET
        Mpi_message_j.file_line_detail_of_string
        (fun result -> `SimulationDetailFileLine result)
    | `SimulationDetailFluxMap (project_id,flux_map_id) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/fluxmaps/%s"
           url
           project_id
           flux_map_id)
        `GET
        Mpi_message_j.flux_map_of_string
        (fun result -> `SimulationDetailFluxMap result)
    | `SimulationDetailLogMessage project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/logmessages"
           url
           project_id)
        `GET
        Mpi_message_j.log_message_of_string
        (fun result -> `SimulationDetailLogMessage result)
    | `SimulationDetailPlot (project_id,plot_parameters) ->
      let args =
        String.concat
          "&"
          (List.map
             (fun (key,value) -> Format.sprintf "%s=%s" key value)
             (match plot_parameters.Api_types_j.plot_parameter_plot_limit with
              | None -> []
              | Some plot_limit ->
                (match plot_limit.Api_types_j.plot_limit_offset with
                 | None -> []
                 | Some plot_limit_offset -> [("plot_limit_offset",string_of_int plot_limit_offset)])
                @
                (match plot_limit.Api_types_j.plot_limit_points with
                 | None -> []
                 | Some plot_limit_points -> [("plot_limit_points",string_of_int plot_limit_points)])
             )) in
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/plot"
           url
           project_id)
        `GET
        ~data:args
        Mpi_message_j.plot_detail_of_string
        (fun result -> `SimulationDetailPlot result)
    | `SimulationDetailSnapshot (project_id,snapshot_id) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/snapshots/%s"
           url
           project_id
           snapshot_id)
        `GET
        Mpi_message_j.snapshot_detail_of_string
        (fun result -> `SimulationDetailSnapshot result)
    | `SimulationInfo project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `GET
        Mpi_message_j.simulation_info_of_string
        (fun result -> `SimulationInfo result)
    | `SimulationEfficiency project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/efficiency"
           url
           project_id)
        `GET
        Mpi_message_j.simulation_efficiency_of_string
        (fun result -> `SimulationEfficiency result)
    | `SimulationCatalogFileLine project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines"
           url
           project_id)
        `GET
        Mpi_message_j.file_line_catalog_of_string
        (fun result -> `SimulationCatalogFileLine result)
    | `SimulationCatalogFluxMap project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/fluxmaps"
           url
           project_id)
        `GET
        Mpi_message_j.flux_map_catalog_of_string
        (fun result -> `SimulationCatalogFluxMap result)
    | `SimulationCatalogSnapshot project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/snapshots"
           url
           project_id)
        `GET
        Mpi_message_j.snapshot_catalog_of_string
        (fun result -> `SimulationCatalogSnapshot result)
    | `SimulationPause project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/pause"
           url
           project_id)
        `PUT
        (fun _ -> ())
        (fun result -> `SimulationPause result)
    | `SimulationParameter project_id ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/parameter"
           url
           project_id)
        `GET
        Mpi_message_j.simulation_parameter_of_string
        (fun result -> `SimulationParameter result)
    | `SimulationPerturbation
        (project_id,simulation_perturbation) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation/perturbation"
           url
           project_id)
        `PUT
        ~data:(Api_types_j.string_of_simulation_perturbation
                 simulation_perturbation)
        (fun _ -> ())
        (fun result -> `SimulationPerturbation result)
    | `SimulationStart
        (project_id,simulation_parameter) ->
      send
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `POST
        ~data:(Api_types_j.string_of_simulation_parameter simulation_parameter)
        Mpi_message_j.simulation_artifact_of_string
        (fun result -> `SimulationStart result)

  inherit Mpi_api.manager_base ()
  method terminate () = () (*TODO*)
  end
