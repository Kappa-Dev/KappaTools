(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

exception BadResponseCode of int
exception TimeOut


let send
    (timeout : float)
    (url : string)
    (meth : Common.meth)
    (data : string option)
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
            let () = Common.debug "response:hydrated" in
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
      ~timeout
      ~data:data
      ~handler:handler in
  Lwt.pick
    [ reply ;
      (Lwt_js.sleep timeout >>=
       (fun () ->
          Lwt.return
            (Api_common.result_error_exception TimeOut)))]

class manager
    ?(timeout:float = 30.0)
    (url:string) =
  object
  method message :
      Mpi_message_j.request ->
      Mpi_message_j.response Api.result Lwt.t =
    function
    | `ProjectInfo () ->
      send
        timeout
        (Format.sprintf "%s/v2/projects" url)
        `GET
        None
        Mpi_message_j.project_info_of_string
        (fun result -> `ProjectInfo result)
    | `EnvironmentInfo () ->
      send
        timeout
        ""
        `GET
        None
        Mpi_message_j.environment_info_of_string
        (fun result -> `EnvironmentInfo result)
    | `FileCreate (project_id,file) ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `POST
        (Some (Api_types_j.string_of_file file))
        (fun result -> Mpi_message_j.file_result_of_string
            Mpi_message_j.read_file_metadata
            result)
        (fun result -> `FileCreate result)
    | `FileDelete (project_id,file_id) ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `DELETE
        None
        (fun result -> Mpi_message_j.file_result_of_string
            Api_types_j.read_unit_t
            result)
        (fun result -> `FileDelete result)
    | `FileGet (project_id,file_id) ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `GET
        None
        Mpi_message_j.file_of_string
        (fun result -> `FileGet result)
    | `FileInfo project_id ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `GET
        None
        Mpi_message_j.file_info_of_string
        (fun result -> `FileInfo result)
    | `FileUpdate (project_id,file_id,file_modification) ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `PUT
        (Some (Api_types_j.string_of_file_modification file_modification))
        (fun result -> Mpi_message_j.file_result_of_string
            Mpi_message_j.read_file_metadata
            result)
        (fun result -> `FileUpdate result)
    | `ProjectCreate project_parameter ->
      send
        timeout
        (Format.sprintf "%s/v2/projects" url)
        `POST
        (Some (Api_types_j.string_of_project_parameter project_parameter))
        Mpi_message_j.project_id_of_string
        (fun result -> `ProjectCreate result)
    | `ProjectDelete project_id ->
      send
        timeout
        (Format.sprintf "%s/v2/projects/%s" url project_id)
        `DELETE
        None
        (fun _ -> ())
        (fun result -> `ProjectDelete result)
    | `SimulationContinue (project_id,simulation_id,simulation_parameter) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/continue"
           url project_id
           simulation_id)
        `PUT
        (Some
           (Api_types_j.string_of_simulation_parameter
              simulation_parameter))
        (fun _ -> ())
        (fun result -> `SimulationContinue result)
    | `SimulationDelete (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s"
           url
           project_id
           simulation_id)
        `DELETE
        None
        (fun _ -> ())
        (fun result -> `SimulationDelete result)
    | `SimulationDetailFileLine (project_id,simulation_id,file_line_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/filelines/%s"
           url
           project_id
           simulation_id
           (match file_line_id with
              None -> ""
            |Some file_line_id -> file_line_id
           ))
        `GET
        None
        Mpi_message_j.file_line_detail_of_string
        (fun result -> `SimulationDetailFileLine result)
    | `SimulationDetailFluxMap (project_id,simulation_id,flux_map_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/fluxmaps/%s"
           url
           project_id
           simulation_id
           flux_map_id)
        `GET
        None
        Mpi_message_j.flux_map_of_string
        (fun result -> `SimulationDetailFluxMap result)
    | `SimulationDetailLogMessage (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/logmessages"
           url
           project_id
           simulation_id)
        `GET
        None
        Mpi_message_j.log_message_of_string
        (fun result -> `SimulationDetailLogMessage result)
    | `SimulationDetailPlot (project_id,simulation_id,plot_parameters) ->
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
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/plot"
           url
           project_id
           simulation_id)
        `GET
        (Some args)
        Mpi_message_j.plot_detail_of_string
        (fun result -> `SimulationDetailPlot result)
    | `SimulationDetailSnapshot (project_id,simulation_id,snapshot_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/snapshots/%s"
           url
           project_id
           simulation_id
           snapshot_id)
        `GET
        None
        Mpi_message_j.snapshot_of_string
        (fun result -> `SimulationDetailSnapshot result)
    | `SimulationInfo (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s"
           url
           project_id
           simulation_id)
        `GET
        None
        Mpi_message_j.simulation_info_of_string
        (fun result -> `SimulationInfo result)
    | `SimulationInfoFileLine (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/filelines"
           url
           project_id
           simulation_id)
        `GET
        None
        Mpi_message_j.file_line_info_of_string
        (fun result -> `SimulationInfoFileLine result)
    | `SimulationInfoFluxMap (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/fluxmaps"
           url
           project_id
           simulation_id)
        `GET
        None
        Mpi_message_j.flux_map_info_of_string
        (fun result -> `SimulationInfoFluxMap result)
    | `SimulationInfoSnapshot (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/snapshots"
           url
           project_id
           simulation_id)
        `GET
        None
        Mpi_message_j.snapshot_info_of_string
        (fun result -> `SimulationInfoSnapshot result)
    | `SimulationList project_id ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations"
           url
           project_id)
        `GET
        None
        Mpi_message_j.simulation_catalog_of_string
        (fun result -> `SimulationList result)
    | `SimulationPause (project_id,simulation_id) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/pause"
           url
           project_id
           simulation_id)
        `PUT
        None
        (fun _ -> ())
        (fun result -> `SimulationPause result)
    | `SimulationPerturbation
        (project_id,simulation_id,simulation_perturbation) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations/%s/perturbation"
           url
           project_id
           simulation_id)
        `PUT
        (Some
           (Api_types_j.string_of_simulation_perturbation
              simulation_perturbation))
        (fun _ -> ())
        (fun result -> `SimulationPerturbation result)
    | `SimulationStart
        (project_id,simulation_parameter) ->
      send
        timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulations"
           url
           project_id)
        `POST
        (Some
           (Api_types_j.string_of_simulation_parameter simulation_parameter))
        Mpi_message_j.simulation_id_of_string
        (fun result -> `SimulationStart result)

  inherit Mpi_api.manager_base ()
  end
