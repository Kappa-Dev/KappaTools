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

let send
    ?(timeout : float option)
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
    let () = Lwt.wakeup feeder result in ()
  in
  let () = Common.ajax_request ~url ~meth ?timeout ?data ~handler in
  reply

class manager
    ?(timeout:float option)
    ~url ~project_id =
  object(self)
  method message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t =
    function
    | `FileCreate file ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `POST
        ~data:(Api_types_j.string_of_file file)
        (fun result ->
           (`FileCreate (Mpi_message_j.file_metadata_of_string result)))
    | `FileDelete file_id ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `DELETE
        (fun _ -> `FileDelete)
    | `FileGet file_id ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `GET
        (fun result ->
           (`FileGet (Mpi_message_j.file_of_string result)))
    | `FileCatalog ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/files" url project_id)
        `GET
        (fun result ->
           (`FileCatalog (Mpi_message_j.file_catalog_of_string result)))
    | `FileUpdate (file_id,file_modification) ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/files/%s" url project_id file_id)
        `PUT
        ~data:(Api_types_j.string_of_file_modification file_modification)
        (fun result ->
             (`FileUpdate (Mpi_message_j.file_metadata_of_string result)))
    | `ProjectParse (overwrite) ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s/parse" url project_id)
        `POST
        ~data:(Yojson.Safe.to_string
                 (`List (List.map (fun (v,n) ->
                      `Tuple [`String v;(Nbr.to_yojson n :> Yojson.Safe.json)])
                           overwrite)))
        (fun result ->
             (`ProjectParse (Mpi_message_j.project_parse_of_string result)))
    | `ProjectGet project_id ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects/%s" url project_id)
        `GET
        (fun result ->
             (`ProjectGet (Mpi_message_j.project_of_string result)))
    | `SimulationContinue simulation_parameter ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/continue"
           url project_id)
        `PUT
        ~data:(Api_types_j.string_of_simulation_parameter
                 simulation_parameter)
        (fun _ -> (`SimulationContinue))
    | `SimulationDelete ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `DELETE
        (fun _ -> (`SimulationDelete))
    | `SimulationDetailFileLine file_line_id ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines/%s"
           url
           project_id
           (match file_line_id with
              None -> ""
            |Some file_line_id -> file_line_id
           ))
        `GET
        (fun result ->
           (`SimulationDetailFileLine
                        (Mpi_message_j.file_line_detail_of_string result)))
    | `SimulationDetailFluxMap flux_map_id ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/fluxmaps/%s"
           url
           project_id
           flux_map_id)
        `GET
        (fun result ->
             (`SimulationDetailFluxMap (Mpi_message_j.flux_map_of_string result)))
    | `SimulationDetailLogMessage ->
      send
        ?timeout
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
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/plot"
           url
           project_id)
        `GET
        ~data:args
        (fun result ->
             (`SimulationDetailPlot (Mpi_message_j.plot_detail_of_string result)))
    | `SimulationDetailSnapshot snapshot_id ->
      send
        ?timeout
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
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation"
           url
           project_id)
        `GET
        (fun result ->
             (`SimulationInfo (Mpi_message_j.simulation_info_of_string result)))
    | `SimulationEfficiency ->
      send
        ?timeout
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
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/trace"
           url
           project_id)
        `GET
        (fun s -> (`SimulationTrace s))
    | `SimulationCatalogFileLine ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/filelines"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationCatalogFileLine
                        (Mpi_message_j.file_line_catalog_of_string result)))
    | `SimulationCatalogFluxMap ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/fluxmaps"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationCatalogFluxMap
                        (Mpi_message_j.flux_map_catalog_of_string result)))
    | `SimulationCatalogSnapshot ->
      send
        ?timeout
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
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/pause"
           url
           project_id)
        `PUT
        (fun _ -> `SimulationPause)
    | `SimulationParameter ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/parameter"
           url
           project_id)
        `GET
        (fun result ->
           (`SimulationParameter
                        (Mpi_message_j.simulation_parameter_of_string result)))
    | `SimulationPerturbation simulation_perturbation ->
      send
        ?timeout
        (Format.sprintf
           "%s/v2/projects/%s/simulation/perturbation"
           url
           project_id)
        `PUT
        ~data:(Api_types_j.string_of_simulation_perturbation
                 simulation_perturbation)
        (fun _ -> `SimulationPerturbation)
    | `SimulationStart simulation_parameter ->
      send
        ?timeout
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
        ?timeout
        (Format.sprintf "%s/v2" url)
        `GET
        (fun result ->
           (`EnvironmentInfo (Mpi_message_j.environment_info_of_string result)))
    | `ProjectCatalog ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects" url)
        `GET
        (fun result ->
             (`ProjectCatalog (Mpi_message_j.project_catalog_of_string result)))
    | `ProjectCreate project_parameter ->
      send
        ?timeout
        (Format.sprintf "%s/v2/projects" url)
        `POST
        ~data:(Api_types_j.string_of_project_parameter project_parameter)
        (fun _ -> `ProjectCreate)
    | `ProjectDelete project_id ->
      send
        ?timeout
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

    method project_delete
        (project_id : Api_types_j.project_id) :
      unit Api.result Lwt.t =
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

    method project_catalog : Api_types_j.project_catalog Api.result Lwt.t =
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
      ?timeout
      (Format.sprintf "%s/v2/projects/%s/analyses" url project_id)
      `POST ~data
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
      ?timeout
      (match accuracy with
       | Some accuracy ->
         Format.sprintf "%s/v2/projects/%s/analyses/contact_map/%s" url
           project_id
           (Yojson.Basic.to_string (Public_data.accuracy_to_json accuracy))
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
      ?timeout
      (match accuracy with
       | Some accuracy ->
         Format.sprintf "%s/v2/projects/%s/analyses/influence_map/%s" url
           project_id
           (Yojson.Basic.to_string (Public_data.accuracy_to_json accuracy))
       | None -> Format.sprintf "%s/v2/analyses/influence_map" url)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method get_dead_rules =
    send
      ?timeout
      (Format.sprintf "%s/v2/projects/%s/analyses/dead_rules" url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")

  method  get_constraints_list =
    send
      ?timeout
      (Format.sprintf "%s/v2/projects/%s/analyses/constraints" url project_id)
      `GET
      (fun x -> Yojson.Basic.from_string x)
    >>= Api_common.result_map
      ~ok:(fun _ x -> Lwt.return_ok x)
      ~error:(fun _ -> function
          | e :: _ -> Lwt.return_error e.Api_types_t.message_text
          | [] -> Lwt.return_error "Rest_api empty error")
end
