open Lwt.Infix

exception BadResponseCode of int
exception TimeOut


type meth = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

let send :
  float ->
  string ->
  meth ->
  string option ->
  (string -> 'a) ->
  ('a Api.result -> Mpi_message_j.response) ->
  Mpi_message_j.response Api.result Lwt.t =
  fun
    (timeout : float)
    (url : string)
    (meth : meth)
    (data : string option)
    (hydrate : string -> 'a)
    (wrap : 'a Api.result -> Mpi_message_j.response)
      ->
  let reply,feeder = Lwt.task () in
  let request : XmlHttpRequest.xmlHttpRequest Js.t = XmlHttpRequest.create () in
  let method_ : string = match meth with
      `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `OPTIONS -> "OPTIONS"
    | `PATCH -> "PATCH"
    | `POST -> "POST"
    | `PUT -> "PUT"
  in
  let () = match data with
    | None -> ()
    | Some data ->
      let data = Url.urlencode data in
      let () = request##setRequestHeader
          (Js.string "Content-type")
          (Js.string "application/xml; charset=utf-8")
      in
      let () = request##setRequestHeader
          (Js.string "Content-type")
          (Js.string "application/xml; charset=utf-8")
      in
      let () = request##setRequestHeader
          (Js.string "Content-length")
          (Js.string (string_of_int (String.length data)))
      in
      ()
  in
  let () =
    request##_open
      (Js.string method_)
      (Js.string url)
      Js._false
  in
  let () = request##send
      (match data with
       | None -> Js.null
       | Some data ->
         Js.some
           (Js.string (Url.urlencode data)))

  in
  let () = request##.onreadystatechange :=
      Js.wrap_callback
        (fun _ ->
           match request##.readyState with
           | XmlHttpRequest.DONE ->
             (let result_code : Api.manager_code option =
                match request##.status with
                | 200 -> Some `OK
                | 201 -> Some `CREATED
                | 202 -> Some `ACCEPTED
                | 400 -> Some `ERROR
                | 404 -> Some `NOT_FOUND
                | 409 -> Some `CONFLICT
                | _ -> None
              in
              let result =
                match result_code with
                | None ->
                  (Api_common.result_error_exception
                     (BadResponseCode request##.status))
                | Some result_code ->
                  (let text : string =
                     Js.to_string request##.responseText
                   in
                   let result : 'a Api.result =
                     if (400 <= request##.status) &&
                        (request##.status < 500) then
                       Api_common.result_ok
                         ~result_code:result_code
                         (hydrate text)
                     else
                     Api_common.result_messages
                       ~result_code:result_code
                       (Api_types_j.errors_of_string text)
                   in
                   (Api_common.result_ok (wrap result)))
              in
              Lwt.wakeup feeder result)
           | XmlHttpRequest.UNSENT
           | XmlHttpRequest.OPENED
           | XmlHttpRequest.HEADERS_RECEIVED
           | XmlHttpRequest.LOADING ->
             ())
  in
  Lwt.pick [reply ;
            (Lwt_js.sleep timeout >>=
             (fun () ->
                Lwt.return (Api_common.result_error_exception TimeOut)))]

class manager
    ?(timeout:float = 10.0)
    (url:string) =
  object
  method message :
      Mpi_message_j.request ->
      Mpi_message_j.response Api.result Lwt.t =
      function
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
          (Format.sprintf "%s/v2/projects/%s" url project_id)
          `PUT
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
                           Mpi_message_j.read_file_metadata
                           result)
          (fun result -> `FileCreate result)
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
          (Format.sprintf "%s/v2/projects/%s" url project_id)
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
      | `ProjectInfo  ->
        send
          timeout
          (Format.sprintf "%s/v2/projects" url)
          `GET
          None
          Mpi_message_j.project_info_of_string
          (fun result -> `ProjectInfo result)
      | `SimulationContinue (project_id,simulation_id,simulation_parameter) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s"
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
             "%s/v2/projects/%s/simulation/%s"
             url
             project_id
             simulation_id)
          `PUT
          None
          (fun _ -> ())
          (fun result -> `SimulationDelete result)
      | `SimulationGetDistance (project_id,simulation_id,distance_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/distances/%d"
             url
             project_id
             simulation_id
             distance_id)
          `GET
          None
          Mpi_message_j.distance_of_string
          (fun result -> `SimulationGetDistance result)
      | `SimulationGetFileLine (project_id,simulation_id,file_line_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/filelines/%s"
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
          (fun result -> `SimulationGetFileLine result)
      | `SimulationGetFluxMap (project_id,simulation_id,flux_map_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/fluxmaps/%s"
             url
             project_id
             simulation_id
             flux_map_id)
          `GET
          None
          Mpi_message_j.flux_map_of_string
          (fun result -> `SimulationGetFluxMap result)
      | `SimulationGetLogMessage (project_id,simulation_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/logmessages"
             url
             project_id
             simulation_id)
          `GET
          None
          Mpi_message_j.log_message_detail_of_string
          (fun result -> `SimulationGetLogMessage result)
      | `SimulationGetPlot (project_id,simulation_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/plot"
             url
             project_id
             simulation_id)
          `GET
          None
          Mpi_message_j.plot_of_string
          (fun result -> `SimulationGetPlot result)
      | `SimulationGetSnapshot (project_id,simulation_id,snapshot_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulation/%s/snapshots/%s"
             url
             project_id
             simulation_id
             snapshot_id)
          `GET
          None
          Mpi_message_j.snapshot_of_string
          (fun result -> `SimulationGetSnapshot result)
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
      | `SimulationInfoDistance (project_id,simulation_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulations/%s/distances"
             url
             project_id
             simulation_id)
          `GET
          None
          Mpi_message_j.distance_info_of_string
          (fun result -> `SimulationInfoDistance result)
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
             "%s/v2/projects/%s/simulations/%s"
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
             "%s/v2/projects/%s/simulation/%s"
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
      | `SimulationStop (project_id,simulation_id) ->
        send
          timeout
          (Format.sprintf
             "%s/v2/projects/%s/simulations/%s"
             url
             project_id
             simulation_id)
          `PUT
          None
          (fun _ -> ())
          (fun result -> `SimulationPerturbation result)

    inherit Mpi_api.manager_base ()
  end
