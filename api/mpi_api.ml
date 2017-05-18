open Lwt.Infix

exception BadResponse of Mpi_message_j.response_content

let on_message
    (manager: Api.manager)
    (post_message : (string -> unit Lwt.t))
    (text_message : string) : unit Lwt.t =
  let message : Mpi_message_j.request Mpi_message_j.message =
    Mpi_message_j.message_of_string
      Mpi_message_j.read_request text_message
  in
  let handler :
    'a. ('a -> Mpi_message_j.response_content)-> 'a Api.result -> unit Lwt.t =
    (fun pack result ->
       let message :  Mpi_message_j.response Mpi_message_j.message =
         { Mpi_message_j.id = message.Mpi_message_j.id ;
           Mpi_message_j.data =
             Api_common.result_bind
               ~ok:(fun x -> Api_common.result_ok (pack x)) result } in
       let text : string =
         Mpi_message_j.string_of_message
           Mpi_message_j.write_response message
       in
       post_message text)
  in
  match message.Mpi_message_j.data with
  | `EnvironmentInfo () ->
    (manager#environment_info ()) >>=
    (handler (fun result -> `EnvironmentInfo result))
  | `FileCreate (project_id,file) ->
    (manager#file_create project_id file) >>=
    (handler (fun result -> `FileCreate result))
  | `FileDelete (project_id,file_id) ->
    (manager#file_delete project_id file_id) >>=
    (handler (fun result -> `FileDelete result))
  | `FileGet (project_id,file_id) ->
    (manager#file_get project_id file_id) >>=
    (handler (fun result -> `FileGet result))
  | `FileCatalog project_id ->
    (manager#file_catalog project_id) >>=
    (handler (fun result -> `FileCatalog result))
  | `FileUpdate (project_id,file_id,file_modification) ->
    (manager#file_update project_id file_id file_modification) >>=
    (handler (fun result -> `FileUpdate result))
  | `ProjectCreate project_parameter ->
    (manager#project_create project_parameter) >>=
    (handler (fun result -> `ProjectCreate result))
  | `ProjectDelete project_id ->
    (manager#project_delete project_id) >>=
    (handler (fun result -> `ProjectDelete result))
  | `ProjectCatalog () ->
    (manager#project_catalog ()) >>=
    (handler (fun result -> `ProjectCatalog result))
  |  `ProjectGet project_id ->
    (manager#project_get project_id) >>=
    (handler (fun result -> `ProjectGet result))
  |  `ProjectParse project_id ->
    (manager#project_parse project_id) >>=
    (handler (fun result -> `ProjectParse result))
  |  `ProjectDeadRules project_id ->
    (manager#project_dead_rules project_id) >>=
    (handler (fun result -> `ProjectDeadRules result))
  | `SimulationContinue (project_id,simulation_parameter) ->
    (manager#simulation_continue project_id simulation_parameter) >>=
    (handler (fun result -> `SimulationContinue result))
  | `SimulationDelete project_id ->
    (manager#simulation_delete project_id) >>=
    (handler (fun result -> `SimulationDelete result))
  | `SimulationDetailFileLine (project_id,file_line_id) ->
    (manager#simulation_detail_file_line project_id file_line_id) >>=
    (handler (fun result -> `SimulationDetailFileLine result))
  | `SimulationDetailFluxMap (project_id,flux_map_id) ->
    (manager#simulation_detail_flux_map project_id flux_map_id) >>=
    (handler (fun result -> `SimulationDetailFluxMap result))
  | `SimulationDetailLogMessage project_id ->
    (manager#simulation_detail_log_message project_id) >>=
    (handler (fun result -> `SimulationDetailLogMessage result))
  | `SimulationDetailPlot (project_id,plot_parameter) ->
    (manager#simulation_detail_plot project_id plot_parameter) >>=
    (handler (fun result -> `SimulationDetailPlot result))
  | `SimulationDetailSnapshot (project_id,snapshot_id) ->
    (manager#simulation_detail_snapshot project_id snapshot_id) >>=
    (handler (fun result -> `SimulationDetailSnapshot result))
  | `SimulationInfo project_id ->
    (manager#simulation_info project_id) >>=
    (handler (fun result -> `SimulationInfo result))
  | `SimulationEfficiency project_id ->
    (manager#simulation_efficiency project_id) >>=
    (handler (fun result -> `SimulationEfficiency result))
  | `SimulationCatalogFileLine project_id ->
    (manager#simulation_catalog_file_line project_id) >>=
    (handler (fun result -> `SimulationCatalogFileLine result))
  | `SimulationCatalogFluxMap project_id ->
    (manager#simulation_catalog_flux_map project_id) >>=
    (handler (fun result -> `SimulationCatalogFluxMap result))
  | `SimulationCatalogSnapshot project_id ->
    (manager#simulation_catalog_snapshot project_id) >>=
    (handler (fun result -> `SimulationCatalogSnapshot result))
  | `SimulationParameter project_id ->
    (manager#simulation_parameter project_id) >>=
    (handler (fun result -> `SimulationParameter result))
  | `SimulationTrace project_id ->
    (manager#simulation_raw_trace project_id) >>=
    (handler (fun result -> `SimulationTrace result))
  | `SimulationPause project_id ->
    (manager#simulation_pause project_id) >>=
    (handler (fun result -> `SimulationPause result))
  | `SimulationPerturbation (project_id,simulation_perturbation) ->
    (manager#simulation_perturbation project_id simulation_perturbation) >>=
    (handler (fun result -> `SimulationPerturbation result))
  | `SimulationStart (project_id,simulation_parameter) ->
    (manager#simulation_start project_id simulation_parameter) >>=
    (handler (fun result -> `SimulationStart result))


class type virtual manager_base_type =
  object
    method virtual message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    inherit Api.manager
end

class virtual  manager_base () : manager_base_type =
  object(self)

    method virtual message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    method environment_info () :
      Api_types_j.environment_info Api.result Lwt.t =
      self#message (`EnvironmentInfo ())
      >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `EnvironmentInfo
                (result : Mpi_message_t.environment_info) ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_create
        (project_id : Api_types_j.project_id)
        (file : Api_types_j.file) :
      Api_types_j.file_metadata Api.result Lwt.t =
      self#message (`FileCreate (project_id,file)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileCreate result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_delete
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      unit Api.result Lwt.t =
      self#message (`FileDelete (project_id,file_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileDelete result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_get
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      Api_types_j.file Api.result Lwt.t =
      self#message (`FileGet (project_id,file_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileGet result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_catalog
      (project_id : Api_types_j.project_id) :
      Api_types_j.file_catalog Api.result Lwt.t =
      self#message (`FileCatalog project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileCatalog result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_update
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id)
        (file_modification : Api_types_j.file_modification) :
      Api_types_j.file_metadata Api.result Lwt.t =
      self#message (`FileUpdate (project_id,file_id,file_modification)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileUpdate result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method project_create
      (project_parameter : Api_types_j.project_parameter) : unit Api.result Lwt.t =
      self#message (`ProjectCreate project_parameter) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectCreate result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method project_get (project_id : Api_types_j.project_id) : Api_types_j.project Api.result Lwt.t =
      self#message (`ProjectGet project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectGet result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method project_parse (project_id : Api_types_j.project_id) : Api_types_j.project_parse Api.result Lwt.t =
      self#message (`ProjectParse project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectParse result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method project_dead_rules (project_id : Api_types_j.project_id) =
      self#message (`ProjectDeadRules project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectDeadRules result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method project_delete
        (project_id : Api_types_j.project_id) :
      unit Api.result Lwt.t =
      self#message (`ProjectDelete project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectDelete result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response))

          )


    method project_catalog () : Api_types_j.project_catalog Api.result Lwt.t =
      self#message (`ProjectCatalog ()) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectCatalog result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_continue
      (project_id : Api_types_j.project_id)
      (simulation_parameter :Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      self#message (`SimulationContinue
                      (project_id,simulation_parameter)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationContinue result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_delete
      (project_id : Api_types_j.project_id):
      unit Api.result Lwt.t =
      self#message (`SimulationDelete project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDelete result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_file_line
      (project_id : Api_types_j.project_id)
      (file_line_id : Api_types_j.file_line_id) :
      Api_types_j.file_line list Api.result Lwt.t =
      self#message (`SimulationDetailFileLine
                      (project_id,file_line_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFileLine file_line_list ->
              Lwt.return (Api_common.result_ok file_line_list)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_detail_flux_map
      (project_id : Api_types_j.project_id)
      (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      self#message (`SimulationDetailFluxMap
                      (project_id,flux_map_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFluxMap flux_map ->
              Lwt.return (Api_common.result_ok flux_map)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_log_message
      (project_id : Api_types_j.project_id):
      Api_types_j.log_message Api.result Lwt.t =
      self#message (`SimulationDetailLogMessage project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailLogMessage log_message ->
              Lwt.return (Api_common.result_ok log_message)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_plot
        (project_id : Api_types_j.project_id)
        (plot_parameter : Api_types_j.plot_parameter):
      Api_types_j.plot_detail Api.result Lwt.t =
      self#message (`SimulationDetailPlot
                      (project_id,plot_parameter)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailPlot plot ->
              Lwt.return (Api_common.result_ok plot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_snapshot
      (project_id : Api_types_j.project_id)
      (snapshot_id : Api_types_j.snapshot_id) :
      Api_types_j.snapshot Api.result Lwt.t =
      self#message (`SimulationDetailSnapshot
                      (project_id,snapshot_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailSnapshot snapshot ->
              Lwt.return (Api_common.result_ok snapshot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info
      (project_id : Api_types_j.project_id):
      Api_types_j.simulation_info Api.result Lwt.t =
      self#message (`SimulationInfo project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfo simulation_status ->
              Lwt.return (Api_common.result_ok simulation_status)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_efficiency
      (project_id : Api_types_j.project_id):
      Counter.Efficiency.t Api.result Lwt.t =
      self#message (`SimulationEfficiency project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationEfficiency efficiency ->
              Lwt.return (Api_common.result_ok efficiency)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_file_line
      (project_id : Api_types_j.project_id):
      Api_types_j.file_line_catalog Api.result Lwt.t =
      self#message (`SimulationCatalogFileLine project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogFileLine info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_flux_map
      (project_id : Api_types_j.project_id):
      Api_types_j.flux_map_catalog Api.result Lwt.t =
      self#message (`SimulationCatalogFluxMap project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogFluxMap info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_snapshot
      (project_id : Api_types_j.project_id):
      Api_types_j.snapshot_catalog Api.result Lwt.t =
      self#message (`SimulationCatalogSnapshot project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogSnapshot info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_pause
      (project_id : Api_types_j.project_id) :
      unit Api.result Lwt.t =
      self#message (`SimulationPause project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPause result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_raw_trace
      (project_id : Api_types_j.project_id): string Api.result Lwt.t =
      self#message (`SimulationTrace project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationTrace result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_parameter
      (project_id : Api_types_j.project_id):
      Api_types_j.simulation_parameter Api.result Lwt.t =
      self#message (`SimulationParameter project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationParameter result -> Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_perturbation
      (project_id : Api_types_j.project_id)
      (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      self#message (`SimulationPerturbation
                     (project_id,simulation_perturbation)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPerturbation result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_start
      (project_id : Api_types_j.project_id)
      (simulation_parameter : Api_types_j.simulation_parameter)
      : Api_types_j.simulation_artifact Api.result Lwt.t =
      self#message (`SimulationStart
                     (project_id,simulation_parameter)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationStart simulation_id ->
              Lwt.return (Api_common.result_ok simulation_id)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))
  end

module IntMap = Mods.IntMap
type context = { mailboxes : Mpi_message_j.response Lwt.u IntMap.t ;
                 id : int }

class type virtual manager_mpi_type =
  object
    method virtual post_message : string -> unit
    method virtual sleep : float -> unit Lwt.t
    method virtual post_message : string -> unit
    method message : Mpi_message_j.request -> Mpi_message_j.response Lwt.t
    method receive : string -> unit

    inherit Api.manager
  end

class virtual manager () : manager_mpi_type =
  object(self)
    val mutable context =
      { mailboxes = IntMap.empty ; id = 0 }

    method virtual sleep : float -> unit Lwt.t
    method virtual post_message : string -> unit

    method receive (response_text : string) =
      let message : Mpi_message_j.response Mpi_message_j.message =
        Mpi_message_j.message_of_string
          Mpi_message_j.read_response response_text in
      match IntMap.pop message.Mpi_message_j.id context.mailboxes with
      | Some value, mailboxes ->
        let () = context <- { context with mailboxes } in
        Lwt.wakeup value message.Mpi_message_j.data
      | None, mailboxes -> context <- { context with mailboxes }

    method message (request : Mpi_message_j.request) :
      Mpi_message_j.response Lwt.t =
      let result,feeder = Lwt.task () in
      let () = context <- { context with id = context.id + 1 } in
      let message : Mpi_message_j.request Mpi_message_j.message =
        { Mpi_message_j.id = context.id ;
          Mpi_message_j.data = request } in
      let message_text : string =
        Mpi_message_j.string_of_message
          Mpi_message_j.write_request message
      in
      let () = self#post_message message_text in
      let () = context <-
          { context with
            mailboxes = IntMap.add context.id feeder context.mailboxes } in
      result

    inherit manager_base ()
  end
