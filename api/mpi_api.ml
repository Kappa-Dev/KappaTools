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
  | `FileCreate file ->
    (manager#file_create file) >>=
    (handler (fun result -> `FileCreate result))
  | `FileDelete file_id ->
    (manager#file_delete file_id) >>=
    (handler (fun () -> `FileDelete))
  | `FileGet file_id ->
    (manager#file_get file_id) >>=
    (handler (fun result -> `FileGet result))
  | `FileCatalog ->
    manager#file_catalog >>=
    (handler (fun result -> `FileCatalog result))
  | `FileUpdate (file_id,file_modification) ->
    (manager#file_update file_id file_modification) >>=
    (handler (fun result -> `FileUpdate result))
  |  `ProjectGet project_id ->
    (manager#project_get project_id) >>=
    (handler (fun result -> `ProjectGet result))
  |  `ProjectParse overwrites ->
    manager#project_parse overwrites >>=
    (handler (fun result -> `ProjectParse result))
  | `SimulationContinue simulation_parameter ->
    (manager#simulation_continue simulation_parameter) >>=
    (handler (fun () -> `SimulationContinue))
  | `SimulationDelete ->
    manager#simulation_delete >>=
    (handler (fun () -> `SimulationDelete))
  | `SimulationDetailFileLine file_line_id ->
    (manager#simulation_detail_file_line file_line_id) >>=
    (handler (fun result -> `SimulationDetailFileLine result))
  | `SimulationDetailFluxMap flux_map_id ->
    (manager#simulation_detail_flux_map flux_map_id) >>=
    (handler (fun result -> `SimulationDetailFluxMap result))
  | `SimulationDetailLogMessage ->
    manager#simulation_detail_log_message >>=
    (handler (fun result -> `SimulationDetailLogMessage result))
  | `SimulationDetailPlot plot_parameter ->
    (manager#simulation_detail_plot plot_parameter) >>=
    (handler (fun result -> `SimulationDetailPlot result))
  | `SimulationDetailSnapshot snapshot_id ->
    (manager#simulation_detail_snapshot snapshot_id) >>=
    (handler (fun result -> `SimulationDetailSnapshot result))
  | `SimulationInfo ->
    manager#simulation_info >>=
    (handler (fun result -> `SimulationInfo result))
  | `SimulationEfficiency ->
    manager#simulation_efficiency >>=
    (handler (fun result -> `SimulationEfficiency result))
  | `SimulationCatalogFileLine ->
    manager#simulation_catalog_file_line >>=
    (handler (fun result -> `SimulationCatalogFileLine result))
  | `SimulationCatalogFluxMap ->
    manager#simulation_catalog_flux_map >>=
    (handler (fun result -> `SimulationCatalogFluxMap result))
  | `SimulationCatalogSnapshot ->
    manager#simulation_catalog_snapshot >>=
    (handler (fun result -> `SimulationCatalogSnapshot result))
  | `SimulationParameter ->
    manager#simulation_parameter >>=
    (handler (fun result -> `SimulationParameter result))
  | `SimulationTrace ->
    manager#simulation_raw_trace >>=
    (handler (fun result -> `SimulationTrace result))
  | `SimulationPause ->
    manager#simulation_pause >>=
    (handler (fun () -> `SimulationPause))
  | `SimulationPerturbation simulation_perturbation ->
    (manager#simulation_perturbation simulation_perturbation) >>=
    (handler (fun () -> `SimulationPerturbation))
  | `SimulationStart simulation_parameter ->
    (manager#simulation_start simulation_parameter) >>=
    (handler (fun result -> `SimulationStart result))


class type virtual manager_base_type =
  object
    method private virtual message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    inherit Api.manager
end

class virtual  manager_base () : manager_base_type =
  object(self)

    method private virtual message :
      Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    method file_create
        (file : Api_types_j.file) :
      Api_types_j.file_metadata Api.result Lwt.t =
      self#message (`FileCreate file) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileCreate result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_delete
        (file_id : Api_types_j.file_id) :
      unit Api.result Lwt.t =
      self#message (`FileDelete file_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileDelete ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_get
        (file_id : Api_types_j.file_id) :
      Api_types_j.file Api.result Lwt.t =
      self#message (`FileGet file_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileGet result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_catalog :
      Api_types_j.file_catalog Api.result Lwt.t =
      self#message `FileCatalog >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileCatalog result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_update
        (file_id : Api_types_j.file_id)
        (file_modification : Api_types_j.file_modification) :
      Api_types_j.file_metadata Api.result Lwt.t =
      self#message (`FileUpdate (file_id,file_modification)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileUpdate result ->
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

    method project_parse overwrite : Api_types_j.project_parse Api.result Lwt.t =
      self#message (`ProjectParse overwrite) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectParse result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_continue
      (simulation_parameter :Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      self#message (`SimulationContinue simulation_parameter) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationContinue ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_delete : unit Api.result Lwt.t =
      self#message `SimulationDelete >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDelete ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_file_line
      (file_line_id : string option) :
      Api_types_j.file_line list Api.result Lwt.t =
      self#message (`SimulationDetailFileLine file_line_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFileLine file_line_list ->
              Lwt.return (Api_common.result_ok file_line_list)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_detail_flux_map
      (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      self#message (`SimulationDetailFluxMap flux_map_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFluxMap flux_map ->
              Lwt.return (Api_common.result_ok flux_map)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_log_message :
      Api_types_j.log_message Api.result Lwt.t =
      self#message `SimulationDetailLogMessage >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailLogMessage log_message ->
              Lwt.return (Api_common.result_ok log_message)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_plot
        (plot_parameter : Api_types_j.plot_parameter):
      Api_types_j.plot_detail Api.result Lwt.t =
      self#message (`SimulationDetailPlot plot_parameter) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailPlot plot ->
              Lwt.return (Api_common.result_ok plot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_snapshot
        (snapshot_id : Api_types_j.snapshot_id) :
      Api_types_j.snapshot Api.result Lwt.t =
      self#message (`SimulationDetailSnapshot snapshot_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailSnapshot snapshot ->
              Lwt.return (Api_common.result_ok snapshot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info :
      Api_types_j.simulation_info Api.result Lwt.t =
      self#message `SimulationInfo >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfo simulation_status ->
              Lwt.return (Api_common.result_ok simulation_status)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_efficiency : Counter.Efficiency.t Api.result Lwt.t =
      self#message `SimulationEfficiency >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationEfficiency efficiency ->
              Lwt.return (Api_common.result_ok efficiency)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_file_line :
      Api_types_j.file_line_catalog Api.result Lwt.t =
      self#message `SimulationCatalogFileLine >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogFileLine info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_flux_map :
      Api_types_j.flux_map_catalog Api.result Lwt.t =
      self#message `SimulationCatalogFluxMap >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogFluxMap info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_catalog_snapshot :
      Api_types_j.snapshot_catalog Api.result Lwt.t =
      self#message `SimulationCatalogSnapshot >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationCatalogSnapshot info ->
              Lwt.return (Api_common.result_ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_pause : unit Api.result Lwt.t =
      self#message `SimulationPause >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPause ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_raw_trace : string Api.result Lwt.t =
      self#message `SimulationTrace >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationTrace result ->
              Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_parameter :
      Api_types_j.simulation_parameter Api.result Lwt.t =
      self#message `SimulationParameter >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationParameter result -> Lwt.return (Api_common.result_ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_perturbation
      (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      self#message (`SimulationPerturbation simulation_perturbation) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPerturbation ->
              Lwt.return (Api_common.result_ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_start
      (simulation_parameter : Api_types_j.simulation_parameter)
      : Api_types_j.simulation_artifact Api.result Lwt.t =
      self#message (`SimulationStart simulation_parameter) >>=
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
    method private virtual sleep : float -> unit Lwt.t
    method private virtual post_message : string -> unit
    method private message : Mpi_message_j.request -> Mpi_message_j.response Lwt.t
    method private receive : string -> unit

    inherit Api.manager
    method virtual is_running : bool
  end

class virtual manager () : manager_mpi_type =
  object(self)
    val mutable context = { mailboxes = IntMap.empty ; id = 0 }

    method private virtual sleep : float -> unit Lwt.t
    method private virtual post_message : string -> unit
    method virtual is_running : bool

    method private receive (response_text : string) =
      let message : Mpi_message_j.response Mpi_message_j.message =
        Mpi_message_j.message_of_string
          Mpi_message_j.read_response response_text in
      match IntMap.pop message.Mpi_message_j.id context.mailboxes with
      | Some value, mailboxes ->
        let () = context <- { context with mailboxes } in
        Lwt.wakeup value message.Mpi_message_j.data
      | None, mailboxes -> context <- { context with mailboxes }

    method private message (request : Mpi_message_j.request) :
      Mpi_message_j.response Lwt.t =
      if self#is_running then
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
      else
        Lwt.return
          (Api_common.result_error_msg "Kappa has died")

    inherit manager_base ()
  end
