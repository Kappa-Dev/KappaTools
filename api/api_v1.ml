open Lwt.Infix

let msg_token_not_found =
  "token not found"
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_perturbation_context =
  "Invalid runtime state missing missing perturbation context"

let () = Printexc.record_backtrace true

let catch_error : 'a . (Api_types_v1_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
  (function
    |  ExceptionDefn.Syntax_Error e ->
      handler (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Malformed_Decl e ->
      handler  (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Internal_Error error ->
      handler (Api_data_v1.api_location_errors error)
    | Invalid_argument error ->
      handler (Api_data_v1.api_message_errors ("Runtime error "^ error))
    | exn -> handler (Api_data_v1.api_message_errors (Printexc.to_string exn))
  )


class type api_runtime =
  object
    method parse :
      Api_types_v1_j.code ->
      Api_types_v1_j.parse Api_types_v1_j.result Lwt.t
    method start :
      Api_types_v1_j.parameter ->
      Api_types_v1_j.token Api_types_v1_j.result Lwt.t
    method status :
      Api_types_v1_j.token ->
      Api_types_v1_j.state Api_types_v1_j.result Lwt.t
    method list :
      unit ->
      Api_types_v1_j.catalog Api_types_v1_j.result Lwt.t
    method stop :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method perturbate :
      Api_types_v1_j.token ->
      Api_types_v1_j.perturbation ->
      unit Api_types_v1_j.result Lwt.t
    method pause :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method continue :
      Api_types_v1_j.token ->
      Api_types_v1_j.parameter ->
      unit Api_types_v1_j.result Lwt.t
  end;;

module Base : sig

  class virtual base_runtime :
    float -> object
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
      inherit api_runtime
    end;;
end = struct
  module IntMap = Mods.IntMap
  type context = { states : Kappa_facade.t IntMap.t ; id : int }

  let assemble_distance
      (manager : Api.manager)
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (state : Api_types_v1_j.state) : Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_info_distance
       project_id
       simulation_id
    ) >>=
    Api_common.result_bind_lwt
      ~ok:(fun (distance_info : Api_types_j.distance_info) ->
          Api_common.result_fold_lwt
            ~f:(fun result (distance_id : Api_types_j.distance_id) ->
                Api_common.result_bind_lwt
                  ~ok:(fun (state : Api_types_v1_j.state) ->
                      (manager#simulation_get_distance
                         project_id
                         simulation_id
                         distance_id)
                      >>=
                      (Api_common.result_bind_lwt
                         ~ok:(fun (distance : Api_types_j.distance) ->
                             let distance : Api_types_v1_j.distance = Api_data.api_distance distance in
                             Lwt.return
                               (Api_common.result_ok
                                  { state with
                                    Api_types_v1_j.distances =
                                      match state.Api_types_v1_j.distances with
                                      | None ->
                                        Some (distance::[])
                                      | Some distances ->
                                        Some (distance::distances) }))
                      )
                    )
                  result)
            ~id:(Api_common.result_ok state)
            distance_info.Api_types_j.distance_ids
        )

  let assemble_file_line
      (manager : Api.manager)
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (state : Api_types_v1_j.state) : Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_info_file_line
       project_id
       simulation_id
    ) >>=
       Api_common.result_bind_lwt
         ~ok:(fun (file_line_info : Api_types_j.file_line_info) ->
             Api_common.result_fold_lwt
               ~f:(fun result (file_line_id : Api_types_j.file_line_id) ->
                   Api_common.result_bind_lwt
                     ~ok:(fun (state : Api_types_v1_j.state) ->
                          (manager#simulation_get_file_line
                             project_id
                             simulation_id
                             file_line_id)
                          >>=
                             (Api_common.result_bind_lwt
                                ~ok:(fun (file_line : Api_types_j.file_line list) ->
                                    let file_lines : Api_types_v1_j.file_line list =
                                      List.map Api_data.api_files file_line in
                                    Lwt.return
                                      (Api_common.result_ok
                                         { state with
                                           Api_types_v1_j.files =
                                             file_lines@state.Api_types_v1_j.files }))
                          )
                       )
                     result)
               ~id:(Api_common.result_ok state)
               file_line_info.Api_types_j.file_line_ids
              )


  let assemble_flux_map
          (manager : Api.manager)
          (project_id : Api_types_j.project_id)
          (simulation_id : Api_types_j.simulation_id)
          (state : Api_types_v1_j.state) :
    Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_info_flux_map
       project_id
       simulation_id
    ) >>=
       Api_common.result_bind_lwt
         ~ok:(fun (flux_map_info : Api_types_j.flux_map_info) ->
             Api_common.result_fold_lwt
               ~f:(fun result (flux_map_id : Api_types_j.flux_map_id) ->
                   Api_common.result_bind_lwt
                     ~ok:(fun (state : Api_types_v1_j.state) ->
                         (manager#simulation_get_flux_map
                            project_id
                            simulation_id
                            flux_map_id)
                         >>=
                            (Api_common.result_bind_lwt
                               ~ok:(fun (flux_map : Api_types_j.flux_map) ->
                                   let flux_map : Api_types_v1_j.flux_map = Api_data.api_flux_map flux_map in
                                   Lwt.return
                                     (Api_common.result_ok
                                        { state with
                                          Api_types_v1_j.flux_maps =
                                            flux_map::state.Api_types_v1_j.flux_maps }))
                         )
                       )
                     result)
               ~id:(Api_common.result_ok state)
               flux_map_info.Api_types_j.flux_map_ids
           )


  let assemble_log_message
          (manager : Api.manager)
          (project_id : Api_types_j.project_id)
          (simulation_id : Api_types_j.simulation_id)
          (state : Api_types_v1_j.state) : Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_get_log_message
       project_id
       simulation_id
    ) >>=
       Api_common.result_bind_lwt
         ~ok:(fun (log_message : Api_types_j.log_message list) ->
             Lwt.return
               (Api_common.result_ok
                  { state with
                    Api_types_v1_j.log_messages = log_message })
           )

  let assemble_plot
          (manager : Api.manager)
          (project_id : Api_types_j.project_id)
          (simulation_id : Api_types_j.simulation_id)
          (state : Api_types_v1_j.state) : Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_get_plot
       project_id
       simulation_id
    ) >>=
       Api_common.result_bind_lwt
         ~ok:(fun (plot : Api_types_j.plot) ->
             Lwt.return
               (Api_common.result_ok
                  { state with
                    Api_types_v1_j.plot = Some (Api_data.api_plot plot)
                  })
           )



  let assemble_snapshot
          (manager : Api.manager)
          (project_id : Api_types_j.project_id)
          (simulation_id : Api_types_j.simulation_id)
          (state : Api_types_v1_j.state) : Api_types_v1_j.state Api.result Lwt.t =
    (manager#simulation_info_snapshot
       project_id
       simulation_id
    ) >>=
       Api_common.result_bind_lwt
         ~ok:(fun (snapshot_info : Api_types_j.snapshot_info) ->
             Api_common.result_fold_lwt
               ~f:(fun result (snapshot_id : Api_types_j.snapshot_id) ->
                   Api_common.result_bind_lwt
                     ~ok:(fun (state : Api_types_v1_j.state) ->
                         (manager#simulation_get_snapshot
                            project_id
                            simulation_id
                            snapshot_id)
                         >>=
                            (Api_common.result_bind_lwt
                               ~ok:(fun (snapshot : Api_types_j.snapshot) ->
                                   let snapshot : Api_types_v1_j.snapshot = Api_data.api_snapshot snapshot in
                                   Lwt.return
                                     (Api_common.result_ok
                                        { state with
                                          Api_types_v1_j.snapshots =
                                            snapshot::state.Api_types_v1_j.snapshots }))
                         )
                       )
                     result)
               ~id:(Api_common.result_ok state)
               snapshot_info.Api_types_j.snapshot_ids
           )

  let assemble_state
      (manager : Api.manager)
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (info : Api_types_j.simulation_info) : Api_types_v1_j.state Api.result Lwt.t =
    let assemblers = [ assemble_distance manager project_id simulation_id ;
                       assemble_file_line manager project_id simulation_id ;
                       assemble_flux_map manager project_id simulation_id ;
                       assemble_log_message manager project_id simulation_id ;
                       assemble_plot manager project_id simulation_id ;
                       assemble_snapshot manager project_id simulation_id ;
                     ] in
    let state =
      { Api_types_v1_j.plot = None;
        Api_types_v1_j.distances = None;
        Api_types_v1_j.time = info.Api_types_j.simulation_info_time ;
        Api_types_v1_j.time_percentage = info.Api_types_j.simulation_info_time_percentage ;
        Api_types_v1_j.event = info.Api_types_j.simulation_info_event ;
        Api_types_v1_j.event_percentage = info.Api_types_j.simulation_info_event_percentage ;
        Api_types_v1_j.tracked_events = info.Api_types_j.simulation_info_tracked_events ;
        Api_types_v1_j.log_messages = [];
        Api_types_v1_j.snapshots = [];
        Api_types_v1_j.flux_maps = [];
        Api_types_v1_j.files = [] ;
        Api_types_v1_j.is_running = info.Api_types_j.simulation_info_is_running ;
      }
    in
    Api_common.result_fold_lwt
      ~f:(fun result assembler ->
          Api_common.result_bind_lwt
            ~ok:(fun state -> assembler state)
            result)
      ~id:(Api_common.result_ok state)
      assemblers


  class virtual base_runtime min_run_duration =
    object(self)
      method private system_process () : Kappa_facade.system_process =
        object
          method log ?exn:exn (message : string) : unit Lwt.t =
            self#log ?exn:exn message
          method min_run_duration () : float =
            min_run_duration
          method yield () : unit Lwt.t =
            self#yield ()
        end
      (* memoize manager - this is to get arround the
         lack of access of self from a variable.
      *)
      val parse_file_id : Api_types_j.file_id = ""
      val parse_project_id : Api_types_j.project_id = ""
      method private parse_file_metadata () : Api_types_j.file_metadata = {
        Api_types_j.file_metadata_compile = true;
        Api_types_j.file_metadata_hash = None;
        Api_types_j.file_metadata_id = parse_file_id;
        Api_types_j.file_metadata_position = 0;
      }

      val mutable _manager : Api.manager option = None
      method private manager () : Api.manager Api.result Lwt.t =
        match _manager with
        | None ->
          let file : Api_types_j.file = {
            Api_types_j.file_metadata = self#parse_file_metadata () ;
            Api_types_j.file_content = "" ;
          }
          in
          let manager =
            (new Api_runtime.manager
              (self#system_process ()) :> Api.manager) in
          (manager#project_create
             { Api_types_j.project_id = parse_project_id ; })
          >>=
          (Api_common.result_bind_lwt
             ~ok:(fun (project_id : Api_types_j.project_id) ->
                 (manager#file_create
                    project_id
                    file
                 ) >>=
                 (Api_common.result_bind_lwt
                      ~ok:(fun _ ->
                          let () = _manager <- Some manager in
                          Lwt.return (Api_common.result_ok manager))
                 )
               )
          )

        | Some manager ->
          Lwt.return
            (Api_common.result_ok manager)

      val mutable lastyield = Sys.time ()
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t


      val mutable context = { states = IntMap.empty ; id = 0 }
      (* not sure if this is good *)
      val start_time : float = Sys.time ()

      method private time_yield () =
        let t = Sys.time () in
        if t -. lastyield > min_run_duration then
          let () = lastyield <- t in
          self#yield ()
        else Lwt.return_unit

      method parse
          (code : Api_types_v1_j.code) :
        Api_types_v1_j.parse Api_types_v1_j.result Lwt.t =
        self#manager () >>=
           (Api_common.result_bind_lwt
              ~ok:(fun manager ->
                  let file_modification : Api_types_j.file_modification = {
                    Api_types_j.file_modification_compile = None ;
                    Api_types_j.file_modification_id = None ;
                    Api_types_j.file_modification_position = None ;
		    Api_types_j.file_modification_patch = Some {
                      Api_types_j.file_patch_start = None ;
                      Api_types_j.file_patch_end = None;
                      Api_types_j.file_patch_content =  code;
                    };
		    Api_types_j.file_modification_hash = None;
                  } in
                  (manager#file_update
                     parse_project_id
                     parse_file_id
                     file_modification
                   >>=
                   (Api_common.result_bind_lwt
                        ~ok:(fun (result : Api_types_j.file_metadata Api_types_j.file_result) ->
                            let () = ignore (result) in
                            Lwt.return (Api_common.result_ok result.Api_types_j.file_status_contact_map)
                        )
                   )
                  )
                )

        ) >>=
        (Api_common.result_map
             ~ok:(fun _ (contact_map : Api_types_j.contact_map) ->
                 Lwt.return
                   (`Right
                      { Api_types_v1_j.contact_map =
                          Api_data_v1.api_contact_map contact_map })
               )
             ~error:(fun _ errors  ->
                 Lwt.return
                   (`Left
                      (Api_data.api_errors errors)))
        )
      method private new_id () : int =
        let result = context.id + 1 in
        let () = context <- { context with id = context.id + 1 } in
        result


      method start
          (parameter : Api_types_v1_j.parameter) :
        Api_types_v1_j.token Api_types_v1_j.result Lwt.t =
        let current_id = self#new_id () in
        let file_id = string_of_int current_id in
        let project_id = file_id in
        let simulation_id = file_id in
        let file =
          { Api_types_j.file_metadata = {
            Api_types_j.file_metadata_compile = true;
            Api_types_j.file_metadata_hash = None;
            Api_types_j.file_metadata_id = file_id;
            Api_types_j.file_metadata_position = 0; } ;
            Api_types_j.file_content = parameter.Api_types_v1_j.code ; }
        in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#project_create
                    { Api_types_j.project_id = project_id; })
                 >>=
                 (Api_common.result_bind_lwt
                      ~ok:(fun (project_id : Api_types_j.project_id) ->
                          (manager#file_create
                             project_id
                             file)
                          >>=
                          (Api_common.result_bind_lwt
                              ~ok:(fun _ ->
                                  manager#simulation_start
                                    project_id
                                    { Api_types_j.simulation_plot_period =
                                        parameter.Api_types_v1_j.plot_period ;
                                      Api_types_j.simulation_max_time =
                                        parameter.Api_types_v1_j.max_time ;
                                      Api_types_j.simulation_max_events =
                                        parameter.Api_types_v1_j.max_events ;
                                      Api_types_j.simulation_id =
                                        simulation_id ;
                                    })
                          )
                      )

                 )
             )
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ (_ : Api_types_j.simulation_id) -> Lwt.return (`Right current_id))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
         )

      method perturbate
          (token : Api_types_v1_j.token)
          (perturbation : Api_types_v1_j.perturbation) :
        unit Api_types_v1_j.result Lwt.t =
        let project_id = string_of_int token in
        let simulation_id = project_id in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#simulation_perturbation
                    project_id
                    simulation_id
                    { Api_types_j.perturbation_code = perturbation.Api_types_v1_j.perturbation_code ;  }))
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ () -> Lwt.return (`Right ()))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
        )


      method status (token : Api_types_v1_j.token) :
        Api_types_v1_j.state Api_types_v1_j.result Lwt.t =
        let project_id = string_of_int token in
        let simulation_id = project_id in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#simulation_info
                    project_id
                    simulation_id)
                 >>=
                 (Api_common.result_bind_lwt
                      ~ok:(fun info ->
                          assemble_state
                            manager
                            project_id
                            simulation_id
                            info)
                  ))
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ state -> Lwt.return (`Right state))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
         )

      method list () : Api_types_v1_j.catalog Api_types_v1_j.result Lwt.t =
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#project_info ())
                 >>=
                 (Api_common.result_bind_lwt
                      ~ok:(fun (project_info : Api_types_j.project_info) ->
                          Api_common.result_fold_lwt
                            ~f:(fun result (project_id : Api_types_j.project_id) ->
                                Api_common.result_bind_lwt
                                  ~ok:(fun result ->
                                      (manager#simulation_list
                                         project_id)
                                      >>=
                                      (fun result_simulation_list ->
                                   Api_common.result_bind_lwt
                                     ~ok:(fun (simulation_list : Api_types_j.simulation_catalog) ->
                                         Lwt.return (Api_common.result_ok (result@(List.map int_of_string simulation_list))))
                                     result_simulation_list))
                                  result
                              )
                            ~id:(Api_common.result_ok [])
                            project_info
                      )
                 )
               )
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ state -> Lwt.return (`Right state))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
         )

      method pause (token : Api_types_v1_j.token) :
        unit Api_types_v1_j.result Lwt.t =
        let project_id = string_of_int token in
        let simulation_id = project_id in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#simulation_pause
                    project_id
                    simulation_id))
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ () -> Lwt.return (`Right ()))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
        )

      method continue
          (token : Api_types_v1_j.token)
          (parameter : Api_types_v1_j.parameter) :
        unit Api_types_v1_j.result Lwt.t =
        let project_id = string_of_int token in
        let simulation_id = project_id in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#simulation_continue
                    project_id
                    simulation_id
                    (Api_data.api_parameter parameter)
                 ))
        ) >>=
        (Api_common.result_map
             ~ok:(fun _ () -> Lwt.return (`Right ()))
             ~error:(fun _ errors  ->
                     Lwt.return
                       (`Left
                          (Api_data.api_errors errors)))
        )

      method stop (token : Api_types_v1_j.token) : unit Api_types_v1_j.result Lwt.t =
        let project_id = string_of_int token in
        let simulation_id = project_id in
        self#manager () >>=
        (Api_common.result_bind_lwt
             ~ok:(fun manager ->
                 (manager#simulation_stop
                    project_id
                    simulation_id))
        ) >>=
        Api_common.result_map
             ~ok:(fun _ () -> Lwt.return (`Right ()))
             ~error:(fun _ errors  ->
                 Lwt.return
                   (`Left
                      (Api_data.api_errors errors)))

      initializer
        Lwt.async (fun () -> self#log "created runtime")
    end;;

end;;
