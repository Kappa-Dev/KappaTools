open Lwt

let status_projection :
  environment:Api_environment.environment ->
  system_process:Kappa_facade.system_process ->
  project_id:Api_types_j.project_id ->
  simulation_id:Api_types_j.simulation_id ->
  projection:(Api_types_j.simulation_status -> 'a Api.result) ->
  'a Api.result Lwt.t
  =
  (fun
    ~(environment:Api_environment.environment)
    ~(system_process:Kappa_facade.system_process)
    ~(project_id:Api_types_j.project_id)
    ~(simulation_id:Api_types_j.simulation_id)
    ~(projection:(Api_types_j.simulation_status -> 'a Api.result))
    ->
  Api_common.bind_simulation
    environment
    project_id
    simulation_id
    (fun _ simulation ->
       let t : Kappa_facade.t = simulation#get_runtime_state () in
       (Kappa_facade.info
          ~system_process:system_process
          ~t:t) >>=
       (Api_common.result_data_map
          ~ok:((fun (simulation_status : Api_types_j.simulation_status) ->
              Lwt.return (projection simulation_status)):
               (Api_types_j.simulation_status -> 'a Api.result Lwt.t)
            )
          ~error:((fun (errors : Api_types_j.errors) ->
              Lwt.return (Api_common.result_messages errors)) :
                    Api_types_j.errors -> 'a Api.result Lwt.t)
       )
    )
  )
class manager_distance
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_distance =
  object(self)
    method private info_distance (status : Api_types_j.simulation_status) :
      Api_types_j.distance_info Api.result =
      match status.Api_types_j.simulation_status_distances with
      | Some distance ->
        let distance_ids : Api_types_j.distance_id list = List.mapi (fun i _ -> i) distance in
        Api_common.result_ok { Api_types_j.distance_ids = distance_ids }
      | None -> let m : string = "distance not available" in
        Api_common.result_error_msg ~result_code:`NOT_FOUND m

    method private get_distance
        (distance_id : Api_types_j.distance_id)
        (status : Api_types_j.simulation_status) :
      Api_types_j.distance Api.result  =
      match status.Api_types_j.simulation_status_distances with
      | Some distance ->
        (try Api_common.result_ok (List.nth distance distance_id)
         with _ ->
           let m : string = Format.sprintf "id %d not found" distance_id in
           Api_common.result_error_msg ~result_code:`NOT_FOUND m)
      | None -> let m : string = "distance not available" in
        Api_common.result_error_msg ~result_code:`NOT_FOUND m


    method simulation_info_distance
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.distance_info Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#info_distance

    method simulation_get_distance
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (distance_id : Api_types_j.distance_id) :
      Api_types_j.distance Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:(self#get_distance distance_id)

  end;;

class manager_file_line
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_file_line =
  object(self)

    method private info_file_line (status : Api_types_j.simulation_status) :
      Api_types_j.file_line_info Api.result =
      let file_lines : Api_types_j.file_line list =
        status.Api_types_j.simulation_status_file_lines in
      let file_line_ids : Api_types_j.file_line_id list =
        List.fold_left
          (fun acc l ->
             if List.mem l.Api_types_j.file_line_name acc then
               acc
             else
               l.Api_types_j.file_line_name::acc
          )
          []
          file_lines
      in
      let file_line_info =
        { Api_types_j.file_line_ids = file_line_ids } in
      Api_common.result_ok file_line_info

    method private get_file_line
        (file_line_id : Api_types_j.file_line_id)
        (status : Api_types_j.simulation_status) :
      (Api_types_j.file_line list) Api.result =
      let file_line_list : Api_types_j.file_line list =
        status.Api_types_j.simulation_status_file_lines in
      let file_line_eq : Api_types_j.file_line -> bool =
        fun file_line -> file_line_id = file_line.Api_types_j.file_line_name
      in
      match List.filter file_line_eq file_line_list with
      | [] -> let m : string = Format.sprintf "id %s not found"
                  (match file_line_id with Some id -> id | None -> "None")
        in
        Api_common.result_error_msg ~result_code:`NOT_FOUND m
      | lines -> Api_common.result_ok lines

    method simulation_info_file_line
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.file_line_info Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#info_file_line


    method simulation_get_file_line
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (file_line_id : Api_types_j.file_line_id) :
      (Api_types_j.file_line list) Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:(self#get_file_line file_line_id)

  end;;

class manager_flux_map
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_flux_map =
  object(self)
    method private info_flux_map (status : Api_types_j.simulation_status) :
      Api_types_j.flux_map_info Api.result =
      let flux_maps : Api_types_j.flux_map list =
        status.Api_types_j.simulation_status_flux_maps in
      let flux_map_info =
        { Api_types_j.flux_map_ids =
            List.map (fun f -> f.Api_types_j.flux_data.Api_types_j.flux_name) flux_maps } in
      Api_common.result_ok flux_map_info

    method private get_flux_map
      (flux_map_id : Api_types_j.flux_map_id)
        (status : Api_types_j.simulation_status) :
      Api_types_j.flux_map Api.result =
      let flux_maps_list : Api_types_j.flux_map list =
        status.Api_types_j.simulation_status_flux_maps
      in
      let flux_maps_eq : Api_types_j.flux_map -> bool =
        fun flux_map -> flux_map_id = flux_map.Api_types_j.flux_data.Api_types_j.flux_name
      in
      try Api_common.result_ok (List.find flux_maps_eq flux_maps_list)
      with Not_found ->
      let m : string = Format.sprintf "id %s not found" flux_map_id in
      Api_common.result_error_msg ~result_code:`NOT_FOUND m


    method simulation_info_flux_map
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.flux_map_info Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#info_flux_map

    method simulation_get_flux_map
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:(self#get_flux_map flux_map_id)

  end;;

class manager_log_message
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_log_message =
  object(self)

    method private log_message (status : Api_types_j.simulation_status) :
      (Api_types_j.log_message list) Api.result =
      Api_common.result_ok status.Api_types_j.simulation_status_log_messages

    method simulation_get_log_message
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.log_message list Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#log_message

  end;;

class manager_plot
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) :
  Api.manager_plot =
  object(self)

    method private get_plot (status : Api_types_j.simulation_status) :
      Api_types_j.plot Api.result =
      match status.Api_types_j.simulation_status_plot with
      | Some plot -> Api_common.result_ok plot
      | None -> let m : string = "plot not available" in
        Api_common.result_error_msg ~result_code:`NOT_FOUND m

    method simulation_get_plot
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.plot Api.result Lwt.t =
      status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#get_plot
  end;;

class manager_snapshot
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) :
  Api.manager_snapshot =
  object(self)
    method private info_snapshot (status : Api_types_j.simulation_status) :
      Api_types_j.snapshot_info Api.result =
      let snapshots : Api_types_j.snapshot list =
        status.Api_types_j.simulation_status_snapshots in
      let snapshot_info =
        { Api_types_j.snapshot_ids =
            List.map (fun s -> s.Api_types_j.snapshot_file) snapshots } in
      Api_common.result_ok snapshot_info
    method private get_snapshot
        (snapshot_id : Api_types_j.snapshot_id)
        (status : Api_types_j.simulation_status)
      : Api_types_j.snapshot Api.result =
      let snapshot_list : Api_types_j.snapshot list =
        status.Api_types_j.simulation_status_snapshots
      in
      let snapshot_eq : Api_types_j.snapshot -> bool =
        fun snapshot -> snapshot_id = snapshot.Api_types_j.snapshot_file
      in
      try Api_common.result_ok (List.find snapshot_eq snapshot_list)
      with Not_found ->
        let m : string = Format.sprintf "id %s not found" snapshot_id in
        Api_common.result_error_msg ~result_code:`NOT_FOUND m

    method simulation_info_snapshot
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.snapshot_info Api.result Lwt.t =
      (status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:self#info_snapshot
       : Api_types_j.snapshot_info Api.result Lwt.t)

    method simulation_get_snapshot
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (snapshot_id : Api_types_j.snapshot_id):
      Api_types_j.snapshot Api.result Lwt.t =
      ((status_projection
          ~environment:environment
          ~system_process:system_process
          ~project_id:project_id
          ~simulation_id:simulation_id
          ~projection:(self#get_snapshot snapshot_id))
       : Api_types_j.snapshot Api.result Lwt.t)

  end;;

class manager_simulation
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) :
  Api.manager_simulation =
  object(self)
    method simulation_list
        (project_id : Api_types_j.project_id) :
      Api_types_j.simulation_catalog Api.result Lwt.t =
      Api_common.ProjectOperations.bind
        project_id
        environment
        (fun project ->
           let result =
             List.map
               (fun simulation -> simulation#get_simulation_id ())
               (project#get_simulations ())
           in
           Lwt.return (Api_common.result_ok result))

    method simulation_delete
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun project _ ->
           (self#simulation_stop
              project_id
              simulation_id)
           >>=
           (fun _ ->
              let simulation_list : Api_environment.simulation list =
                (project#get_simulations ())
              in
              let simulation_ne : Api_environment.simulation -> bool =
                fun simulation ->
                  (simulation#get_simulation_id ()) <> simulation_id
              in
              let () =
                project#set_simulations
                  (List.filter simulation_ne simulation_list)
              in
              Lwt.return (Api_common.result_ok ())
              : (unit, Api.manager_code)
                  Api_types_t.result -> unit Api.result Lwt.t)
        )

    method simulation_start
        (project_id : Api_types_j.project_id)
        (simulation_parameter : Api_types_j.simulation_parameter) :
      Api_types_j.simulation_id Api.result Lwt.t =
      Api_common.ProjectOperations.bind
        project_id
        environment
        (fun project ->
           let simulation_id = simulation_parameter.Api_types_j.simulation_id in
           if Api_common.SimulationOperations.exists
               simulation_parameter.Api_types_j.simulation_id
               project
           then
             let message : string =
               Format.sprintf
               "simulation id %s exists"
	       (Api_common.SimulationCollection.id_to_string simulation_id)
             in
             Lwt.return
               (Api_common.result_error_msg ~result_code:`CONFLICT message)
           else
             let facade = Kappa_facade.clone_t (project#get_state ()) in
             (Kappa_facade.start
		~system_process:system_process
		~parameter:simulation_parameter
		~t:facade
             )
             >>=
             (Api_common.result_data_map
                ~ok:((fun () ->
                    let simulation = project#create_simulation simulation_id facade in
                    let () =
                      Api_common.SimulationCollection.update
                        project
                        (simulation::(Api_common.SimulationCollection.list project))
                    in
                    Lwt.return (Api_common.result_ok simulation_id)
                  ))
                ~error:((fun errors -> Lwt.return (Api_common.result_messages errors)) :
                   Api_types_t.message list ->
                        Api_types_j.project_id Api.result Lwt.t)
             )
        )
    method simulation_pause
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun _ simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.pause
             ~system_process:system_process
             ~t:t) >>=
           (fun _ ->
             Lwt.return (Api_common.result_ok ())))

    method simulation_stop
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun _ simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.stop ~system_process:system_process ~t:t) >>=
           (Api_common.result_data_map
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) -> Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_perturbation
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun _ simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.perturbation
              ~system_process:system_process
              ~t:t
              ~perturbation:simulation_perturbation) >>=
           (Api_common.result_data_map
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_continue
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id)
        (simulation_parameter : Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun _ simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.continue
              ~system_process:system_process
              ~t:t
              ~parameter:simulation_parameter) >>=
           (Api_common.result_data_map
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_info
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.simulation_info Api.result Lwt.t =
      Api_common.bind_simulation
        environment
        project_id
        simulation_id
        (fun _ simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.info
              ~system_process:system_process
              ~t:t) >>=
           (Api_common.result_data_map
              ~ok:((fun (simulation_status : Api_types_j.simulation_status) ->
                  Lwt.return
                    (Api_common.result_ok
                       (simulation_status.Api_types_j.simulation_status_info))))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> Api_types_j.simulation_info Api.result Lwt.t)
           )
        )

    inherit  manager_distance environment system_process
    inherit  manager_file_line environment system_process
    inherit  manager_flux_map environment system_process
    inherit  manager_log_message environment system_process
    inherit  manager_plot environment system_process
    inherit  manager_snapshot environment system_process
  end;;
