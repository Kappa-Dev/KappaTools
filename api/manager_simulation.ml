open Lwt.Infix
(* addd seed to parameter *)
let patch_parameter (simulation_parameter : Api_types_j.simulation_parameter) :
  (Api_types_j.simulation_parameter*int) =
  match simulation_parameter.Api_types_j.simulation_seed with
  | None ->
    let () = Random.self_init () in
    let seed = Random.bits () in
    ({ simulation_parameter with Api_types_j.simulation_seed = Some seed } ,
     seed)
  | Some seed -> (simulation_parameter,seed)


let detail_projection
    ~(project : Api_environment.project)
    ~(system_process:Kappa_facade.system_process)
    ~(projection:(Api_types_j.simulation_detail -> 'a Api.result))
  : 'a Api.result Lwt.t =
  Api_common.bind_simulation
    project
    (fun simulation ->
       let t : Kappa_facade.t = simulation#get_runtime_state () in
       (Kappa_facade.info
          ~system_process:system_process
          ~t:t) >>=
       (Result_util.map
          ~ok:(fun (simulation_detail : Api_types_j.simulation_detail) ->
              Lwt.return (projection simulation_detail):
                (Api_types_j.simulation_detail -> 'a Api.result Lwt.t))
          ~error:((fun (errors : Api_types_j.errors) ->
              Lwt.return (Api_common.result_messages errors)) :
                    Api_types_j.errors -> 'a Api.result Lwt.t)
       )
    )

class manager_file_line
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_file_line =
  object(self)

    method private info_file_line (detail : Api_types_j.simulation_detail) :
      Api_types_j.file_line_catalog Api.result =
      let file_lines : Api_types_j.file_line list =
        detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_file_lines in
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
      let file_line_catalog =
        { Api_types_j.file_line_ids = file_line_ids } in
      Api_common.result_ok file_line_catalog

    method private get_file_line
        (file_line_id : Api_types_j.file_line_id)
        (status : Api_types_j.simulation_detail) :
      (Api_types_j.file_line list) Api.result =
      let file_line_list : Api_types_j.file_line list =
        status.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_file_lines in
      let file_line_eq : Api_types_j.file_line -> bool =
        fun file_line -> file_line_id = file_line.Api_types_j.file_line_name
      in
      match List.filter file_line_eq file_line_list with
      | [] -> let m : string = Format.sprintf "id %s not found"
                  (match file_line_id with Some id -> id | None -> "None")
        in
        Api_common.result_error_msg ~result_code:`Not_found m
      | lines -> Api_common.result_ok lines

    method simulation_catalog_file_line :
      Api_types_j.file_line_catalog Api.result Lwt.t =
      detail_projection ~project ~system_process ~projection:self#info_file_line

    method simulation_detail_file_line
        (file_line_id : Api_types_j.file_line_id) :
      (Api_types_j.file_line list) Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_file_line file_line_id)

  end;;

class manager_flux_map
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_flux_map =
  object(self)
    method private info_flux_map (detail : Api_types_j.simulation_detail) :
      Api_types_j.flux_map_catalog Api.result =
      let flux_maps : Api_types_j.flux_map list =
        detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_flux_maps in
      let flux_map_catalog =
        { Api_types_j.flux_map_ids =
            List.map (fun f -> f.Api_types_j.flux_data.Api_types_j.flux_name)
              flux_maps } in
      Api_common.result_ok flux_map_catalog

    method private get_flux_map
      (flux_map_id : Api_types_j.flux_map_id)
      (detail : Api_types_j.simulation_detail) :
      Api_types_j.flux_map Api.result =
      let flux_maps_list : Api_types_j.flux_map list =
        detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_flux_maps
      in
      let flux_maps_eq : Api_types_j.flux_map -> bool =
        fun flux_map ->
          flux_map_id = flux_map.Api_types_j.flux_data.Api_types_j.flux_name
      in
      try Api_common.result_ok (List.find flux_maps_eq flux_maps_list)
      with Not_found ->
      let m : string = Format.sprintf "id %s not found" flux_map_id in
      Api_common.result_error_msg ~result_code:`Not_found m


    method simulation_catalog_flux_map :
      Api_types_j.flux_map_catalog Api.result Lwt.t =
      detail_projection ~project ~system_process ~projection:self#info_flux_map

    method simulation_detail_flux_map
        (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_flux_map flux_map_id)

  end;;

class manager_log_message
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_log_message =
  object(self)

    method private log_message (detail : Api_types_j.simulation_detail) :
      Api_types_j.log_message Api.result =
      Api_common.result_ok detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_log_messages

    method simulation_detail_log_message :
      Api_types_j.log_message Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:self#log_message
  end

let select_observables
    (plot_limit : Api_types_j.plot_limit)
    (plot : Api_types_j.plot) : Api_types_j.plot_detail =
  let plot_time_series = Array.of_list (List.rev plot.Api_types_j.plot_time_series) in
  let plot_detail_size = Array.length plot_time_series in
  let plot_limit_offset = plot_limit.Api_types_j.plot_limit_offset in
  let plot_limit_points = plot_limit.Api_types_j.plot_limit_points in
  if (match plot_limit_offset with
      | None -> false
      | Some plot_limit_offset -> plot_limit_offset > plot_detail_size) then
    { Api_types_j.plot_detail_plot = { plot with Api_types_j.plot_time_series = [] } ;
      Api_types_j.plot_detail_range = None ;
      Api_types_j.plot_detail_size = plot_detail_size ;
    }
  else
    let start : int =
      match plot_limit_offset with
      | None -> 0
      | Some plot_limit_offset -> plot_limit_offset
    in
    let default_size : int = max 0 (plot_detail_size - start) in
    let len : int =
      match plot_limit_points with
      | None -> default_size
      | Some plot_limit_points -> min plot_limit_points default_size in
    let new_plot_time_series = (List.rev (Array.to_list (Array.sub plot_time_series start len))) in
    let plot_detail_plot = { plot with Api_types_j.plot_time_series = new_plot_time_series }  in
    let plot_detail_range : Api_types_j.plot_range option =
      if len > 0 then
        Some { Api_types_j.plot_range_begin = start ;
               Api_types_j.plot_range_end = start + len ; }
      else
        None
    in
    { Api_types_j.plot_detail_plot = plot_detail_plot ;
      Api_types_j.plot_detail_range = plot_detail_range ;
      Api_types_j.plot_detail_size = plot_detail_size ; }

class manager_plot
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) :
  Api.manager_plot =
  object(self)
    method private get_plot
        (plot_parameter : Api_types_j.plot_parameter)
        (detail : Api_types_j.simulation_detail) :
      Api_types_j.plot_detail Api.result =
      match detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_plot with
      | Some plot ->
        let plot_detail_size = List.length plot.Api_types_j.plot_time_series  in
        Api_common.result_ok
          (match  plot_parameter.Api_types_j.plot_parameter_plot_limit with
           | None ->
             { Api_types_j.plot_detail_plot = plot ;
               Api_types_j.plot_detail_range =
               Some { Api_types_j.plot_range_begin = 0 ;
                      Api_types_j.plot_range_end = plot_detail_size - 1 ; } ;
               Api_types_j.plot_detail_size = plot_detail_size ; }
           | Some plot_limit ->
             select_observables
               plot_limit
               plot
          )
      | None -> let m : string = "plot not available" in
        Api_common.result_error_msg ~result_code:`Not_found m

    method simulation_detail_plot
        (plot_parameter : Api_types_j.plot_parameter) :
      Api_types_j.plot_detail Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_plot plot_parameter)
  end

class manager_snapshot
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) :
  Api.manager_snapshot =
  object(self)
    method private info_snapshot (detail : Api_types_j.simulation_detail) :
      Api_types_j.snapshot_catalog Api.result =
      let snapshots : Api_types_j.snapshot list =
        detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_snapshots in
      let snapshot_catalog =
        { Api_types_j.snapshot_ids =
            List.map (fun s -> s.Api_types_j.snapshot_file) snapshots } in
      Api_common.result_ok snapshot_catalog
    method private get_snapshot
        (snapshot_id : Api_types_j.snapshot_id)
        (detail : Api_types_j.simulation_detail)
      : Api_types_j.snapshot Api.result =
      let snapshot_list : Api_types_j.snapshot list =
        detail.Api_types_j.simulation_detail_output.Api_types_j.simulation_output_snapshots
      in
      let snapshot_eq : Api_types_j.snapshot -> bool =
        fun snapshot -> snapshot_id = snapshot.Api_types_j.snapshot_file
      in
      try Api_common.result_ok (List.find snapshot_eq snapshot_list)
      with Not_found ->
        let m : string = Format.sprintf "id %s not found" snapshot_id in
        Api_common.result_error_msg ~result_code:`Not_found m

    method simulation_catalog_snapshot :
      Api_types_j.snapshot_catalog Api.result Lwt.t =
      (detail_projection
         ~project ~system_process ~projection:self#info_snapshot
       : Api_types_j.snapshot_catalog Api.result Lwt.t)

    method simulation_detail_snapshot
        (snapshot_id : Api_types_j.snapshot_id):
      Api_types_j.snapshot Api.result Lwt.t =
      ((detail_projection
          ~project ~system_process ~projection:(self#get_snapshot snapshot_id))
       : Api_types_j.snapshot Api.result Lwt.t)
  end

class manager_simulation
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) :
  Api.manager_simulation =
  object(self)
    method simulation_delete : unit Api.result Lwt.t =
      self#simulation_stop >>=
      (fun _ ->
         let () = project#unset_simulation () in
         Lwt.return (Api_common.result_ok ()) : (unit, Api.manager_code)
             Api_types_t.result -> unit Api.result Lwt.t)

    method simulation_start
        (simulation_parameter : Api_types_j.simulation_parameter) :
      Api_types_j.simulation_artifact Api.result Lwt.t =
      let (simulation_parameter,simulation_seed) =
        patch_parameter simulation_parameter in
      let simulation_id = simulation_parameter.Api_types_j.simulation_id in
      match project#get_simulation () with
      | Some _ ->
        let message : string =
          Format.sprintf
            "simulation id %s exists" simulation_id in
        Lwt.return
          (Api_common.result_error_msg ~result_code:`Conflict message)
      | None ->
        project#get_state () >>= function
        | None ->
          Lwt.return (Api_common.result_error_msg
                        "Cannot start simulation: Parse not done")
        | Some parse ->
          Result_util.map
            ~ok:
              (fun (facade : Kappa_facade.t) ->
                 let facade = Kappa_facade.clone_t facade in
                 (Kappa_facade.start
                    ~system_process
                    ~parameter:simulation_parameter
                    ~t:facade)
                 >>=
                 (Result_util.map
                    ~ok:
                      (fun () ->
                         let () =
                           project#set_simulation simulation_parameter facade in
                         Lwt.return
                           (Api_common.result_ok
                              { Api_types_j.simulation_artifact_simulation_id = simulation_id ;
                                Api_types_j.simulation_artifact_simulation_seed = simulation_seed ;
                              }))
                    ~error:
                      (fun (errors : Api_types_j.errors) ->
                         Lwt.return (Api_common.result_messages errors)))
              )
            ~error:
              (fun (errors : Api_types_j.errors) ->
                 Lwt.return (Api_common.result_messages errors))
            parse

    method simulation_parameter :
      Api_types_j.simulation_parameter Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let parameter = simulation#get_simulation_parameter() in
           Lwt.return (Api_common.result_ok parameter))

    method simulation_raw_trace : string Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation ->
           let t = simulation#get_runtime_state() in
           Lwt.return (Api_common.result_ok
                         (Kappa_facade.get_raw_trace t)))

    method simulation_pause : unit Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.pause ~system_process ~t) >>=
           (fun _ ->
             Lwt.return (Api_common.result_ok ())))

    method private simulation_stop : unit Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.stop ~system_process ~t) >>=
           (Result_util.map
              ~ok:((fun () ->
                  Lwt.return (Api_common.result_ok ())))
              ~error:((fun (errors : Api_types_j.errors) ->
                       Lwt.return (Api_common.result_messages errors)) :
                        Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_perturbation
        (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.perturbation
              ~system_process:system_process
              ~t:t
              ~perturbation:simulation_perturbation) >>=
           (Result_util.map
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_continue
        (simulation_parameter : Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.continue
              ~system_process:system_process
              ~t:t
              ~parameter:simulation_parameter) >>=
           (Result_util.map
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_info : Api_types_j.simulation_info Api.result Lwt.t =
      Api_common.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.info
              ~system_process:system_process
              ~t:t) >>=
           (Result_util.map
              ~ok:(fun (simulation_detail : Api_types_j.simulation_detail) ->
                  Lwt.return
                    (Api_common.result_ok
                       (Api_data.api_simulation_status simulation_detail))
                )
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors ->
                 Api_types_j.simulation_info Api.result Lwt.t)
           )
        )

    method simulation_efficiency =
      Api_common.bind_simulation project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           Lwt.return (Api_common.result_ok (Kappa_facade.efficiency t)))

    inherit  manager_file_line project system_process
    inherit  manager_flux_map project system_process
    inherit  manager_log_message project system_process
    inherit  manager_plot project system_process
    inherit  manager_snapshot project system_process
  end
