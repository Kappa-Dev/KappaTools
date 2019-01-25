(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

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
    ~(projection:(Api_data.simulation_detail_output -> 'a Api.result))
  : 'a Api.result Lwt.t =
  Model_storage.bind_simulation
    project
    (fun simulation ->
       let t : Kappa_facade.t = simulation#get_runtime_state () in
       (Kappa_facade.outputs
          ~system_process:system_process
          ~t:t) >>=
       (Result_util.fold
          ~ok:(fun (simulation_detail : Api_data.simulation_detail_output) ->
              Lwt.return (projection simulation_detail):
                (Api_data.simulation_detail_output -> 'a Api.result Lwt.t))
          ~error:((fun (errors : Api_types_j.errors) ->
              Lwt.return (Api_common.result_messages errors)) :
                    Api_types_j.errors -> 'a Api.result Lwt.t)
       )
    )

class manager_file_line
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_file_line =
  object(self)

    method private info_file_line
        (detail : Api_data.simulation_detail_output) :
      Api_types_j.file_line_catalog Api.result =
      let file_lines : string list Mods.StringMap.t =
        detail.Api_types_t.simulation_output_file_lines in
      let file_line_ids : string list =
        List.map fst (Mods.StringMap.bindings file_lines) in
      let file_line_catalog = { Api_types_j.file_line_ids } in
      Api_common.result_ok file_line_catalog

    method private get_file_line
        ~file_line_id
        (status : Api_data.simulation_detail_output) :
      (string list) Api.result =
      let file_line_list = status.Api_types_j.simulation_output_file_lines in
      match Mods.StringMap.find_option file_line_id file_line_list with
      | None ->
        let m : string = Format.sprintf "id %s not found"  file_line_id in
        Api_common.result_error_msg ~result_code:`Not_found m
      | Some lines -> Api_common.result_ok (List.rev lines)

    method simulation_catalog_file_line :
      Api_types_j.file_line_catalog Api.result Lwt.t =
      detail_projection ~project ~system_process ~projection:self#info_file_line

    method simulation_detail_file_line
        (file_line_id : string) : string list Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_file_line ~file_line_id)

  end;;

class manager_flux_map
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_din =
  object(self)
    method private info_flux_map
        (detail : Api_data.simulation_detail_output) :
      Api_types_j.din_catalog Api.result =
      let flux_maps : Api_types_j.din list =
        detail.Api_types_j.simulation_output_dins in
      let flux_map_catalog =
        { Api_types_j.din_ids =
            List.map (fun f -> f.Data.din_data.Data.din_name)
              flux_maps } in
      Api_common.result_ok flux_map_catalog

    method private get_flux_map
      (flux_map_id : Api_types_j.din_id)
      (detail : Api_data.simulation_detail_output) :
      Api_types_j.din Api.result =
      let flux_maps_list : Api_types_j.din list =
        detail.Api_types_j.simulation_output_dins
      in
      let flux_maps_eq : Api_types_j.din -> bool =
        fun flux_map ->
          flux_map_id = flux_map.Data.din_data.Data.din_name
      in
      try Api_common.result_ok (List.find flux_maps_eq flux_maps_list)
      with Not_found ->
      let m : string = Format.sprintf "id %s not found" flux_map_id in
      Api_common.result_error_msg ~result_code:`Not_found m


    method simulation_catalog_din :
      Api_types_j.din_catalog Api.result Lwt.t =
      detail_projection ~project ~system_process ~projection:self#info_flux_map

    method simulation_detail_din
        (flux_map_id : Api_types_j.din_id) :
      Api_types_j.din Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_flux_map flux_map_id)

  end;;

class manager_log_message
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) : Api.manager_log_message =
  object(self)

    method private log_message
        (detail : Api_data.simulation_detail_output) :
      Api_types_j.log_message Api.result =
      Api_common.result_ok detail.Api_types_j.simulation_output_log_messages

    method simulation_detail_log_message :
      Api_types_j.log_message Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:self#log_message
  end

let select_observables
    (plot_limit : Api_types_j.plot_limit)
    (plot : Api_types_j.plot) : Api_types_j.plot =
  let plot_time_series = Tools.array_rev_of_list plot.Data.plot_series in
  let plot_detail_size = Array.length plot_time_series in
  let plot_limit_offset = plot_limit.Api_types_j.plot_limit_offset in
  let plot_limit_points = plot_limit.Api_types_j.plot_limit_points in
  let start,len =
    match plot_limit_offset, plot_limit_points with
    | None, None -> 0, plot_detail_size
    | Some offset, None -> offset, max 0 (plot_detail_size - offset)
    | None, Some nb -> max 0 (plot_detail_size - nb), min nb plot_detail_size
    | Some offset, Some nb -> offset, min nb (max 0 (plot_detail_size - offset))
  in
  let new_plot_time_series = (List.rev (Array.to_list (Array.sub plot_time_series start len))) in
  { plot with Data.plot_series = new_plot_time_series }

class manager_plot
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) :
  Api.manager_plot =
  object(self)
    method private get_plot
        (plot_limit : Api_types_j.plot_parameter)
        (detail : Api_data.simulation_detail_output) :
      Api_types_j.plot Api.result =
      match detail.Api_types_j.simulation_output_plot with
      | Some plot ->
        Api_common.result_ok
          (select_observables plot_limit plot)
      | None -> let m : string = "plot not available" in
        Api_common.result_error_msg ~result_code:`Not_found m

    method simulation_detail_plot
        (plot_parameter : Api_types_j.plot_parameter) :
      Api_types_j.plot Api.result Lwt.t =
      detail_projection
        ~project ~system_process ~projection:(self#get_plot plot_parameter)
  end

class manager_snapshot
    (project : Api_environment.project)
    (system_process : Kappa_facade.system_process) :
  Api.manager_snapshot =
  object(self)
    method private info_snapshot
        (detail : Api_data.simulation_detail_output) :
      Api_types_j.snapshot_catalog Api.result =
      let snapshots : Api_types_j.snapshot list =
        detail.Api_types_j.simulation_output_snapshots in
      let snapshot_catalog =
        { Api_types_j.snapshot_ids =
            List.map (fun s -> s.Data.snapshot_file) snapshots } in
      Api_common.result_ok snapshot_catalog
    method private get_snapshot
        (snapshot_id : Api_types_j.snapshot_id)
        (detail : Api_data.simulation_detail_output)
      : Api_types_j.snapshot Api.result =
      let snapshot_list : Api_types_j.snapshot list =
        detail.Api_types_j.simulation_output_snapshots
      in
      let snapshot_eq : Api_types_j.snapshot -> bool =
        fun snapshot -> snapshot_id = snapshot.Data.snapshot_file
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
      match project#get_simulation () with
      | Some _ ->
        Lwt.return
          (Api_common.result_error_msg
             ~result_code:`Conflict "A simulation already exists")
      | None ->
        project#get_state () >>= function
        | None ->
          Lwt.return (Api_common.result_error_msg
                        "Cannot start simulation: Parse not done")
        | Some parse ->
          Result_util.fold
            ~ok:
              (fun (facade : Kappa_facade.t) ->
                 (Kappa_facade.start
                    ~system_process
                    ~parameter:simulation_parameter
                    ~t:facade)
                 >>=
                 (Result_util.fold
                    ~ok:
                      (fun () ->
                         let () =
                           project#set_simulation simulation_parameter facade in
                         Lwt.return
                           (Api_common.result_ok {
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
      Model_storage.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let parameter = simulation#get_simulation_parameter() in
           Lwt.return (Api_common.result_ok parameter))

    method simulation_raw_trace : string Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation ->
           let t = simulation#get_runtime_state() in
           Lwt.return (Api_common.result_ok
                         (Kappa_facade.get_raw_trace t)))

    method simulation_outputs_zip =
      let projection t =
        try
          let filename = "simulation_outputs" in
          let file = Fakezip.open_out (filename^".zip") in
          let () = Fakezip.add_entry
              t.Api_types_t.simulation_output_inputs
              file (filename^"/inputs.ka") in
          let () = Fakezip.add_entry
              t.Api_types_t.simulation_output_log_messages
              file (filename^"/log.txt") in
          let () =
            match t.Api_types_t.simulation_output_plot with
            | None -> ()
            | Some plot ->
              Fakezip.add_entry
                (Data.export_plot ~is_tsv:false plot)
                file (filename^"/data.csv") in
          let () =
            Mods.StringMap.iter
              (fun name content ->
                 Fakezip.add_entry (String.concat "\n" (List.rev content))
                   file (filename^"/"^name))
              t.Api_types_t.simulation_output_file_lines in
          let () =
            List.iter
              (fun din ->
                 Fakezip.add_entry (Data.string_of_din ?len:None din)
                   file (filename^"/"^din.Data.din_data.Data.din_name))
            t.Api_types_t.simulation_output_dins in
          let () =
            List.iter
              (fun snapshot ->
                 Fakezip.add_entry (Data.string_of_snapshot ?len:None snapshot)
                   file (filename^"/"^snapshot.Data.snapshot_file))
              t.Api_types_t.simulation_output_snapshots in
          let out = Fakezip.close_out file in
          Api_common.result_ok out
        with Fakezip.Error (_,f,e) ->
          Api_common.result_error_msg ("Zip error in "^f^": "^e) in
      detail_projection ~project ~system_process ~projection

    method simulation_pause : unit Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.pause ~system_process ~t) >>=
           (fun _ ->
             Lwt.return (Api_common.result_ok ())))

    method private simulation_stop : unit Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation(* project simulation *) ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.stop ~system_process ~t) >>=
           (Result_util.fold
              ~ok:((fun () ->
                  Lwt.return (Api_common.result_ok ())))
              ~error:((fun (errors : Api_types_j.errors) ->
                       Lwt.return (Api_common.result_messages errors)) :
                        Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_intervention
        (simulation_perturbation : Api_types_j.simulation_intervention) :
      string Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.perturbation
              ~system_process:system_process
              ~t:t
              ~perturbation:simulation_perturbation) >>=
           (Result_util.fold
                ~ok:((fun s -> Lwt.return (Api_common.result_ok s)))
                ~error:(fun (errors : Api_types_j.errors) ->
                    Lwt.return (Api_common.result_messages errors))
           )
        )

    method simulation_continue (pause_condition : string) :
      unit Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           (Kappa_facade.continue
              ~system_process:system_process
              ~t:t ~pause_condition) >>=
           (Result_util.fold
                ~ok:((fun () -> Lwt.return (Api_common.result_ok ())))
                ~error:((fun (errors : Api_types_j.errors) ->
                          Lwt.return (Api_common.result_messages errors)) :
                          Api_types_j.errors -> unit Api.result Lwt.t)
           )
        )

    method simulation_info : Api_types_j.simulation_info Api.result Lwt.t =
      Model_storage.bind_simulation
        project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           Kappa_facade.progress ~system_process ~t >>=
           (Result_util.fold
              ~ok:(fun progress ->
                  Kappa_facade.outputs ~system_process ~t >>=
                  (Result_util.fold
                     ~ok:(fun outputs ->
                         Lwt.return
                           (Api_common.result_ok
                              (Api_data.api_simulation_status progress outputs))
                       )
                     ~error:(fun (errors : Api_types_j.errors) ->
                         Lwt.return (Api_common.result_messages errors :
                                       Api_types_j.simulation_info Api.result)))
                )
              ~error:(fun (errors : Api_types_j.errors) ->
                  Lwt.return (Api_common.result_messages errors :
                                Api_types_j.simulation_info Api.result)))
        )

    method simulation_efficiency =
      Model_storage.bind_simulation project
        (fun simulation ->
           let t : Kappa_facade.t = simulation#get_runtime_state () in
           Lwt.return (Api_common.result_ok (Kappa_facade.efficiency t)))

    inherit  manager_file_line project system_process
    inherit  manager_flux_map project system_process
    inherit  manager_log_message project system_process
    inherit  manager_plot project system_process
    inherit  manager_snapshot project system_process
  end
