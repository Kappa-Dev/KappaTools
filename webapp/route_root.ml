(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let project_ref context =
  List.assoc "projectid" context.Webapp_common.arguments
let file_ref context =
  (List.assoc "projectid" context.Webapp_common.arguments,
   List.assoc "fileid" context.Webapp_common.arguments)
let field_ref context field =
  (List.assoc "projectid" context.Webapp_common.arguments,
   List.assoc field context.Webapp_common.arguments)

let route
    ~(manager: Api.concrete_manager)
    ~(shutdown_key: string option)
  : Webapp_common.route_handler list =
  [  { Webapp_common.path = "/v2" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context ->
            (manager#environment_info ()) >>=
            (fun (info : Api_types_j.environment_info Api.result)
              -> Webapp_common.result_response
                  ~string_of_success:(Api_types_j.string_of_environment_info ?len:None)
                  info
            )
         )
     };
     { Webapp_common.path = "/v2/shutdown" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         fun ~context:context ->
           (Cohttp_lwt_body.to_string context.Webapp_common.body)
           >>= (fun body ->
               match shutdown_key with
               | Some shutdown_key when shutdown_key = body ->
                 let () =
                   Lwt.async
                     (fun () -> Lwt_unix.sleep 1.0 >>=
                       fun () -> exit 0)
                 in
                 Lwt.return
                   { Api_types_j.result_data = Result.Ok "shutting down" ;
                     Api_types_j.result_code = `OK }
               | _ ->
                 Lwt.return
                   { Api_types_j.result_data =
                       Result.Error [{ Api_types_j.message_severity = `Error ;
                                 Api_types_j.message_text = "invalid key";
                                 Api_types_j.message_range = None ; }] ;
                     Api_types_j.result_code = `ERROR })
           >>= (fun (msg) ->
               Webapp_common.result_response
                 ~string_of_success:(fun x -> x)
                 msg
             )
     };
     { Webapp_common.path =
         "/v2/projects" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let () = ignore(context) in
            (manager#project_catalog ()) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_project_catalog ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let () = ignore(context) in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Mpi_message_j.project_parameter_of_string
            >>=
            (manager#project_create) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}" ;
       Webapp_common.methods = [ `OPTIONS ; `DELETE ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#project_delete project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#project_get project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_project ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/parse" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#project_parse project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_project_parse ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Mpi_message_j.file_of_string
            >>=
            (manager#file_create project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_metadata ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#file_catalog project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_catalog ?len:None))
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files/{fileid}" ;
       Webapp_common.methods = [ `OPTIONS ; `DELETE ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,file_id) = file_ref context in
            (manager#file_delete project_id file_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files/{fileid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,file_id) = file_ref context in
            (manager#file_get project_id file_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files/{fileid}" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,file_id) = file_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Mpi_message_j.file_modification_of_string
            >>=
            (manager#file_update project_id file_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_metadata ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation" ;
       Webapp_common.methods = [ `OPTIONS ; `DELETE ; ] ;
       Webapp_common.operation =
         (fun ~context ->
            let project_id = project_ref context in
            (manager#simulation_delete project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/trace" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context ->
            let project_id = project_ref context in
            (manager#simulation_raw_trace project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(fun out -> out)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/parameter" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context ->
            let project_id = project_ref context in
            (manager#simulation_parameter project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:
                 (Mpi_message_j.string_of_simulation_parameter
                    ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/filelines/{filelinesid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,filelines_id) =
              field_ref
                context
                "filelinesid" in
            (manager#simulation_detail_file_line
               project_id
               (Some filelines_id)) >>=
            (Webapp_common.result_response
               ~string_of_success:
                 (Mpi_message_j.string_of_file_line_detail
                    ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/fluxmaps/{fluxmapsid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,fluxmaps_id) =
              field_ref
                context
                "fluxmapsid" in
            (manager#simulation_detail_flux_map
               project_id
               fluxmaps_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_flux_map
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/logmessages" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_detail_log_message project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_log_message ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/plot" ;
       (* get args *)
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            (* "max_points" "limit_method" *)
            let request = context.Webapp_common.request in
            let uri = Cohttp.Request.uri request in
            let query = Uri.get_query_param  uri in
            let plot_limit_points : int option =
              match query "plot_limit_points" with
              | None -> None
              | Some plot_limit_points ->
                Some (int_of_string plot_limit_points)
            in
            let plot_limit_offset : int option =
              match query "plot_limit_offset" with
              | None -> None
              | Some t -> Some (int_of_string t)
            in
            let plot_limit : Api_types_j.plot_limit =
              { Api_types_j.plot_limit_offset = plot_limit_offset  ;
                Api_types_j.plot_limit_points = plot_limit_points ; } in
            let project_id = project_ref context in
            (* handle malformed *)
            (Lwt.return
               (Api_common.result_ok
                  { Api_types_j.plot_parameter_plot_limit  = Some plot_limit ;
                  } )) >>=
            (Api_common.result_bind_lwt
               ~ok:(fun plot_parameter ->
                   manager#simulation_detail_plot
                     project_id
                     plot_parameter
               )
            )>>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_plot_detail ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/snapshots/{snapshotid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,snapshot_id) =
              field_ref
                context
                "snapshotid" in
            (manager#simulation_detail_snapshot
               project_id
               snapshot_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot_detail
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_info project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_info
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/efficiency" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_efficiency project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Counter.Efficiency.string_of_t
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/filelines" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_catalog_file_line project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_line_catalog
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/fluxmaps" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_catalog_flux_map project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_flux_map_catalog
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/snapshots" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_catalog_snapshot project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot_catalog
                                     ?len:None)
            )
         )
     };
     (* use the controller pattern *)
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/continue" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Api_types_j.simulation_parameter_of_string
            >>=
            (manager#simulation_continue project_id)
            >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/pause" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_pause project_id)
            >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulation/perturbation" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Api_types_j.simulation_perturbation_of_string
            >>=
            (manager#simulation_perturbation project_id)
            >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t
                                     ?len:None)
            )
         )
     };
     { Webapp_common.path = "/v2/projects/{projectid}/simulation" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Mpi_message_j.simulation_parameter_of_string
            >>=
            (manager#simulation_start project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_artifact
                                     ?len:None)
            )
         )
     };
     (** Static analyses *)
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         (fun ~context ->
            let project_id = project_ref context in
            Cohttp_lwt_body.to_string context.Webapp_common.body >|=
            (fun x -> Ast.compil_of_json (Yojson.Basic.from_string x)) >>=
            (manager#init_static_analyser project_id) >>= function
            | Result.Ok () ->
              let body = "null" in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/contact_map/{accuracy}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id,raw_accuracy = field_ref context "accuracy" in
            let accuracy = Some
                (Public_data.accuracy_of_json
                   (Yojson.Basic.from_string raw_accuracy)) in
            (manager#get_contact_map project_id accuracy) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/contact_map" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#get_contact_map project_id None) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/influence_map/{accuracy}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id,raw_accuracy = field_ref context "accuracy" in
            let accuracy = Some
                (Public_data.accuracy_of_json
                   (Yojson.Basic.from_string raw_accuracy)) in
            (manager#get_influence_map project_id accuracy) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/influence_map" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#get_influence_map project_id None) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/dead_rules" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#get_dead_rules project_id) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
       };
     { Webapp_common.path =
         "/v2/projects/{projectid}/analyses/constraints" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#get_constraints_list project_id) >>= function
            | Result.Ok r ->
              let body = Yojson.Basic.to_string r in
              Webapp_common.string_response ?headers:None ?code:None ~body
            | Result.Error e ->
              Webapp_common.error_response
                ?headers:None ?status:None ~errors:[Api_common.error_msg e]
         )
       };
  ]
