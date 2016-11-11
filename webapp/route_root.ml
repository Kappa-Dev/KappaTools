open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open Conduit_lwt_unix
open Unix
open Lwt_log

let project_ref context =
  List.assoc "projectid" context.Webapp_common.arguments
let file_ref context =
  (List.assoc "projectid" context.Webapp_common.arguments,
   List.assoc "fileid" context.Webapp_common.arguments)
let simulation_ref context =
  (List.assoc "projectid" context.Webapp_common.arguments,
   List.assoc "simulationid" context.Webapp_common.arguments)
let field_ref context field =
  (List.assoc "projectid" context.Webapp_common.arguments,
   List.assoc "simulationid" context.Webapp_common.arguments,
   List.assoc field context.Webapp_common.arguments)

let route
    ~(manager: Api.manager)
    ~(shutdown_key: string option)
  : Webapp_common.route_handler list =
  [  { Webapp_common.path = "/v2" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            (manager#environment_info ()) >>=
            (fun (info : Api_types_j.environment_info Api.result)
              -> Webapp_common.result_response
                  ~string_of_success:Api_types_j.string_of_environment_info
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
                   async
                     (fun () -> Lwt_unix.sleep 1.0 >>=
                       fun () -> exit 0)
                 in
                 Lwt.return
                   { Api_types_j.result_data = `Ok "shutting down" ;
                     Api_types_j.result_code = `OK }
               | _ ->
                 Lwt.return
                   { Api_types_j.result_data =
                       `Error [{ Api_types_j.message_severity = `Error ;
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
            (manager#project_info ()) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_project_info ?len:None)
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
               ~string_of_success:(Mpi_message_j.string_of_project_id ?len:None)
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
               ~string_of_success:(Mpi_message_j.string_of_file_result ?len:None
                                     Mpi_message_j.write_file_metadata)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/files" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#file_info project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_info ?len:None))
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
               ~string_of_success:(Mpi_message_j.string_of_file_result ?len:None
                                     Mpi_message_j.write_unit_t)
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
               ~string_of_success:(Mpi_message_j.string_of_file_result ?len:None
                                     Mpi_message_j.write_file_metadata)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}" ;
       Webapp_common.methods = [ `OPTIONS ; `DELETE ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_delete project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/distances/{distanceid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id,distance_id) = field_ref context "distanceid" in
            let distance_id = int_of_string distance_id in
            (manager#simulation_detail_distance project_id simulation_id distance_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_distance ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/filelines/{filelinesid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id,filelines_id) = field_ref context "filelinesid" in
            (manager#simulation_detail_file_line project_id simulation_id (Some filelines_id)) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_line_detail ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/fluxmaps/{fluxmapsid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id,fluxmaps_id) = field_ref context "fluxmapsid" in
            (manager#simulation_detail_flux_map project_id simulation_id fluxmaps_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_flux_map ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/logmessages" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_detail_log_message project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_log_message_detail ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/plot" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_detail_plot project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_plot ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/snapshots/{snapshotid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id,snapshot_id) = field_ref context "snapshotid" in
            (manager#simulation_detail_snapshot project_id simulation_id snapshot_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_info project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_info ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/distances" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_info_distance project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_distance_info ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/filelines" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_info_file_line project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_file_line_info ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/fluxmaps" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_info_flux_map project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_flux_map_info ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/snapshots" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_info_snapshot project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot_info ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let project_id = project_ref context in
            (manager#simulation_list project_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_catalog ?len:None)
            )
         )
     };
     (* use the controller pattern *)
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/continue" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Api_types_j.simulation_parameter_of_string
            >>=
            (manager#simulation_continue project_id simulation_id)
            >>=
            (Webapp_common.result_response
                     ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/pause" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_pause project_id simulation_id)
            >>=
            (Webapp_common.result_response
                     ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path =
         "/v2/projects/{projectid}/simulations/{simulationid}/perturbation" ;
       Webapp_common.methods = [ `OPTIONS ; `PUT ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (Cohttp_lwt_body.to_string context.Webapp_common.body)
            >|=
            Api_types_j.simulation_perturbation_of_string
            >>=
            (manager#simulation_perturbation project_id simulation_id)
            >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
     { Webapp_common.path = "/v2/projects/{projectid}/simulations" ;
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
               ~string_of_success:(Mpi_message_j.string_of_simulation_id ?len:None)
            )
         )
     };
     { Webapp_common.path = "/v2/projects/{projectid}/simulations/{simulationid}" ;
       Webapp_common.methods = [ `OPTIONS ; `DELETE ; ] ;
       Webapp_common.operation =
         (fun ~context:context ->
            let (project_id,simulation_id) = simulation_ref context in
            (manager#simulation_delete project_id simulation_id) >>=
            (Webapp_common.result_response
               ~string_of_success:(Mpi_message_j.string_of_unit_t ?len:None)
            )
         )
     };
  ]
