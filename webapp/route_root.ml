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

class new_manager : Api.concrete_manager =
  let re = Re.compile (Re.str "WebSim") in
  let sa_command = Re.replace_string re ~by:"KaSaAgent" Sys.argv.(0) in
  let sa_process = Lwt_process.open_process (sa_command,[|sa_command|]) in
  let sim_command = Re.replace_string re ~by:"KaSimAgent" Sys.argv.(0) in
  let sim_process = Lwt_process.open_process (sim_command,[|sim_command|]) in
  let stor_command = Re.replace_string re ~by:"KaStor" Sys.argv.(0) in
  let stor_process = Lwt_process.open_process (stor_command,[|stor_command|]) in
  let sa_mailbox = Kasa_client.new_mailbox () in
  let stor_state,update_stor_state = Kastor_client.init_state () in
  object(self)
    initializer
      let () =
        Lwt.ignore_result
          (Agent_common.serve
             sa_process#stdout Tools.default_message_delimter
             (fun r ->
                let ()= Kasa_client.receive sa_mailbox r in Lwt.return_unit)) in
      let () =
        Lwt.ignore_result
          (Agent_common.serve
             sim_process#stdout Tools.default_message_delimter
             (fun r -> let () = self#receive r in Lwt.return_unit)) in
      let () =
        Lwt.ignore_result
          (Agent_common.serve
             stor_process#stdout Tools.default_message_delimter
             (fun r ->
                let () = Kastor_client.receive update_stor_state r in
                Lwt.return_unit)) in
      ()
    method is_running =
      sim_process#state = Lwt_process.Running &&
      stor_process#state = Lwt_process.Running &&
      sa_process#state = Lwt_process.Running
    method private sleep t = Lwt_unix.sleep t
    method private post_message message_text =
      Lwt.ignore_result
        (Lwt_io.atomic
           (fun f ->
              Lwt_io.write f message_text >>= fun () ->
              Lwt_io.write_char f Tools.default_message_delimter)
           sim_process#stdin)
    inherit Mpi_api.manager ()
    inherit Kasa_client.new_client
        ~post:(fun message_text ->
            Lwt.ignore_result
              (Lwt_io.atomic
                 (fun f ->
                    Lwt_io.write f message_text >>= fun () ->
                    Lwt_io.write_char f Tools.default_message_delimter)
                 sa_process#stdin))
        sa_mailbox
    inherit Kastor_client.new_client
        ~post:(fun message_text ->
            Lwt.ignore_result
              (Lwt_io.atomic
                 (fun f ->
                    Lwt_io.write f message_text >>= fun () ->
                    Lwt_io.write_char f Tools.default_message_delimter)
                 stor_process#stdin))
        stor_state
    method terminate =
      let () = sim_process#terminate in
      let () = stor_process#terminate in
      sa_process#terminate
    method is_computing =
      self#sim_is_computing || Kasa_client.is_computing sa_mailbox ||
      self#story_is_computing
  end

let bind_projects f id projects =
  match Mods.StringMap.find_option id !projects with
  | Some p -> f p
  | None ->
    let m = "Project '"^id^"' not found" in
    Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)

let tmp_bind_projects f id projects =
  match Mods.StringMap.find_option id !projects with
  | Some p -> f p
  | None ->
    let message = "Project '"^id^"' not found" in
    Lwt.return_error
      (Exception_without_parameter.add_uncaught_error
         (Exception_without_parameter.build_uncaught_exception
            ~file_name:"route_root" ~message Not_found)
         Exception_without_parameter.empty_error_handler)

let tmp_bind_projects_raw f id projects =
  match Mods.StringMap.find_option id !projects with
  | Some p -> f p
  | None ->
    let message = "Project '"^id^"' not found" in
    let err =
      Exception_without_parameter.add_uncaught_error
        (Exception_without_parameter.build_uncaught_exception
           ~file_name:"route_root" ~message Not_found)
        Exception_without_parameter.empty_error_handler in
    Webapp_common.kasa_response
      ~string_of_success:(fun x -> Yojson.Basic.to_string x)
      (Result.Error err)

let add_projects parameter projects =
  let project_id = parameter.Api_types_j.project_parameter_project_id in
  if Mods.StringMap.mem project_id !projects then
    let message = "project "^project_id^" exists" in
    Lwt.return
      (Api_common.result_error_msg ~result_code:`Conflict message)
  else
    let manager = new new_manager in
    let () = projects := Mods.StringMap.add project_id manager !projects in
    Lwt.return (Api_common.result_ok ())

let delete_projects project_id projects :
  unit Api.result Lwt.t =
  match Mods.StringMap.pop project_id !projects with
  | None,_ ->
    let m = "Project '"^project_id^"' not found" in
    Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)
  | Some m, p ->
    let () = projects := p in
    let () = m#terminate in
    Lwt.return (Api_common.result_ok ())

let influence_node_of_string =
  let rule_re =
    Re.compile
      (Re.whole_string
         (Re.seq [Re.str "_rule_"; Re.group (Re.rep1 Re.digit)])) in
  let var_re =
    Re.compile
      (Re.whole_string
         (Re.seq [Re.str "_var_"; Re.group (Re.rep1 Re.digit)])) in
  fun node_id -> match Re.exec_opt rule_re node_id with
    | Some g -> Public_data.Rule (Re.Group.get g 1 |> int_of_string)
    | None ->
      Public_data.Var
        (Re.Group.get (Re.exec var_re node_id) 1 |> int_of_string)

let route
    ~(shutdown_key: string option)
  : Webapp_common.route_handler list =
  let projects = ref Mods.StringMap.empty in
  [
    { Webapp_common.path = "/v2" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let info = {
              Api_types_j.environment_projects =
                Mods.StringMap.size !projects;
              Api_types_j.environment_build = Version.version_string;
            } in
            Webapp_common.api_result_response
              ~string_of_success:(Api_types_j.string_of_environment_info
                                    ?len:None)
              (Api_common.result_ok info)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/shutdown" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `POST ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `POST ->
            (Cohttp_lwt.Body.to_string context.Webapp_common.body)
            >>= (fun body ->
                match shutdown_key with
                | Some shutdown_key when shutdown_key = body ->
                  let () =
                    Lwt.async
                      (fun () ->
                         let () =
                           Mods.StringMap.iter
                             (fun _ p -> p#terminate)
                             !projects in
                         Lwt_unix.sleep 1.0 >>=
                         fun () -> exit 0)
                  in
                  Lwt.return
                    { Api_types_j.result_data = Result.Ok "shutting down" ;
                      Api_types_j.result_code = `OK }
                | _ ->
                  Lwt.return
                    { Api_types_j.result_data =
                        Result.Error
                          [{ Api_types_j.message_severity = `Error ;
                             Api_types_j.message_text = "invalid key";
                             Api_types_j.message_range = None ; }] ;
                      Api_types_j.result_code = `Bad_request })
            >>= fun (msg) ->
            Webapp_common.api_result_response
              ~string_of_success:(fun x -> x) msg
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; `POST ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let names = List.map fst (Mods.StringMap.bindings !projects) in
            Webapp_common.api_result_response
              ~string_of_success:
                (JsonUtil.string_of_write
                   (JsonUtil.write_list Yojson.Basic.write_string) ?len:None)
              (Api_common.result_ok names)
          | `POST ->
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            Mpi_message_j.project_parameter_of_string >>=
            (fun param -> add_projects param projects) >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null"))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `DELETE ] in
        fun ~context ->
          let project_id = project_ref context in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `DELETE ->
            (delete_projects project_id projects) >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null"))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/parse" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `POST ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `POST ->
            let project_id = project_ref context in
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            (fun s ->
               Yojson.Safe.read_list
                 (Api_types_j.read_overwritten_var)
                 (Yojson.Safe.init_lexer ()) (Lexing.from_string s))
            >>= fun overwrites ->
            bind_projects
              (fun manager -> manager#project_parse overwrites)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_project_parse
                                     ?len:None)
            )
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/files" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `POST ; `GET ] in
        fun ~context:context ->
          let project_id = project_ref context in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `POST ->
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            Mpi_message_j.file_of_string >>= fun file ->
            bind_projects
              (fun manager -> manager#file_create file) project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_file_metadata
                                     ?len:None)
            )
          | `GET ->
            bind_projects
              (fun manager -> manager#file_catalog)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_file_catalog
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/files/{fileid}" ;
      Webapp_common.operation =
        fun ~context:context ->
          let methods = [ `OPTIONS ; `DELETE ; `GET ; `PUT ] in
          let (project_id,file_id) = file_ref context in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `DELETE ->
            bind_projects
              (fun manager -> manager#file_delete file_id)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null"))
          | `GET ->
            bind_projects
              (fun manager -> manager#file_get file_id)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_file ?len:None))
          | `PUT ->
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            Mpi_message_j.file_modification_of_string >>= fun modif ->
            bind_projects
              (fun manager -> manager#file_update file_id modif)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_file_metadata
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/simulation" ;
      Webapp_common.operation =
        fun ~context ->
          let methods = [ `OPTIONS ; `DELETE ; `GET ; `POST ] in
          let project_id = project_ref context in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `DELETE ->
            bind_projects
              (fun manager -> manager#simulation_delete)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null")
            )
          | `GET ->
            bind_projects
              (fun manager -> manager#simulation_info)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_info
                                     ?len:None))
          | `POST ->
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            Mpi_message_j.simulation_parameter_of_string >>= fun params ->
            bind_projects
              (fun manager -> manager#simulation_start params)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_simulation_artifact
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/simulation/trace" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_raw_trace)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun out -> out))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/simulation/outputs" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_outputs_zip)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun out -> Base64.encode out))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/parameter" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_parameter)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:
                 (Mpi_message_j.string_of_simulation_parameter
                    ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/filelines/{filelinesid}" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let (project_id,filelines_id) = field_ref context "filelinesid" in
            bind_projects
              (fun manager -> manager#simulation_detail_file_line
                  filelines_id)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:
                 (fun l -> Yojson.Safe.to_string
                     (`List (List.map (fun x -> `String x) l))))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/DIN/{fluxmapsid}" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let (project_id,fluxmaps_id) = field_ref context "fluxmapsid" in
            bind_projects
              (fun manager -> manager#simulation_detail_din fluxmaps_id)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_din
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/logmessages" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          let project_id = project_ref context in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            bind_projects
              (fun manager -> manager#simulation_detail_log_message)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_log_message
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path = "/v2/projects/{projectid}/simulation/plot" ;
      (* get args *)
      Webapp_common.operation =
        fun ~context:context ->
          let methods = [ `OPTIONS ; `GET ; ] in
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
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
                  plot_limit )) >>=
            (Api_common.result_bind_lwt
               ~ok:(fun plot_parameter ->
                   bind_projects
                     (fun manager -> manager#simulation_detail_plot
                         plot_parameter)
                     project_id projects)) >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_plot ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/snapshots/{snapshotid}" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let (project_id,snapshot_id) = field_ref context "snapshotid" in
            bind_projects
              (fun manager -> manager#simulation_detail_snapshot snapshot_id)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot_detail
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/efficiency" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_efficiency)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Counter.Efficiency.string_of_t
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/filelines" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_catalog_file_line)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_file_line_catalog
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/DIN" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_catalog_din)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_din_catalog
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/snapshots" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_catalog_snapshot)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(Mpi_message_j.string_of_snapshot_catalog
                                     ?len:None))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    (* use the controller pattern *)
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/continue" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `PUT ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `PUT ->
            let project_id = project_ref context in
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            (fun s ->
               Yojson.Safe.read_string
                 (Yojson.Safe.init_lexer ()) (Lexing.from_string s))
            >>= fun params ->
            bind_projects
              (fun manager -> manager#simulation_continue params)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null"))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/pause" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `PUT ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `PUT ->
            let project_id = project_ref context in
            bind_projects
              (fun manager -> manager#simulation_pause)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun () -> "null"))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/simulation/intervention" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `PUT ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `PUT ->
            let project_id = project_ref context in
            (Cohttp_lwt.Body.to_string context.Webapp_common.body) >|=
            Api_types_j.simulation_intervention_of_string >>= fun pert ->
            bind_projects
              (fun manager -> manager#simulation_intervention pert)
              project_id projects >>=
            (Webapp_common.api_result_response
               ~string_of_success:(fun s -> Yojson.Safe.to_string (`String s)))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    (** Static analyses *)
    { Webapp_common.path = "/v2/projects/{projectid}/analyses" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `PUT ; ] in
        fun ~context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `PUT ->
            let project_id = project_ref context in
            Cohttp_lwt.Body.to_string context.Webapp_common.body >>=
            fun compil -> tmp_bind_projects
              (fun manager -> manager#init_static_analyser_raw compil)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun () -> "null")
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/contact_map" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            let request = context.Webapp_common.request in
            let uri = Cohttp.Request.uri request in
            let query = Uri.get_query_param  uri in
            let accuracy = Option_util.bind
                Public_data.accuracy_of_string (query "accuracy") in
            tmp_bind_projects
              (fun manager -> manager#get_contact_map accuracy)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x -> Yojson.Basic.to_string x)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/influence_map" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            let request = context.Webapp_common.request in
            let uri = Cohttp.Request.uri request in
            let query = Uri.get_query_param  uri in
            let accuracy = Option_util.bind
                Public_data.accuracy_of_string (query "accuracy") in
            let fwd = Option_util.map int_of_string (query "fwd") in
            let bwd = Option_util.map int_of_string (query "bdw") in
            let total = Option_util.map int_of_string (query "total") in
            let origin =
              Option_util.map influence_node_of_string (query "origin") in
            tmp_bind_projects_raw
              (fun manager ->
                 match total with
                 | Some total ->
                   manager#get_local_influence_map
                     accuracy ?fwd ?bwd ?origin ~total >>=
                   Webapp_common.kasa_response
                     ~string_of_success:(fun x ->
                         Yojson.Basic.to_string
                           (Public_data.local_influence_map_to_json x))
                 | _ -> manager#get_influence_map_raw accuracy >>=
                   Webapp_common.kasa_response
                     ~string_of_success:(fun x -> x))
              project_id projects
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/influence_map/initial_node" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_initial_node)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  let o = JsonUtil.of_option
                      Public_data.refined_influence_node_to_json x in
                  Yojson.Basic.to_string o)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/influence_map/next_node{nodeid}" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let (project_id,node_id) = field_ref context "nodeid" in
            let node =
              if node_id = "" then None
              else Some (influence_node_of_string node_id) in
            tmp_bind_projects
              (fun manager -> manager#get_next_node node)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  let o = JsonUtil.of_option
                      Public_data.refined_influence_node_to_json x in
                  Yojson.Basic.to_string o)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/influence_map/previous_node{nodeid}";
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let (project_id,node_id) = field_ref context "nodeid" in
            let node =
              if node_id = "" then None
              else Some (influence_node_of_string node_id) in
            tmp_bind_projects
              (fun manager -> manager#get_previous_node node)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  let o = JsonUtil.of_option
                      Public_data.refined_influence_node_to_json x in
                  Yojson.Basic.to_string o)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/all_nodes_of_influence_map" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            let request = context.Webapp_common.request in
            let uri = Cohttp.Request.uri request in
            let query = Uri.get_query_param  uri in
            let accuracy = Option_util.bind
                Public_data.accuracy_of_string (query "accuracy") in
            tmp_bind_projects
              (fun manager ->
                   manager#get_nodes_of_influence_map
                     accuracy)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  Yojson.Basic.to_string
                    Public_data.nodes_of_influence_map_to_json x))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/dead_rules" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_dead_rules)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x -> Yojson.Basic.to_string
                                     (Public_data.dead_rules_to_json x))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/dead_agents" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_dead_agents)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x -> Yojson.Basic.to_string x)
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/non_weakly_reversible_transitions" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_non_weakly_reversible_transitions)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  Yojson.Basic.to_string
                    (Public_data.separating_transitions_to_json x))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/constraints" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_constraints_list)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x ->
                  Yojson.Basic.to_string (Public_data.lemmas_list_to_json x))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
    { Webapp_common.path =
        "/v2/projects/{projectid}/analyses/potential_polymers" ;
      Webapp_common.operation =
        let methods = [ `OPTIONS ; `GET ; ] in
        fun ~context:context ->
          match context.Webapp_common.request.Cohttp.Request.meth with
          | `GET ->
            let project_id = project_ref context in
            tmp_bind_projects
              (fun manager -> manager#get_potential_polymers (Some Public_data.High) (Some Public_data.High)) (*TO DO make it tunable *)
              project_id projects >>=
            Webapp_common.kasa_response
              ~string_of_success:(fun x -> Yojson.Basic.to_string
                                     (Public_data.scc_to_json x))
          | `OPTIONS -> Webapp_common.options_respond methods
          | _ -> Webapp_common.method_not_allowed_respond methods
    };
  ]
