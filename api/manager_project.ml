open Lwt

let stop_simulation (system_process:Kappa_facade.system_process) :
  Api_environment.simulation option -> unit Api.result Lwt.t =
  function
  | None -> Lwt.return (Api_common.result_ok ())
  | Some current ->
    let t : Kappa_facade.t = current#get_runtime_state () in
    (Kappa_facade.stop ~system_process:system_process ~t:t) >>=
    (Result_util.map
       ~ok:(fun _ -> Lwt.return (Api_common.result_ok ()))
       ~error:(fun errors ->
           Lwt.return (Api_common.result_messages errors)))

let to_project (project : Api_environment.project) =
  { Api_types_j.project_id = project#get_project_id () ;
    Api_types_j.project_version = project#get_version () ; }

class manager_project
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_project =
object
  method project_catalog () :
    Api_types_j.project_catalog Api.result Lwt.t =
    let project_catalog : Api_types_j.project_catalog =
      { Api_types_j.project_list =
          List.map to_project (environment#get_projects ()) }
    in
    Lwt.return (Api_common.result_ok project_catalog)

  method project_get
      (project_id : Api_types_j.project_id) :
    Api_types_j.project Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun (project : Api_environment.project) ->
         Lwt.return (Api_common.result_ok (to_project project))
      )

  method project_parse (project_id : Api_types_j.project_id) :
    Api_types_j.project_parse Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun (project : Api_environment.project) ->
         (project#get_state () >>= function
           | Some x -> Lwt.return x
           | None ->
             let harakiri,_ = Lwt.task () in
             let cand =
               Lwt.pick [
                 Kappa_facade.parse
                   ~system_process
                   ~kappa_files:(project#get_files ());
                 harakiri >>= fun () ->
                 Lwt.return (Result_util.error
                               [Api_common.error_msg
                                  "Parse cancelled by modified files"])
               ] in
             let _ = project#set_state cand in
             cand)
         >>=
         (fun state ->
            Lwt.return
              (Result_util.map
                 ~ok:(fun kappa_facade ->
                     Api_common.result_ok
                       { Api_types_t.project_parse_contact_map =
                           Kappa_facade.get_contact_map kappa_facade;
                         Api_types_t.project_parse_project_version =
                           project#get_version ();
                         Api_types_t.project_parse_raw_ast =
                           Kappa_facade.get_raw_ast kappa_facade;
                       })
                 ~error:(fun error -> Api_common.result_messages error)
                 state
           )
         )
      )

  method project_create
      (project_parameter : Api_types_j.project_parameter) :
    unit Api.result Lwt.t =
    let project_id : Api_types_j.project_id =
      project_parameter.Api_types_j.project_parameter_project_id
    in
    if Api_common.ProjectOperations.exists project_id environment
    then
      let message : string =
        Format.sprintf
          "project %s exists"
          (Api_common.ProjectCollection.id_to_string project_id)
      in
      Lwt.return
        (Api_common.result_error_msg ~result_code:`CONFLICT message)
    else
      let project =
        environment#create_project project_id in
      let () =
        Api_common.ProjectCollection.update
          environment
          (project::(Api_common.ProjectCollection.list environment))
      in
      Lwt.return (Api_common.result_ok ())

  method project_delete (project_id : Api_types_j.project_id) :
    unit Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun project ->
         (stop_simulation system_process (project#get_simulation ()))>>=
         (Api_common.result_bind_lwt
            ~ok:(fun () ->
                let not_project =
                  (fun project ->
                     not (Api_common.ProjectOperations.refs project_id project))
                in
                let projects =
                  List.filter
                    not_project
                    (Api_common.ProjectCollection.list environment)
                in
                let () =
                  Api_common.ProjectCollection.update
                    environment
                    projects
                in
                Lwt.return (Api_common.result_ok ())
              )
         )
      )

end;;
