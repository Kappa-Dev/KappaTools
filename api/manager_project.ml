open Lwt

let rec stop_simulations (system_process:Kappa_facade.system_process) :
  Api_environment.simulation list -> unit Api.result Lwt.t =
  function
  | [] -> Lwt.return (Api_common.result_ok ())
  | current::sumulations ->
    let t : Kappa_facade.t = current#get_runtime_state () in
    (Kappa_facade.stop ~system_process:system_process ~t:t) >>=
    (fun result_l ->
      (stop_simulations system_process sumulations)>>=
      (fun result_r ->
         match (result_l) with
         | (`Ok _) -> Lwt.return result_r
         | (`Error errors_l) ->
           (Api_common.result_map
              ~ok:(fun _ _ -> Lwt.return (Api_common.result_ok ()))
              ~error:(fun _ errors_r ->
                  let errors = errors_l@errors_r in
                  Lwt.return (Api_common.result_messages errors))
              result_r)
      )
    )
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
      (project_id : Api_types_j.project_id) : Api_types_j.project Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun (project : Api_environment.project) ->
         Lwt.return (Api_common.result_ok (to_project project))
      )

  method project_parse
      (project_id : Api_types_j.project_id) : Api_types_j.project_parse Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun (project : Api_environment.project) ->
         let project_version = project#get_version () in
         let state = project#get_state () in
         Lwt.return
           (match state with
            | `Ok kappa_facade ->
              Api_common.result_ok
                { Api_types_j.project_parse_contact_map = Kappa_facade.get_contact_map kappa_facade;
                  Api_types_j.project_parse_project_version = project_version;
                }
            | `Error error ->
              Api_common.result_messages error)
      )




  method project_create
      (project_parameter : Api_types_j.project_parameter) :
    Api_types_j.project_id Api.result Lwt.t =
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
      (Kappa_facade.parse
        ~system_process:system_process
        ~kappa_files:[])
      >>=
      (Api_common.result_data_map
         ~ok:((fun (t : Kappa_facade.t) ->
             let project =
               environment#create_project
                 project_id
                 (`Ok t)
             in
             let () =
               Api_common.ProjectCollection.update
                 environment
                 (project::(Api_common.ProjectCollection.list environment))
             in
             Lwt.return (Api_common.result_ok project_id) :
               Kappa_facade.t ->
             Api_types_j.project_id Api.result Lwt.t))
         ~error:((fun errors -> Lwt.return (Api_common.result_messages errors)) :
                   Api_types_t.message list ->
                   Api_types_j.project_id Api.result Lwt.t)
      )

  method project_delete (project_id : Api_types_j.project_id) :
    unit Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun project ->
         (stop_simulations system_process (project#get_simulations ()))>>=
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
