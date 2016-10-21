open Lwt

class manager_project
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_project =
object
  method project_info () :
    Api_types_j.project_info Api.result Lwt.t =
    let project_info : Api_types_j.project_info =
      List.map
         (fun project -> project#get_project_id ())
         (environment#get_projects ())
    in
    Lwt.return (Api_common.result_ok project_info)

  method project_create
      (project_parameter : Api_types_j.project_parameter) :
    Api_types_j.project_id Api.result Lwt.t =
    let project_id : Api_types_j.project_id =
      project_parameter.Api_types_j.project_id
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
      let kappa_code = "" in
      (Kappa_facade.parse
        ~system_process:system_process
        ~kappa_code:kappa_code)
      >>=
      (Api_common.result_data_map
         ~ok:((fun (t : Kappa_facade.t) ->
             let project =
               environment#create_project
                 project_id
                 t
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

  method project_delete
      (project_id : Api_types_j.project_id) :
    unit Api.result Lwt.t =
    Api_common.ProjectOperations.bind
      project_id
      environment
      (fun _ ->
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

end;;
