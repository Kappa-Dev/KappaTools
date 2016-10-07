class manager_environment
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_environment =
object
  method environment_info () :
    Api_types_j.environment_info Api.result Lwt.t =
    let projects =
      List.flatten
        (List.map
           (fun workspace -> workspace#get_projects ())
           (environment#get_workspaces ()))
    in
    let simulations =
      List.flatten
        (List.map
           (fun project -> project#get_simulations ())
        projects)
    in
    Lwt.return
      (Api_common.result_ok
         { Api_types_j.environment_simulations = List.length simulations;
           Api_types_j.environment_projects = List.length projects;
           Api_types_j.environment_build = Version.version_string; })
end;;
