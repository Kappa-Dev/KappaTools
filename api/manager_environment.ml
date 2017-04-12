class manager_environment
    (environment : Api_environment.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_environment =
object
  method environment_info () :
    Api_types_j.environment_info Api.result Lwt.t =
    let projects = environment#get_projects () in
    let simulations =
      List.fold_right
        (fun project acc ->
           match project#get_simulation () with None -> acc |Some _ -> succ acc)
        projects 0 in
    Lwt.return
      (Api_common.result_ok
         { Api_types_j.environment_simulations = simulations;
           Api_types_j.environment_projects = List.length projects;
           Api_types_j.environment_build = Version.version_string; })
end;;
