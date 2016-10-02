(* good old cake pattern *)
class manager
    (system_process : Kappa_facade.system_process) =
  let environment : Api_common.environment = Api_common.environment_new () in
  object
    inherit Manager_environment.manager_environment environment system_process
  end;;
