(* good old cake pattern *)
class manager
    (system_process : Kappa_facade.system_process) =
  let environment : Api_environment.environment = (new Environment_memory.environment () :> Api_environment.environment) in
  object
    inherit Manager_environment.manager_environment environment system_process
  end;;
