(* good old cake pattern *)

class manager
    (system_process : Kappa_facade.system_process) : Api.manager =
  let project = new Environment_memory.project
  in
  object
    inherit Manager_project.manager_project project system_process
    inherit Manager_simulation.manager_simulation project system_process
    inherit Manager_file. manager_file project
  end;;
