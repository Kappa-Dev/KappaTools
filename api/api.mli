(* Manage kappa projects. Kappa project consists
   of a set of kappa files and simulations that
   are run using the kappa code.
*)

type manager_code = OK | CREATED | ERROR | CONFLICT | NOT_FOUND | ACCEPTED
type result_code = manager_code
type 'ok result = ('ok,manager_code) Api_types_j.result


class type manager_workspace =
  object
    (** List

        List available workspaces by their id's.

        REST : workspaces GET
        API : unit -> workspace_catalog

    *)
    method workspace_list :
      unit ->
      Api_types_j.workspace_catalog result Lwt.t

    (** Create

        Create a workspace.  Given a workspace identifier create a
        workspace.  An error is returned if the workspace identier
        already in use.

        REST : workspaces/{id}  POST
        API : workspace_parameter -> workspace_info

     *)
    method workspace_create :
      Api_types_j.workspace_parameter ->
      Api_types_j.workspace_id result Lwt.t

    (** Status

        Description : Return the status of the workspace.  For now it for
        file management features and returns the contents of the files
        and parse results.

        REST : workspaces/{id}  GET
        API : workspace_id -> workspace_status


     *)
    method workspace_info :
      Api_types_j.workspace_id ->
      Api_types_j.workspace_info result Lwt.t

      (** Delete

          Description : Delete a workspace

          REST : workspaces/{id} DELETE
          API : workspace_id -> unit
      *)
    method workspace_delete :
      Api_types_j.workspace_id ->
      unit result Lwt.t
  end;;

class type manager_project =
  object
    (** List

        List available projects by their id's.

        REST : projects GET
        API : unit -> project_catalog

    *)
    method project_list :
      Api_types_j.workspace_id ->
      Api_types_j.project_catalog result Lwt.t

    (** Create

        Create a project.  Given a project identifier create a
        project.  An error is returned if the project identier
        already in use.

        REST : projects/{id}  POST
        API : project_parameter -> project_info

     *)
    method project_create :
      Api_types_j.workspace_id ->
      Api_types_j.project_parameter ->
      Api_types_j.project_id result Lwt.t

    (** Status

        Description : Return the status of the project.  For now it for
        file management features and returns the contents of the files
        and parse results.

        REST : projects/{id}  GET
        API : project_id -> project_status


     *)
    method project_info :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.project_info result Lwt.t

    (** Update

       Description : Update the contents of the Kappa file

       REST : projects/{id} PUT
       API : project_id -> project_modification -> parse

     *)
    method project_update :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.project_modification ->
      Api_types_j.project_info result Lwt.t

      (** Delete

          Description : Delete a project

          REST : projects/{id} DELETE
          API : project_id -> unit
      *)
    method project_delete :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      unit result Lwt.t
  end;;

class type manager_file =
  object
    (** List

        List available files by their id's.

        REST : file GET
        API : unit -> file_catalog

    *)
    method file_list :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.file_catalog result Lwt.t

    (** Create

        Create a file.  Given a file parameter create a
        project.  An error is returned if the file identier
        already in use.

        REST : file/{id}  POST
        API : file_parameter -> project_info

     *)
    method file_create :
      Api_types_j.workspace_id ->
	Api_types_j.project_id ->
      Api_types_j.file ->
      Api_types_j.file_metadata result Lwt.t

    (** Status

        Description : Return the status of the project.  For now it for
        file management features and returns the contents of the files
        and parse results.

        REST : projects/{id}  GET
        API : project_id -> project_status


     *)
    method file_get :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      Api_types_j.file result Lwt.t

    (** Update

       Description : Update the contents of the Kappa file

       REST : projects/{id} PUT
       API : project_id -> project_modification -> parse

     *)
    method file_update :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      Api_types_j.file_modification ->
      Api_types_j.file_metadata result Lwt.t

      (** Delete

          Description : Delete a project

          REST : projects/{id} DELETE
          API : project_id -> unit
      *)
    method file_delete :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.file_id ->
      unit result Lwt.t
  end;;

(** Manage kappa simulations.  This manages running Kappa simulations.
    The core functionality is to map simulation management operations
    to the given simulation identifier and store the state of the
    simulation. *)

class type manager_simulation =
  object
    (** List

        Description : List available simulations.

        REST : projects/{id}/simulations GET
        API : project_id -> project_catalog

    *)
    method simulation_list :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_catalog result Lwt.t
    (** Start

       Description : Give a runtime parameter start a simulation.

       REST : projects/{id}/simulations POST
       API : project_id -> simulation_parameter -> simulation_id
    *)
    method simulation_start :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_parameter->
      Api_types_j.simulation_id result Lwt.t
    (** Pause

        Description : Pause a running simulation.  In the paused
        states perturbations can be applied to the simulation

        REST : projects/{id}/simulations PUT
        API : project_id -> simulation_id -> unit
    *)
    method simulation_pause :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      unit result Lwt.t
    (** Stop

        Description : Stop a simulation and remove state.

        REST : projects/{id}/simulations/{simulation} DELETE
        API : project_id -> simulation_id -> unit
    *)
    method simulation_stop :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      unit result Lwt.t

    (** Perturbation

        Description : Apply a pertubation to the simulation.

        REST : projects/{id}/simulations/{simulation} PUT
        API : project_id -> simulation_id -> simulation_perturbation -> unit
    *)
    method simulation_perturbation :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_perturbation ->
      unit result Lwt.t

    (** Continue

       Description : Continue a paused simulation.

       REST : projects/{id}/simulations/{simulation} POST
       API : project_id -> simulation_id -> simulation_parameter -> unit
    *)
    method simulation_continue :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_parameter ->
      unit result Lwt.t

    (** Status

       Description : Get the status of a simulation

       REST : projects/{id}/simulations/{simulation} GET
       API : project_id -> simulation_id -> simulation_parameter -> simulation_state
    *)
    method simulation_info :
      Api_types_j.workspace_id ->
      Api_types_j.project_id ->
      Api_types_j.simulation_id ->
      Api_types_j.simulation_info result Lwt.t
  end;;

class type manager_environment =
object
  method environment_info :
    unit ->
    Api_types_j.environment_info result Lwt.t
end;;

class type manager =
object
  inherit manager_environment
  inherit manager_workspace
  inherit manager_project
  inherit manager_simulation
  inherit manager_file
end;;
