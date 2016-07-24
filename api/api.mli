class type service_manager =
  object
    method service_info :
      unit -> Api_types_j.service_info Api_types_j.result Lwt.t
  end;;

(* Manager for the session *)
class type session_storage =
  object
    method session_list :
      unit ->
      Api_types_j.session_info Api_types_j.result Lwt.t
    method session_create :
      Api_types_j.session_metadata ->
      unit Api_types_j.result Lwt.t
    method session_delete :
      session_id:Api_types_j.session_id ->
      unit Api_types_j.result Lwt.t
    (* Provide feedback for session.  This is to allow for clients to
       report crash information. *)
    method session_feedback :
      session_feedback:Api_types_j.session_feedback ->
      unit Api_types_j.result Lwt.t
    method list_feedback :
      unit ->
      Api_types_j.session_feedback list Api_types_j.result Lwt.t
  end;;
(* Expose the parts of the session storage relevant
   to the client in the form of a manager.
*)
class type session_manager =
  object
    method session_create :
      Api_types_j.session_metadata ->
      unit Api_types_j.result Lwt.t
    method session_delete :
      Api_types_j.session_id ->
      unit Api_types_j.result Lwt.t
    method session_feedback :
      Api_types_j.session_feedback ->
      unit Api_types_j.result Lwt.t
    method session_authenticate :
      Api_types_j.session_metadata ->
      bool Api_types_j.result Lwt.t
  end;;

(* Manager for the storage *)
type storage_id = string
type storage_metadata = string
type storage_info = storage_metadata list

class type storage_manager =
  object
    method storage_info :
      session_id:Api_types_j.session_id ->
      storage_info Api_types_j.result Lwt.t
  end;;

(* manager for simulation *)
type simulation_id = string
type simulation_metadata = string
type simulation_info = simulation_metadata list

class type simulation_manager =
  object
    method simulation_info :
      session_id:Api_types_j.session_id ->
      simulation_info Api_types_j.result Lwt.t
  end;;

class type manager =
  object
    inherit service_manager
    inherit session_manager
  end;;
