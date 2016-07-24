open Api
open Lwt

module StringMap = Mods.StringMap
type session_state = unit

class service_manager
    (session_manager : Api.session_manager) : Api.service_manager =
  object
    val mutable session_manager : session_storage option = None
    val mutable service_state : session_state StringMap.t = StringMap.empty
    val start_time : float = Sys.time()

    method private count_processes () : int Api_types_j.result Lwt.t =
      failwith "not implemented"

    method private count_sessions () : int Api_types_j.result Lwt.t =
      failwith "not implemented"

    method service_info () : Api_types_j.service_info Api_types_j.result Lwt.t =
      Lwt.return
	(Api_common.result_ok
           { Api_types_j.sessions = 0;
             Api_types_j.processes = StringMap.size service_state;
             Api_types_j.build = Version.version_msg })
  end;;

class session_storage : Api.session_storage =
  object
    val mutable session_state : Api_types_j.session_metadata StringMap.t ref =
      ref StringMap.empty
    val mutable feedback_state : Api_types_j.session_feedback list ref =
      ref []

    method session_list () :
      Api_types_j.session_info Api_types_j.result Lwt.t =
      let sessions : Api_types_j.session_metadata list =
	List.map snd (StringMap.bindings (!session_state)) in
      Lwt.return (Api_common.result_ok sessions)

    method session_create
	(session_metadata : Api_types_j.session_metadata) :
      unit Api_types_j.result Lwt.t =
      let session_id = session_metadata.Api_types_j.id in
      if StringMap.mem session_id !session_state then
	Lwt.return
	  (Api_common.result_error_msg
	     (Format.sprintf "session exists %s" session_id))
      else
	let () =
	  session_state :=
	    StringMap.add
	      session_id
	      session_metadata
	      !session_state
	in
	Lwt.return
	  (Api_common.result_ok ())

    method session_delete
	~(session_id:Api_types_j.session_id) :
      unit Api_types_j.result Lwt.t =
      if StringMap.mem session_id !session_state then
	let () =
	  session_state :=
	    StringMap.remove
	      session_id
	      !session_state
	in
	Lwt.return
	  (Api_common.result_ok ())

      else
	Lwt.return
	  (Api_common.result_error_msg
	     (Format.sprintf
		"session does not exists %s"
		session_id))

    (* Provide feedback for session.  This is to allow for clients to
       report crash information. *)
    method session_feedback
	~(session_feedback:Api_types_j.session_feedback) :
      unit Api_types_j.result Lwt.t =
      let () = feedback_state := session_feedback::(!feedback_state) in
      Lwt.return
	(Api_common.result_ok ())

    method list_feedback
	() :
      Api_types_j.session_feedback list Api_types_j.result Lwt.t =
      Lwt.return
	(Api_common.result_ok (!feedback_state))

  end

class session_manager : Api.session_manager =
  object(self)
    val session_storage : session_storage = new session_storage

    method session_create
	(metadata : Api_types_j.session_metadata) :
      unit Api_types_j.result Lwt.t =
      session_storage#session_create metadata

    method session_delete
	(session_id : Api_types_j.session_id) :
      unit Api_types_j.result Lwt.t =
      session_storage#session_delete session_id

    method session_feedback
	(session_feedback : Api_types_j.session_feedback) :
      unit Api_types_j.result Lwt.t =
      session_storage#session_feedback
	session_feedback
    method private authenticate
	(session_info : Api_types_j.session_info)
	(session_metadata : Api_types_j.session_metadata) : bool
      =
      List.exists
	(fun (current_metadata : Api_types_j.session_metadata) ->
	   current_metadata = session_metadata)
	session_info
    method session_authenticate
	(session_metadata : Api_types_j.session_metadata) :
      bool Api_types_j.result Lwt.t =
      (session_storage#session_list ())
      >>=
      (fun (session_info : Api_types_j.session_info Api_types_j.result) ->
	 Lwt.return
	   (Api_common.result_bind
	      ~ok:(fun (session_info : Api_types_j.session_info)
	            -> Api_common.result_ok
			(self#authenticate session_info session_metadata))
	      ~result:session_info))

  end;;

class storage_manager : Api.storage_manager =
  object
    method storage_info
	~(session_id:Api_types_j.session_id)
      : storage_info Api_types_j.result Lwt.t =
      failwith ""
  end;;

class simulation_manager : Api.simulation_manager =
  object
    method simulation_info
	~(session_id:Api_types_j.session_id)
      : simulation_info Api_types_j.result Lwt.t =
      failwith ""
  end;;

(* manager via inheritance *)
class manager : Api.manager =
  object
    inherit service_manager (new session_manager)
    inherit session_manager

  end;;
