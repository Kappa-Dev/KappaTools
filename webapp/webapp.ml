module ApiTypes = ApiTypes_j

open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open ApiTypes
open Conduit_lwt_unix
open Unix
open Lwt_log
open Re

let route_handler
    ?(shutdown_key : string option  = None)
    ()
  :
    Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt_body.t ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
  =
  let manager : Api.manager = new Api_runtime.manager in
  fun (conn : Cohttp_lwt_unix.Server.conn)
    (request : Cohttp.Request.t)
    (body : Cohttp_lwt_body.t)
    ->
      let context = { Webapp_common.arguments = []
		    ; Webapp_common.connection = conn
		    ; Webapp_common.request = request
		    ; Webapp_common.body = body }
      in
      (Webapp_common.route_handler
	 ((Route_root.route
             ~manager:manager
             ~shutdown_key:shutdown_key)
	  @ Route_sessions.route
	    ~manager:manager))
	~context:context
