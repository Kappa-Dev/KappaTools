open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open Conduit_lwt_unix
open Unix
open Lwt_log
open Re

let route
    ~(manager: Api.manager)
    ~(shutdown_key: string option)
  : Webapp_common.route_handler list =
  [  { Webapp_common.path = "/v2" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ; ] ;
       Webapp_common.operation =
      (fun ~context:context ->
        (manager#environment_info ()) >>=
          (fun (info : Api_types_j.environment_info Api.result)
          -> Webapp_common.result_response
            ~string_of_success:Api_types_j.string_of_environment_info
            ~result:info
          )
      )
     };
     { Webapp_common.path = "/v2/shutdown" ;
       Webapp_common.methods = [ `OPTIONS ; `POST ; ] ;
       Webapp_common.operation =
         fun ~context:context ->
           (Cohttp_lwt_body.to_string context.Webapp_common.body)
           >>= (fun body -> match shutdown_key with
           | Some shutdown_key when shutdown_key = body ->
              let () =
                async
                  (fun () -> Lwt_unix.sleep 1.0 >>=
                    fun () -> exit 0)
              in
              Lwt.return
                { Api_types_j.result_data = `Ok "shutting down" ;
                  Api_types_j.result_code = Api.OK }
           | _ ->
              Lwt.return
                { Api_types_j.result_data =
		    `Error [{ Api_types_j.message_severity = `Error ;
			      Api_types_j.message_text = "shutting down" ;
			      Api_types_j.message_range = None ; }] ;
                  Api_types_j.result_code = Api.ERROR })
         >>= (fun (msg) ->
           Webapp_common.result_response
             ~string_of_success:(fun x -> x)
             ~result:msg
         )
     }
  ]
