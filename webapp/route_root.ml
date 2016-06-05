module Runtime = Api.Base
module ApiTypes = ApiTypes_j

open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open Runtime
open ApiTypes
open Conduit_lwt_unix
open Unix
open Lwt_log
open Re

let route
    ~(runtime : Api.Base.runtime )
    ~(shutdown_key: string option)
    : Webapp_common.route list =
  [  { Webapp_common.path = "" ;
       Webapp_common.methods = [ `OPTIONS ; `GET ] ;
       Webapp_common.handler =
      fun ~context:context ->
          (runtime#info ()) >>=
            (fun info -> Webapp_common.result_response
              Api_types_j.string_of_info info)
     };
     { Webapp_common.path = "/shutdown" ;
       Webapp_common.methods = [ `POST ] ;
       Webapp_common.handler =
         fun ~context:context ->
           (Cohttp_lwt_body.to_string context.Webapp_common.body)
         >>= (fun body ->
           let shutdown_okay =
             Webapp_common.server_respond "shutting down"
           in
           let shutdown_fail =
          Server.respond_string
            ?headers:(Some Webapp_common.headers)
            ~status:`Unauthorized
            ~body:"unathorized"
            ()
           in
           match shutdown_key with
             None -> shutdown_fail
           | Some shutdown_key ->
             if shutdown_key = body then
               let () =
                 async
                   (fun () ->
                     Lwt_unix.sleep 1.0 >>=
                       fun () -> exit 0)
               in
               shutdown_okay
             else
               shutdown_fail)
     }
  ]
