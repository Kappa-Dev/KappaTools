module ApiTypes = ApiTypes_j

open Api
open ApiTypes
open Unix
open Lwt

class runtime ()  = object
  method yield () = Lwt_main.yield ()
  method log ?exn (msg : string) =
    Lwt_log_core.log
      ~level:Lwt_log_core.Info
      ?exn
      msg
  inherit Api_v1.Base.runtime 1.0
end

let runtime = (new runtime () :> Api_v1.runtime)

let process_comand (text_message : string) : unit Lwt.t =
  let () =
    Api_mpi.on_message
      runtime
      (fun message ->
         Lwt.async
           (fun () -> Lwt_io.write Lwt_io.stdout message))
      text_message
  in Lwt.return_unit

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let rec serve ?(buffer = "") () : unit Lwt.t =

  (Lwt_io.read_line Lwt_io.stdin)
  >>=
  (fun line ->
     if line = Api_mpi.message_delimter then
       (process_comand buffer) >>=
       (fun _ -> serve ())
     else
       serve
         ~buffer:(Format.sprintf "%s\n%s" buffer line)
         ()
  )
