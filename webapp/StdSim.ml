module ApiTypes = ApiTypes_j

open Lwt.Infix

class runtime ()  = object
  method yield () = Lwt_main.yield ()
  method log ?exn (msg : string) =
    Lwt_log_core.log
      ~level:Lwt_log_core.Info
      ?exn
      msg
  inherit Api_v1.Base.base_runtime 0.1
end

let runtime = (new runtime () :> Api_v1.api_runtime)

let process_comand (text_message : string) : unit Lwt.t =
  Api_mpi.on_message
    runtime
    (fun message ->
       Lwt_io.write Lwt_io.stdout message >>=
       (fun () ->
          Lwt_io.write Lwt_io.stdout (String.make 1 Api_mpi.message_delimter)))
    text_message

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let serve () : unit Lwt.t =
  let app_args = App_args.default in
  let common_args = Common_args.default in
  let options = App_args.options app_args @
                Common_args.options common_args
  in
  let usage_msg = "kappa stdio simulator" in
  let () = Arg.parse options (fun _ -> ()) usage_msg in
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char Lwt_io.stdin >>=
    (fun (char : char) ->
       if char = Api_mpi.message_delimter then
         let m = Buffer.contents buffer in
         process_comand m <&>
         let () = Buffer.reset buffer in aux_serve ()
       else
         let () = Buffer.add_char buffer char in
         aux_serve ()) in
  aux_serve ()

let () = Lwt_main.run (serve ())
