open Lwt.Infix
(* runtime for v1 *)
class runtime ()  = object
  method yield () = Lwt_main.yield ()
  method log ?exn (msg : string) =
    Lwt_log_core.log
      ~level:Lwt_log_core.Info
      ?exn
      msg
  inherit Api_v1.Base.base_runtime 0.1
end

(* set up handlers for v1 *)
let process_comand_v1
    (message_delimter : char) :
    string -> unit Lwt.t =
  Api_mpi_v1.on_message
    (new runtime () :> Api_v1.api_runtime)
    (fun message ->
       Lwt_io.write Lwt_io.stdout message >>=
       (fun () ->
          Lwt_io.write Lwt_io.stdout (String.make 1 message_delimter)))

(* system process for v2 *)
class system_process () : Kappa_facade.system_process =
  object
    method log ?exn (msg : string) =
      Lwt_log_core.log ~level:Lwt_log_core.Info ?exn msg
    method yield () : unit Lwt.t = Lwt_main.yield ()
    method min_run_duration () = 0.1
  end

(* set up handlers for v2 *)
let process_comand_v2
    (message_delimter : char) :
    string -> unit Lwt.t =
  let sytem_process : Kappa_facade.system_process = new system_process () in
  let manager : Api.manager = new Api_runtime.manager sytem_process in
  Mpi_api.on_message
    manager
    (fun message ->
       Lwt_io.write Lwt_io.stdout (message^(String.make 1 message_delimter)))

(*  http://ocsigen.org/lwt/2.5.2/api/Lwt_io *)
let serve () : unit Lwt.t =
  let app_args = App_args.default in
  let common_args = Common_args.default in
  let stdsim_args = Stdsim_args.default in
  let options =
    App_args.options app_args @
    Common_args.options common_args @
    Stdsim_args.options stdsim_args in
  let usage_msg =
    "kappa stdio simulator" in
  let () =
    Arg.parse options
      (fun _ -> ()) usage_msg in
  (* set message delimiter *)
  let message_delimter : char =
    match stdsim_args.Stdsim_args.delimiter with
    | None -> Mpi_api.default_message_delimter
    | Some d ->
      d in
  (* debugging TODO : remove me
  let () =
    Lwt.async
      (fun _ ->
         Lwt_io.print
           (Format.sprintf "delimiter:'%c'" message_delimter))
  in
  *)
  (* set protocol version *)
  let process_comand : string -> unit Lwt.t =
    (match app_args.App_args.api with
     | App_args.V1 -> process_comand_v1
     | App_args.V2 -> process_comand_v2
    ) message_delimter in
  (* read and handle messages *)
  let buffer = Buffer.create 512 in
  let rec aux_serve () =
    Lwt_io.read_char Lwt_io.stdin >>=
    (fun (char : char) ->
       if char = message_delimter then
         let m = Buffer.contents buffer in
         process_comand m <&>
         let () = Buffer.reset buffer in aux_serve ()
       else
         let () = Buffer.add_char buffer char in
         aux_serve ()) in
  aux_serve ()

(* start server *)
let () = Lwt_main.run (serve ())
