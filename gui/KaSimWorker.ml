(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class system_process () : Kappa_facade.system_process =
  object
    method log ?exn (msg : string) =
      let () = Common.debug exn in
      let () = Common.debug msg in
      Lwt.return_unit

    method yield () : unit Lwt.t = Js_of_ocaml_lwt.Lwt_js.yield ()
    method min_run_duration () = 0.1
  end

let sytem_process : Kappa_facade.system_process = new system_process ()
let manager : Api.manager_simulation = new Api_runtime.manager sytem_process

let on_message (text_message : string) : unit =
  Lwt.ignore_result
    (Mpi_api.on_message manager
       (fun s ->
         let () = Worker.post_message s in
         Lwt.return_unit)
       text_message)

let () = Worker.set_onmessage on_message
