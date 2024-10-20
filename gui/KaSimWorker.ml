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
      let () = Common.log_group "KaSimWorker log: exn and msg" in
      let () = Common.debug ~loc:__LOC__ exn in
      let () = Common.debug ~loc:__LOC__ msg in
      let () = Common.log_group_end () in
      Lwt.return_unit

    method yield () : unit Lwt.t = Js_of_ocaml_lwt.Lwt_js.yield ()
    method min_run_duration () = 0.1
  end

let system_process_ : Kappa_facade.system_process = new system_process ()
let manager : Api.manager_simulation = new Kasim_runtime.manager system_process_

let on_message (text_message : string) : unit =
  Lwt.ignore_result
    (Kasim_mpi.on_message manager
       (fun s ->
         let () = Worker.post_message s in
         Lwt.return_unit)
       text_message)

let () = Worker.set_onmessage on_message
