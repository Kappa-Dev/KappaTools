(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class manager
    ?(timeout : float = 10.)
    ()  =
  object(self)
    val worker = Worker.create "WebWorker.js"
    initializer
      let () = worker##.onmessage :=
          (Dom.handler
             (fun (response_message : string Worker.messageEvent Js.t) ->
                let response_text : string =
                  response_message##.data
                in
                let () = self#receive response_text  in
                Js._true
             ))
      in ()
    method sleep timeout = Lwt_js.sleep timeout
    method post_message (message_text : string) : unit =
      worker##postMessage(message_text)
    inherit Mpi_api.manager ~timeout:timeout ()
    method terminate () = worker##terminate
  end
