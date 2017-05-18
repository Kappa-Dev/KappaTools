(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

class manager ()  =
  let kasa_worker = Worker.create "KaSaWorker.js" in
  object(self)
    val sim_worker = Worker.create "WebWorker.js"
    initializer
      let () = kasa_worker##.onmessage :=
          (Dom.handler
             (fun (response_message : string Worker.messageEvent Js.t) ->
                let response_text : string = response_message##.data in
                let () = Kasa_client.receive response_text  in
                Js._true
             )) in
      let () = sim_worker##.onmessage :=
          (Dom.handler
             (fun (response_message : string Worker.messageEvent Js.t) ->
                let response_text : string = response_message##.data in
                let () = self#receive response_text  in
                Js._true
             ))
      in ()
    method sleep timeout = Lwt_js.sleep timeout
    method post_message (message_text : string) : unit =
      sim_worker##postMessage(message_text)
    inherit Mpi_api.manager ()
    inherit Kasa_client.new_client
        ~post:(fun message_text -> kasa_worker##postMessage(message_text))
    method terminate () =
      let () = sim_worker##terminate in
      kasa_worker##terminate
  end
