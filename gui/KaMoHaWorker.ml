(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let on_message =
  let f =
    Kappa_grammar.Kamoha_mpi.on_message Js_of_ocaml_lwt.Lwt_js.yield (fun s ->
        let () = Js_of_ocaml.Worker.post_message s in
        Lwt.return_unit)
  in
  fun text_message -> Lwt.ignore_result (f text_message)

let () = Js_of_ocaml.Worker.set_onmessage on_message
