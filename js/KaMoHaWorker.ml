(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let on_message (text_message : string) : unit =
  Lwt.ignore_result
    (Kappa_grammar.Kamoha_mpi.on_message
       Js_of_ocaml_lwt.Lwt_js.yield
       (fun s -> let () = Js_of_ocaml.Worker.post_message s in Lwt.return_unit)
       text_message)

let () = Js_of_ocaml.Worker.set_onmessage on_message
