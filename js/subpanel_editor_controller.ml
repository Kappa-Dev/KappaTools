(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let set_content (filecontent : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.set_content filecontent)
       >>= (fun _ -> Lwt.return_unit)
    )


let set_manager (runtime_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_runtime.create_spec ~load:true runtime_id) >>=
       (fun _ -> State_project.sync ()) >>=
       (Api_common.result_bind_lwt ~ok:State_file.sync) >>=
       (fun _ -> Lwt.return_unit)
    )

let with_file (handler : Api_types_j.file Api.result -> unit Api.result Lwt.t) : unit =
    Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         ((State_file.get_file ()) >>=
          handler)
       >>= (fun _ -> Lwt.return_unit)
    )
