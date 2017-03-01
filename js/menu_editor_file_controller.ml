(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let create_file
    ?(content:string = "")
    (file_id : string) : unit =
  Common.async
    (fun () ->
       (State_error.wrap
          __LOC__
          (State_file.create_file ~filename:file_id ~content))
       >>=
       (* get new contact map *)
       (fun _ -> State_project.sync ())
       >>=
       (fun _ -> Lwt.return_unit)
    )

let set_file (file_id : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.select_file file_id)
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let set_content (content : string) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.set_content content)
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let close_file () : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.remove_file ())
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )

let set_file_compile (file_id: string) (compile : bool) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.set_compile
            file_id
            compile)
       >>=
       (fun _ -> State_project.sync ()) (* get new contact map *)
       >>=
       (fun _ -> Lwt.return_unit)
    )

let order_files (filenames : string list) : unit =
  Common.async
    (fun () ->
       State_error.wrap
         __LOC__
         (State_file.order_files filenames)
       >>= (fun _ -> State_project.sync ()) (* get new contact map *)
       >>= (fun _ -> Lwt.return_unit)
    )
