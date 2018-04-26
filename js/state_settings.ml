(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let _client_id : string ref = ref (Common.guid ())
let get_client_id () : string = !_client_id
let set_client_id (client_id : string) : unit = _client_id := client_id

let synch, set_synch = React.S.create false

let agent_coloring = Js.Unsafe.obj [||]

let init () : unit Lwt.t =
  let client_ids = Common_state.url_args "client_id" in
  let synch = Common_state.url_args "synch" in
  let () =
    match client_ids with
    | client_id::_  -> set_client_id client_id
    | [] -> ()
  in
  let () = set_synch (["true"] = synch) in
  Lwt.return_unit

let sync () : unit Lwt.t = Lwt.return_unit
