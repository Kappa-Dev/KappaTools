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

let _synch : bool ref = ref false
let get_synch () : bool = !_synch
let set_synch (synch : bool) : unit = _synch := synch

let init () : unit Lwt.t =
  let client_ids = Common_state.url_args "client_id" in
  let synch = Common_state.url_args "client_id" in
  let () =
    match client_ids with
    | client_id::_  -> set_client_id client_id
    | [] -> ()
  in
  let () = set_synch (["true"] = synch) in
  Lwt.return_unit

let sync () : unit Lwt.t = Lwt.return_unit
