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

let fontSizeParamId = Js.string "kappappFontSize"
let initFontSize () =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> 1.4)
    (fun st ->
       Js.Opt.case (st##getItem fontSizeParamId) (fun () -> 1.4) Js.parseFloat)
let currentFontSize = ref (initFontSize ())
let storeFontSize () =
  let v' = string_of_float !currentFontSize in
  Js.Optdef.iter
    Dom_html.window##.localStorage
    (fun st -> st##setItem fontSizeParamId (Js.string v'))
let updateFontSize ~delta =
  let () = currentFontSize :=
      max 0.2 (min (!currentFontSize +. delta) 3.) in
  let v' = string_of_float !currentFontSize in
  let () = Dom_html.document##.body##.style##.fontSize :=
      Js.string (v'^"em") in
  ()

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
