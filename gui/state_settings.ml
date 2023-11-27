(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let clientIdParamId = Js.string "kappappClientId"
let _client_id : string ref = ref ""
let get_client_id () : string = !_client_id
let set_client_id (client_id : string) : unit = _client_id := client_id
let fontSizeParamId = Js.string "kappappFontSize"
let currentFontSize = ref 1.4

let initFromStorage () =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () ->
      let () = currentFontSize := 1.4 in
      _client_id := Common.guid ())
    (fun st ->
      let () =
        currentFontSize :=
          Js.Opt.case
            (st##getItem fontSizeParamId)
            (fun () -> 1.4)
            Js.parseFloat
      in
      _client_id :=
        Js.Opt.case (st##getItem clientIdParamId) Common.guid Js.to_string)

let set_parameters_as_default () =
  let v' = string_of_float !currentFontSize in
  let () =
    Js.Optdef.iter Dom_html.window##.localStorage (fun st ->
        st##setItem fontSizeParamId (Js.string v'))
  in
  Js.Optdef.iter Dom_html.window##.localStorage (fun st ->
      st##setItem clientIdParamId (Js.string !_client_id))

let updateFontSize ~delta =
  let () = currentFontSize := max 0.2 (min (!currentFontSize +. delta) 3.) in
  let v' = string_of_float !currentFontSize in
  let () =
    Dom_html.document##.body##.style##.fontSize := Js.string (v' ^ "em")
  in
  ()

let synch, set_synch = React.S.create false
let agent_coloring = Js.Unsafe.obj [||]

let init () : unit Lwt.t =
  let () = initFromStorage () in
  let client_ids = Common_state.url_args "client_id" in
  let synch = Common_state.url_args "synch" in
  let () =
    match client_ids with
    | client_id :: _ -> set_client_id client_id
    | [] -> ()
  in
  let () = set_synch ([ "true" ] = synch) in
  Lwt.return_unit

let sync () : unit Lwt.t = Lwt.return_unit
