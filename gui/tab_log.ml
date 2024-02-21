(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let line_count state =
  match state with
  | None -> 0
  | Some state ->
    let open Api_types_t in
    state.simulation_info_output.simulation_output_log_messages

let navli () =
  Ui_common.label_news tab_is_active (fun state -> line_count state)

let dont_gc_me = ref []

let content () =
  let state_log, set_state_log = React.S.create ("" : string) in
  let () =
    dont_gc_me :=
      [
        Lwt_react.S.map_s
          (fun _ ->
            State_simulation.with_simulation_info ~label:__LOC__
              ~ready:(fun manager _ ->
                manager#simulation_detail_log_message
                >>= Api_common.result_bind_lwt
                      ~ok:(fun (log_messages : Api_types_j.log_message) ->
                        let () = set_state_log log_messages in
                        Lwt.return (Result_util.ok ())))
              ~stopped:(fun _ ->
                let () = set_state_log "" in
                Lwt.return (Result_util.ok ()))
              ())
          (React.S.on tab_is_active State_simulation.dummy_model
             State_simulation.model);
      ]
  in
  [
    Html.div
      ~a:[ Html.a_class [ "panel-pre"; "panel-scroll" ] ]
      [ Tyxml_js.R.Html.txt state_log ];
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () =
    Common.jquery_on "#navlog" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navlog" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        set_tab_is_active true)
  in
  ()

let onresize () : unit = ()
