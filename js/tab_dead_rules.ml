(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let navli () = []

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let content () =
  let state_log , set_state_log = React.S.create ("" : string) in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun manager project_id ->
                (manager#project_dead_rules project_id)
                >>=
                (Api_common.result_bind_lwt
                   ~ok:(fun rule_ids ->
                       let () = set_state_log
                           (String.concat
                              ", "
                              (List.map Ckappa_sig.string_of_rule_id rule_ids)
                           ) in
                       Lwt.return (Api_common.result_ok ()))
                )
             )
      )
      (React.S.on tab_is_active None State_file.refresh_file) in
    [ Html.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll" ; "tab-log" ]]
      [ Tyxml_js.R.Html.pcdata state_log ]
    ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navdead_rules" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navdead_rules" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
