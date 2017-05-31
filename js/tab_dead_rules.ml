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
  let dead_rules,set_dead_rules = ReactiveData.RList.create [] in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) project_id ->
              (Lwt_result.map
                 (fun dead_json ->
                    let () = ReactiveData.RList.set set_dead_rules
                        [ Html.p [Html.pcdata
                                    (Yojson.Basic.to_string dead_json) ] ] in
                    ())
                 (manager#get_dead_rules project_id)) >>=
              fun out -> Lwt.return (Api_common.result_lift out)
           )
      )
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll" ; "tab-log" ]]
      dead_rules
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
