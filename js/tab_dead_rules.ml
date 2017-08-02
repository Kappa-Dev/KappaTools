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

let print_string s list = (Html.pcdata s)::list
let print_newline list = print_string "\n" list

let content () =
  let dead_rules,set_dead_rules = ReactiveData.RList.create [] in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun dead_json ->
                    let list = Public_data.dead_rules_of_json dead_json in
                    let list =
                      if list = []
                      then
                        print_string "No dead rules" []
                      else
                        List.fold_left
                          (fun list rule ->
                             let string1 =
                               if rule.Public_data.rule_label <> ""
                               then ("rule '"^rule.Public_data.rule_label^"'")
                               else if rule.Public_data.rule_ast <> ""
                               then rule.Public_data.rule_ast
                               else ("rule "^string_of_int rule.Public_data.rule_id)
                             in
                             let buf = Buffer.create 0 in
                             let fmt = Format.formatter_of_buffer buf in
                             let () =
                                 Locality.print fmt rule.Public_data.rule_position
                             in
                             let () = Format.pp_print_flush fmt () in
                             let string2 = Buffer.contents buf in
                             let list = print_newline list in
                             print_string (string2^" "^string1) list
                          )
                          [] list
                    in
                    let () = ReactiveData.RList.set set_dead_rules
                        [ Html.p list ] in
                    ())
                 manager#get_dead_rules) >>=
              fun out -> Lwt.return (Api_common.result_lift out)
           )
      )
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll"]]
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
