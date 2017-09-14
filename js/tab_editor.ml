(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let navli () = ReactiveData.RList.empty
let rightsubpanel_id : string = "rightsubpanel"
let rightsubpanel () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (React.S.bind
              Subpanel_editor.editor_full
              (fun editor_full ->
                 React.S.const
                   (if editor_full then
                      ["hidden"]
                    else
                      ["col-md-6"; "hidden-xs"; "hidden-sm"; "flex-content"])
              )
           )
       ]
    [Ui_common.navtabs "subnavtab"
       [ "contact_map", (Tab_contact_map.navli ())
       ; "influences", (Tab_influences.navli ())
       ; "constraints", (Tab_constraints.navli ())
       ];
     Ui_common.navcontent
       ~id:rightsubpanel_id
       []
       [ "contact_map", [], (Tab_contact_map.content ())
       ; "influences",  [], (Tab_influences.content ())
       ; "constraints", [], (Tab_constraints.content ())
       ]]

let content () =
  [Html.div
     ~a:[
       Tyxml_js.R.Html.a_class
         (React.S.bind
            Subpanel_editor.editor_full
            (fun editor_full ->
               React.S.const
                 (if editor_full then ["col-md-12";"flex-content"]
                  else ["col-md-6";"flex-content"])
            )
         )
     ]
     [Subpanel_editor.content ()];
   (rightsubpanel ()) ]

let childs_hide b =
  if b then
    let () = Tab_contact_map.parent_hide () in
    let () = Tab_influences.parent_hide () in
    Tab_constraints.parent_hide ()
  else
    let () = Tab_contact_map.parent_shown () in
    let () = Tab_influences.parent_shown () in
    Tab_constraints.parent_shown ()

let init_dead_rules () =
  React.S.l1
    (fun _ ->
       State_project.with_project
         ~label:__LOC__
         (fun (manager : Api.concrete_manager) ->
            (Lwt_result.map
               (fun dead_json ->
                  let list = Public_data.dead_rules_of_json dead_json in
                  let warnings =
                    List.fold_left
                      (fun acc rule ->
                         let message_text =
                           "Dead rule "^
                           if rule.Public_data.rule_label <> ""
                           then (" '"^rule.Public_data.rule_label^"'")
                           else if rule.Public_data.rule_ast <> ""
                           then rule.Public_data.rule_ast
                           else string_of_int rule.Public_data.rule_id in
                         {
                           Api_types_t.message_severity = `Warning;
                           Api_types_t.message_range =
                             Some rule.Public_data.rule_position;
                           Api_types_t.message_text;
                         } :: acc) [] list in
                  State_error.add_error __LOC__ warnings)
               manager#get_dead_rules) >>=
            fun out -> Lwt.return (Api_common.result_lift out)
         )
    )
    State_project.model

let onload () =
  let () = Subpanel_editor.onload () in
  let _ = init_dead_rules () in
  let () = Tab_contact_map.onload () in
  let () = Tab_influences.onload () in
  let () = Tab_constraints.onload () in
  let _ = React.S.map childs_hide Subpanel_editor.editor_full in
  let () = Common.jquery_on
      "#naveditor" "hide.bs.tab" (fun _ -> childs_hide true) in
  let () = Common.jquery_on
      "#naveditor" "shown.bs.tab" (fun _ -> childs_hide false) in
  ()

let onresize () : unit =
  let () = Subpanel_editor.onresize () in
  let () = Tab_contact_map.onresize () in
  let () = Tab_influences.onresize () in
  let () = Tab_constraints.onresize () in
  ()
