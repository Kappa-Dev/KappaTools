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
       [ "contact_map", None, (Tab_contact_map.navli ())
       ; "influences",  None, (Tab_influences.navli ())
       ; "constraints", None, (Tab_constraints.navli ())
       ; "polymers",    None, (Tab_polymers.navli ())
       ];
     Ui_common.navcontent
       ~id:rightsubpanel_id
       []
       [ "contact_map", [], (Tab_contact_map.content ())
       ; "influences",  [], (Tab_influences.content ())
       ; "constraints", [], (Tab_constraints.content ())
       ; "polymers", [], (Tab_polymers.content ())
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
    let () = Tab_constraints.parent_hide () in
    Tab_polymers.parent_hide ()
  else
    let () = Tab_contact_map.parent_shown () in
    let () = Tab_influences.parent_shown () in
    let () = Tab_constraints.parent_shown () in
    Tab_polymers.parent_shown ()

let init_dead_rules () =
  React.S.l1
    (fun model ->
       State_error.wrap  ~append:true "tab_editor_dead_rule"
         (State_project.with_project
            ~label:__LOC__
            (fun (manager : Api.concrete_manager) ->
               if model.State_project.model_parameters.
                    State_project.show_dead_rules then
                 manager#get_dead_rules >>= function
                 | Result.Ok list ->
                   let warnings =
                     List.fold_left
                       (fun acc rule ->
                          if rule.Public_data.rule_hidden
                          then acc
                          else
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
                   let warnings = List.rev warnings in
                   Lwt.return (Api_common.result_messages warnings)
                 | Result.Error mh ->
                   Lwt.return (Api_common.method_handler_messages mh)
               else
                 Lwt.return (Api_common.result_ok ()))
         ))
    State_project.model

let init_non_weakly_reversible_transitions () =
  React.S.l1
    (fun model ->
       State_error.wrap  ~append:true "tab_editor_dead_rule"
         (State_project.with_project
            ~label:__LOC__
            (fun (manager : Api.concrete_manager) ->
               if model.State_project.model_parameters.
                    State_project.show_non_weakly_reversible_transitions then
                 manager#get_non_weakly_reversible_transitions >>= function
                 | Result.Ok list ->
                   let warnings =
                     List.fold_left
                       (fun acc (rule,context_list) ->
                          if rule.Public_data.rule_hidden
                          then acc (* hint: reversible rule are always weakly reversible *)
                          else
                            let plural,skip,tab =
                              match context_list with
                              | [] | [_] -> "",""," "
                              | _::_ -> "s","\n","\t"
                            in
                            let message_text =
                              Format.asprintf
                                "Rule %s may induce non wealky reversible events in the following context%s:%s%a"
                                
                                (if rule.Public_data.rule_label <> ""
                                 then (" '"^rule.Public_data.rule_label^"'")
                                 else if rule.Public_data.rule_ast <> ""
                                 then rule.Public_data.rule_ast
                                 else string_of_int rule.Public_data.rule_id)
                                plural
                                skip
                                ( Pp.list
                                    (fun fmt -> Format.fprintf fmt "%s" skip)
                                    (fun fmt (a,b) ->
                                       Format.fprintf fmt "%s%s -> %s " tab a b)
                                )
                                context_list
                            in
                            (* to do, add the potential contexts *)
                            {
                              Api_types_t.message_severity = `Warning;
                              Api_types_t.message_range =
                                Some rule.Public_data.rule_position;
                              Api_types_t.message_text;
                            } :: acc) [] list in
                   let warnings = List.rev warnings in
                   Lwt.return (Api_common.result_messages warnings)
                 | Result.Error mh -> Lwt.return (Api_common.method_handler_messages mh)
               else
                 Lwt.return (Api_common.result_ok ()))
         )
    )
    State_project.model

let onload () =
  let () = Subpanel_editor.onload () in
  let _ = init_dead_rules () in
  let _ = init_non_weakly_reversible_transitions () in
  let () = Tab_contact_map.onload () in
  let () = Tab_influences.onload () in
  let () = Tab_constraints.onload () in
  let () = Tab_polymers.onload () in
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
  let () = Tab_polymers.onresize () in
  ()
