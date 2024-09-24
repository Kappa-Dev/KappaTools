(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
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
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (React.S.bind Editor.editor_full (fun editor_full ->
               React.S.const
                 (if editor_full then
                    [ "hidden" ]
                  else
                    [ "col-md-6"; "hidden-xs"; "hidden-sm"; "flex-content" ])));
      ]
    [
      Ui_common.navtabs "subnavtab"
        [
          "contact_map", None, Subtab_contact_map.navli ();
          "influences", None, Subtab_influences.navli ();
          "constraints", None, Subtab_constraints.navli ();
          "polymers", None, Subtab_polymers.navli ();
        ];
      Ui_common.navcontent ~id:rightsubpanel_id []
        [
          "contact_map", [], Subtab_contact_map.content ();
          "influences", [], Subtab_influences.content ();
          "constraints", [], Subtab_constraints.content ();
          "polymers", [], Subtab_polymers.content ();
        ];
    ]

(** [childs_hide b] triggers change the state of child tabs to hide if b is True, or else to show *)
let childs_hide (b : bool) : unit =
  if b then (
    let () = Subtab_contact_map.parent_hide () in
    let () = Subtab_influences.parent_hide () in
    let () = Subtab_constraints.parent_hide () in
    Subtab_polymers.parent_hide ()
  ) else (
    let () = Subtab_contact_map.parent_shown () in
    let () = Subtab_influences.parent_shown () in
    let () = Subtab_constraints.parent_shown () in
    Subtab_polymers.parent_shown ()
  )

let content () =
  [
    Html.div
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (React.S.bind Editor.editor_full (fun editor_full ->
                 (* child hiding set here to avoid "gc" *)
                 let () = childs_hide editor_full in
                 React.S.const
                   (if editor_full then
                      [ "col-md-12"; "flex-content" ]
                    else
                      [ "col-md-6"; "flex-content" ])));
        ]
      [ Editor.content () ];
    rightsubpanel ();
  ]

let init_dead_rules () =
  React.S.l1
    (fun model ->
      State_error.wrap ~append:true "tab_editor_dead_rule"
        (State_project.eval_with_project ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
             if
               model.State_project.model_parameters
                 .State_project.show_dead_rules
             then
               manager#get_dead_rules
               >|= Result_util.fold
                     ~ok:(fun list ->
                       let warnings =
                         List.fold_left
                           (fun acc rule ->
                             if rule.Public_data.rule_hidden then
                               acc
                             else (
                               let text =
                                 "Dead rule "
                                 ^
                                 if rule.Public_data.rule_label <> "" then
                                   " '" ^ rule.Public_data.rule_label ^ "'"
                                 else if rule.Public_data.rule_ast <> "" then
                                   rule.Public_data.rule_ast
                                 else
                                   string_of_int rule.Public_data.rule_id
                               in
                               {
                                 Result_util.severity = Logs.Warning;
                                 Result_util.range =
                                   Some rule.Public_data.rule_position;
                                 Result_util.text;
                               }
                               :: acc
                             ))
                           [] list
                       in
                       List.rev warnings)
                     ~error:(fun mh -> mh)
               >|= Api_common.err_result_of_msgs ?result_code:None
             else
               Lwt.return (Result_util.ok ()))))
    State_project.model

let init_dead_agents () =
  React.S.l1
    (fun model ->
      State_error.wrap ~append:true "tab_editor_dead_agent"
        (State_project.eval_with_project ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
             if
               model.State_project.model_parameters
                 .State_project.show_dead_agents
             then
               manager#get_dead_agents
               >|= Result_util.fold
                     ~ok:(fun list ->
                       let warnings =
                         List.fold_left
                           (fun acc agent ->
                             let text =
                               "Dead agent "
                               ^
                               if agent.Public_data.agent_ast <> "" then
                                 agent.Public_data.agent_ast
                               else
                                 string_of_int agent.Public_data.agent_id
                             in
                             List.fold_left
                               (fun acc range ->
                                 {
                                   Result_util.severity = Logs.Warning;
                                   Result_util.range = Some range;
                                   Result_util.text;
                                 }
                                 :: acc)
                               acc agent.Public_data.agent_position)
                           [] list
                       in
                       List.rev warnings)
                     ~error:(fun mh -> mh)
               >|= Api_common.err_result_of_msgs ?result_code:None
             else
               Lwt.return (Result_util.ok ()))))
    State_project.model

let init_non_weakly_reversible_transitions () =
  React.S.l1
    (fun model ->
      State_error.wrap ~append:true "tab_editor_dead_rule"
        (State_project.eval_with_project ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
             if
               model.State_project.model_parameters
                 .State_project.show_non_weakly_reversible_transitions
             then
               manager#get_non_weakly_reversible_transitions
               >|= Result_util.fold
                     ~ok:(fun list ->
                       let warnings =
                         List.fold_left
                           (fun acc (rule, context_list) ->
                             if rule.Public_data.rule_hidden then
                               acc
                             (* hint: reversible rule are always weakly reversible *)
                             else (
                               let plural, skip, tab =
                                 match context_list with
                                 | [] | [ _ ] -> "", "", " "
                                 | _ :: _ -> "s", "\n", "\t"
                               in
                               let text =
                                 Format.asprintf
                                   "Rule %s may induce non weakly reversible \
                                    events in the following context%s:%s%a"
                                   (if rule.Public_data.rule_label <> "" then
                                      " '" ^ rule.Public_data.rule_label ^ "'"
                                    else if rule.Public_data.rule_ast <> "" then
                                      rule.Public_data.rule_ast
                                    else
                                      string_of_int rule.Public_data.rule_id)
                                   plural skip
                                   (Pp.list
                                      (fun fmt -> Format.fprintf fmt "%s" skip)
                                      (fun fmt (a, b) ->
                                        Format.fprintf fmt "%s%s -> %s " tab a b))
                                   context_list
                               in
                               (* to do, add the potential contexts *)
                               {
                                 Result_util.severity = Logs.Warning;
                                 Result_util.range =
                                   Some rule.Public_data.rule_position;
                                 Result_util.text;
                               }
                               :: acc
                             ))
                           [] list
                       in
                       List.rev warnings)
                     ~error:(fun mh -> mh)
               >|= Api_common.err_result_of_msgs ?result_code:None
             else
               Lwt.return (Result_util.ok ()))))
    State_project.model

let dont_gc_me = ref []

let onload () =
  let () = Editor.onload () in
  dont_gc_me := init_dead_rules () :: !dont_gc_me;
  dont_gc_me := init_dead_agents () :: !dont_gc_me;
  dont_gc_me := init_non_weakly_reversible_transitions () :: !dont_gc_me;
  let () = Subtab_contact_map.onload () in
  let () = Subtab_influences.onload () in
  let () = Subtab_constraints.onload () in
  let () = Subtab_polymers.onload () in
  let () =
    Common.jquery_on "#naveditor" "hide.bs.tab" (fun _ -> childs_hide true)
  in
  let () =
    Common.jquery_on "#naveditor" "shown.bs.tab" (fun _ -> childs_hide false)
  in
  ()

let onresize () : unit =
  let () = Editor.onresize () in
  let () = Subtab_contact_map.onresize () in
  let () = Subtab_influences.onresize () in
  let () = Subtab_constraints.onresize () in
  let () = Subtab_polymers.onresize () in
  ()
