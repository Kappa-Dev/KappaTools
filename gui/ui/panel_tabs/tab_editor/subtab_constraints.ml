(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix
open Lwt.Syntax

let navli () = ReactiveData.RList.empty
let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

(* todo add button to switch on/off the display of the information of each abstract domain
   let non_relational = ref true
   let relational = ref true
   let site_accross = ref true
   let parallel_bond = ref true
*)

let content () =
  let print_refinement_constraint lemma list =
    let hyp = Public_data.get_hyp lemma in
    let conclusion = Public_data.get_refinement lemma in
    let contains_formula =
      List.exists (fun (_, formula) -> Option.is_some formula) conclusion
    in
    let prefix =
      if contains_formula then
        "\n\t\t"
      else
        ""
    in
    let list =
      match conclusion with
      | [ (site_graph, formula) ] ->
        let list =
          Html_utility.print_formula_option formula
            (Html_utility.print_newline list)
        in
        Html_utility.print_site_graph site_graph list
      | _ :: _ | [] ->
        let list = Html_utility.print_newline list in
        let list = Html_utility.print_string (prefix ^ " ]") list in
        let list =
          snd
            (List.fold_left
               (fun (bool, list) (a, formula) ->
                 let list =
                   if bool then
                     Html_utility.print_string (prefix ^ " v ") list
                   else
                     list
                 in
                 let list = Html_utility.print_formula_option formula list in
                 let list = Html_utility.print_site_graph a list in
                 true, list)
               (false, list) (List.rev conclusion))
        in
        let list = Html_utility.print_string "[ " list in
        list
    in
    let list = Html_utility.print_string "  =>  " list in
    let list = Html_utility.print_site_graph hyp list in
    list
  in
  let print_conditionally_dead_agents
      ((agent : Public_data.agent_kind), formula) list =
    let list = Html_utility.print_newline list in
    let list = Html_utility.print_formula formula list in
    let list = Html_utility.print_string " can occur in the model if " list in
    Html_utility.print_agent_kind agent list
  in
  let print_conditionally_dead_rules ((rule : Public_data.rule), formula) list =
    let list = Html_utility.print_newline list in
    let list = Html_utility.print_formula formula list in
    let list = Html_utility.print_string " could be applied if " list in
    Html_utility.print_rule rule list
  in

  let print_panel header print_function content =
    let texts =
      List.fold_left (fun list c -> print_function c list) [] content
    in
    let title =
      Html.div ~a:[ Html.a_class [ "panel-heading" ] ] [ Html.txt header ]
    in
    let content =
      Html.div ~a:[ Html.a_class [ "panel-body"; "panel-pre" ] ] texts
    in
    Html.div ~a:[ Html.a_class [ "panel"; "panel-default" ] ] [ title; content ]
  in
  let add_constraints constraints =
    List.fold_left
      (fun panels (a, b) ->
        print_panel a print_refinement_constraint b :: panels)
      [] constraints
  in
  let add_rules
      (conditionally_dead_rules : Public_data.rule_deadness_conditions) =
    if List.length conditionally_dead_rules > 0 then
      print_panel "Conditions for rule deadness" print_conditionally_dead_rules
        conditionally_dead_rules
      :: []
    else
      [ Html.div [] ]
  in
  let add_agents
      (conditionally_dead_agents : Public_data.agent_deadness_conditions) =
    if List.length conditionally_dead_agents > 0 then
      print_panel "Conditions for agent deadness"
        print_conditionally_dead_agents conditionally_dead_agents
      :: []
    else
      [ Html.div [] ]
  in
  let print_error_message r =
    let title =
      Html.div
        ~a:[ Html.a_class [ "panel-heading" ] ]
        [ Html.txt "KaSa has failed" ]
    in
    let content =
      Html.div
        ~a:[ Html.a_class [ "panel-body"; "panel-pre" ] ]
        (List.map
           (fun m ->
             Html.p
               [
                 Html.txt (Format.asprintf "@[%a@]" Result_util.print_message m);
               ])
           r)
    in
    let out =
      Html.div
        ~a:[ Html.a_class [ "panel"; "panel-danger" ] ]
        [ title; content ]
    in
    [ out ]
  in
  let constraints_div =
    State_project.on_project_change_async ~on:tab_is_active ()
      (React.S.const ()) [] (fun (manager : Api.concrete_manager) () ->
        let* out_constraints =
          manager#get_constraints_list
          >|= Result_util.fold ~ok:add_constraints ~error:print_error_message
        in
        let* out_rules =
          manager#get_conditionally_dead_rules
          >|= Result_util.fold ~ok:add_rules ~error:print_error_message
        in
        let* out_agents =
          manager#get_conditionally_dead_agents
          >|= Result_util.fold ~ok:add_agents ~error:print_error_message
        in
        Lwt.return (out_constraints @ out_rules @ out_agents))
  in
  [
    Tyxml_js.R.Html5.div
      ~a:[ Html.a_class [ "panel-scroll" ] ]
      (ReactiveData.RList.from_signal constraints_div);
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () =
    Common.jquery_on "#navconstraints" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navconstraints" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        set_tab_is_active true)
  in
  ()

let onresize () : unit = ()
