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
let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

(* todo add button to switch on/off the display of the information of each abstract domain
   let non_relational = ref true
   let relational = ref true
   let site_accross = ref true
   let parallel_bond = ref true
*)

let content () =
  let constraints_div =
    State_project.on_project_change_async ~on:tab_is_active ()
      (React.S.const ()) [] (fun (manager : Api.concrete_manager) () ->
        manager#get_constraints_list
        >|= Result_util.fold
              ~ok:(fun constraints ->
                List.fold_left
                  (fun panels (a, b) ->
                    (*match b with
                       | [] -> panels
                         | _ :: _ ->*)
                    let texts =
                      List.fold_left
                        (fun list lemma ->
                          let hyp = Public_data.get_hyp lemma in
                          let conclusion = Public_data.get_refinement lemma in
                          let list =
                            match conclusion with
                            | [ site_graph ] ->
                              Utility.print_site_graph site_graph
                                (Utility.print_newline list)
                            | _ :: _ | [] ->
                              let list = Utility.print_newline list in
                              let list = Utility.print_string " ]" list in
                              let list =
                                snd
                                  (List.fold_left
                                     (fun (bool, list) a ->
                                       let list =
                                         if bool then
                                           Utility.print_string " v " list
                                         else
                                           list
                                       in
                                       let list =
                                         Utility.print_site_graph a list
                                       in
                                       true, list)
                                     (false, list) (List.rev conclusion))
                              in
                              let list = Utility.print_string "[ " list in
                              list
                          in
                          let list = Utility.print_string "  =>  " list in
                          let list = Utility.print_site_graph hyp list in
                          list)
                        [] (List.rev b)
                    in
                    let title =
                      Html.div
                        ~a:[ Html.a_class [ "panel-heading" ] ]
                        [ Html.txt a ]
                    in
                    let content =
                      Html.div
                        ~a:[ Html.a_class [ "panel-body"; "panel-pre" ] ]
                        texts
                    in
                    Html.div
                      ~a:[ Html.a_class [ "panel"; "panel-default" ] ]
                      [ title; content ]
                    :: panels)
                  [] constraints)
              ~error:(fun r ->
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
                             Html.txt
                               (Format.asprintf "@[%a@]"
                                  Result_util.print_message m);
                           ])
                       r)
                in
                let out =
                  Html.div
                    ~a:[ Html.a_class [ "panel"; "panel-danger" ] ]
                    [ title; content ]
                in
                [ out ]))
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
