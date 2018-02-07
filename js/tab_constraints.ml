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

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

(* todo add button to switch on/off the display of the information of each abstract domain *)
let non_relational = ref true
let relational = ref true
let site_accross = ref true
let parallel_bond = ref true

let content () =
  let constraints,set_constraints = ReactiveData.RList.create [] in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun constraints_json ->
                    let constraints =
                      Public_data.lemmas_list_of_json
                        constraints_json
                    in
                    let () = ReactiveData.RList.set set_constraints
                        [ Html.p
                            (List.fold_left
                               (fun list (a,b) ->
                                  let list = Utility.print_newline list in
                                  let list =
                                    List.fold_left
                                      (fun list lemma ->
                                         let hyp =
                                           Public_data.get_hyp
                                             lemma
                                         in
                                         let conclusion =
                                           Public_data.get_refinement lemma
                                         in
                                         let list =              (
                                           match conclusion with
                                           | [site_graph] ->
                                             Utility.print_site_graph site_graph
                                               (Utility.print_newline list)
                                           | _::_ | [] ->
                                             let list = Utility.print_newline list in
                                             let list = Utility.print_string " ]" list in
                                             let list =
                                               (snd
                                              (
                                               List.fold_left

                                                 (fun (bool,list) a ->
                                                    let list =
                                                      if bool then
                                                        (Utility.print_string " v " list)
                                                      else
                                                        list
                                                    in
                                                    let list =
                                                      Utility.print_site_graph a list
                                                    in                          true,
                                                    list)
                                                 (false,list)
                                                 (List.rev conclusion)
                                              )) in
                                             let list = Utility.print_string " [ " list
                                             in
                                             list)
                                         in

                                         let list = Utility.print_string " =>  " list in
                                         let list = Utility.print_site_graph hyp list in


                                        list)
                                      list
                                      b
                    in
                    let list = Utility.print_newline list in
                    let list = Utility.print_newline list in
                    let list = Utility.print_string a list in
                                  list)
                               [] constraints)

                        ] in
                    ())
                 manager#get_constraints_list) >>=
              fun out -> Lwt.return (Api_common.result_lift out)
           )
      )
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll"]]
      constraints
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navconstraints" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navconstraints" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
