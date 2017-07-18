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
  let constraints,set_constraints = ReactiveData.RList.create [] in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun constraints_json ->
                    let constraints =
                      Remanent_state.lemmas_list_of_json
                        constraints_json
                    in
                    let () = ReactiveData.RList.set set_constraints
                        [ Html.p
                            (List.fold_left
                               (fun list (a,b) ->

                                  (Html.pcdata a)::
                                  (Html.pcdata "\n")::
                                  (Html.pcdata "\n")::
                                  (List.fold_left
                                     (fun list lemma ->
                                        let hyp =
                                          Remanent_state.get_hyp
                                            lemma
                                        in
                                        let conclusion =
                                          Remanent_state.get_refinement lemma in
                                        (Html.pcdata ".")::
                                        (Html.pcdata " => ")::
                                        (if List.length conclusion = 1
                                         then
                                           (Html.pcdata ".\n")::list
                                         else
                                           (Html.pcdata "   [ \n   ")::
                                           (snd
                                              (
                                               List.fold_left

                                                 (fun (bool,list) a ->
                                                    let l =
                                                      (Html.pcdata ".")::list
                                                    in                          true,
                                                    if bool then
                                                      (Html.pcdata "\n v ")::
                                                      (Html.pcdata ".")::list
                                                    else
                                                      (Html.pcdata ".")::list)
                                                 (false,
                                                  (Html.pcdata "\n]\n")::list)
                                                 conclusion
                                              ))))
                                     ((Html.pcdata "\n")::(Html.pcdata "\n")::list)
                                     b))
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
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll" ; "tab-log" ]]
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
