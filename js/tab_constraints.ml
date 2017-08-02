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

(* todo add button to switch on/off the display of the information of each abstract domain *)
let non_relational = ref true
let relational = ref true
let site_accross = ref true
let parallel_bond = ref true

let print_string s list = (Html.pcdata s)::list
let print_newline list = print_string "\n" list
let print_int i l = print_string (string_of_int i) l
let print_site site list =
  let site_name, prop_opt, binding_opt = site in
  let list =
    match binding_opt with
    | Some Public_data.Free | None -> print_string Public_data.free list
    | Some Public_data.Wildcard -> print_string Public_data.wildcard list
    | Some Public_data.Bound_to_unknown -> print_string Public_data.bound list
    | Some (Public_data.Binding_type (ag,site)) ->
      print_string "!"
        (print_string ag (
            print_string "." (
              print_string site list)))
    | Some (Public_data.Bound_to i) ->
      print_string "!"
        (print_int i list)
  in
  let list =
    match prop_opt with
    | None -> list
    | Some a ->
      print_string a list
  in
  print_string site_name list

let print_agent agent list =
  let agent_name, interface = agent in
  let list = print_string ")" list in
  let list =
    snd
      (List.fold_left
         (fun (b,list) site ->
            let list =
              if b then
                print_string "," list
              else
                list
            in
            let list = print_site site list in
            true,list)
         (false,list) interface)
  in
  let list = (Html.pcdata "(")::list in
  let list = (Html.pcdata agent_name)::list in
  list

let print_site_graph agent_list list =
  snd (
    List.fold_left
      (fun (b,list) agent ->
         let list =
           if b then
             print_string "," list
           else list
         in
         true, print_agent agent list)
    (false,list)
    (List.rev agent_list))

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
                                  let list = print_newline list in
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
                                             print_site_graph site_graph
                                               (print_newline list)
                                           | _::_ | [] ->
                                             let list = print_newline list in
                                             let list = print_string " ]" list in
                                             let list =
                                               (snd
                                              (
                                               List.fold_left

                                                 (fun (bool,list) a ->
                                                    let list =
                                                      if bool then
                                                        (print_string " v " list)
                                                      else
                                                        list
                                                    in
                                                    let list =
                                                      print_site_graph a list
                                                    in                          true,
                                                    list)
                                                 (false,list)
                                                 (List.rev conclusion)
                                              )) in
                                             let list = print_string " [ " list
                                             in
                                             list)
                                         in

                                         let list = print_string " =>  " list in
                                         let list = print_site_graph hyp list in


                                        list)
                                      list
                                      b
                    in
                    let list = print_newline list in
                    let list = print_newline list in
                    let list = print_string a list in
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
