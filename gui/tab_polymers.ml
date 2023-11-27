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
let site a = [ a, None, Some (Public_data.Bound_to 1), None ]

let print_edge ((a, b), (c, d)) list =
  Utility.print_newline (Utility.print_site_graph [ a, site b; c, site d ] list)

let content () =
  let scc =
    State_project.on_project_change_async ~on:tab_is_active ()
      (React.S.const ()) [] (fun (manager : Api.concrete_manager) () ->
        manager#get_potential_polymers (Some Public_data.High)
          (Some Public_data.High)
        (*TODO: make these options tunable *)
        >|= Result_util.fold
              ~ok:(fun (_, _, scc) ->
                let scc = List.rev_map List.rev scc in
                let output =
                  if scc = [] || scc = [ [] ] then
                    Utility.print_string
                      "The size of biomolecular compounds is uniformly bounded."
                      []
                  else (
                    let list =
                      List.fold_left
                        (fun list list_edges ->
                          let list = Utility.print_newline list in
                          List.fold_left
                            (fun list ((a, b), (c, d)) ->
                              print_edge ((a, b), (c, d)) list)
                            list list_edges)
                        [] scc
                    in
                    let list = Utility.print_newline list in
                    let list =
                      Utility.print_string
                        "The following bonds may form arbitrary long chains of \
                         agents:"
                        list
                    in
                    list
                  )
                in
                [ Html.p output ])
              ~error:(fun mh ->
                List.map
                  (fun m ->
                    Html.p
                      [
                        Html.txt
                          (Format.asprintf "@[%a@]" Result_util.print_message m);
                      ])
                  mh))
  in
  [
    Tyxml_js.R.Html5.div
      ~a:[ Html.a_class [ "panel-pre"; "panel-scroll" ] ]
      (ReactiveData.RList.from_signal scc);
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () =
    Common.jquery_on "#navpolymers" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navpolymers" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        set_tab_is_active true)
  in
  ()

let onresize () : unit = ()
