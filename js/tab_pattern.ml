(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let navli () = ReactiveData.RList.empty

let content () =
  let scc =
    State_project.on_project_change_async ~on:tab_is_active
      (React.S.value State_file.model) State_file.model []
      (fun (manager : Api.concrete_manager) -> function
         | { State_file.current = None; _ } -> Lwt.return_nil
         | { State_file.current = Some { State_file.cursor_pos; rank; _ } ; directory } ->
           match Mods.IntMap.find_option rank directory with
           | None -> Lwt.return_nil
           | Some { State_file.name; _ } ->
             manager#mixture_at_position name cursor_pos >|=
             Result_util.fold
               ~ok:(fun mix -> [Html.txt (Format.asprintf "@[%a@]" Kappa_mixtures.User_graph.print_cc mix)])
               ~error:(fun mh ->
                   List.map
                     (fun m -> Html.p [Html.txt (Format.asprintf "@[%a@]" Result_util.print_message m)])
                     mh)) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll"]]
      (ReactiveData.RList.from_signal scc)
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navpattern" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navpattern" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()

let onresize () = ()
