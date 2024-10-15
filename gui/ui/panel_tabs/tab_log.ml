(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let tab_is_active, set_tab_is_active = Hooked.S.create false
let tab_was_active = ref false

let line_count state =
  match state with
  | None -> 0
  | Some state ->
    let open Api_types_t in
    state.simulation_info_output.simulation_output_log_messages

let navli () =
  Ui_track_sim_status.label_news (Hooked.S.to_react_signal tab_is_active)
    (fun state -> line_count state)

let content () =
  let content_log_id = "content_log_id" in
  let update_log_content (log : string) =
    let dom_elt : 'a Js.t = Ui_common.id_dom content_log_id in
    dom_elt##.innerText := Js.string log
  in
  Hooked.S.register_lwt
    (Hooked.S.on tab_is_active State_simulation.dummy_model
       (Hooked.S.of_react_signal State_simulation.model))
    (fun _ ->
      Common.debug ~loc:__LOC__ "[tab_log] Updating log";
      State_simulation.eval_with_sim_manager_and_info ~label:__LOC__
        ~ready:(fun manager _ -> manager#simulation_detail_log_message)
        ~stopped:(fun _ -> Lwt.return (Result_util.ok ""))
        ~initializing:(fun _ -> Lwt.return (Result_util.ok ""))
        ()
      >|= fun (x : string Api.result) ->
      (match x.Result_util.value with
      | Ok x -> x
      | Error list ->
        String.concat "\n" (List.map (fun Result_util.{ text; _ } -> text) list))
      |> update_log_content);
  [
    Html.div
      ~a:
        [
          Html.a_id content_log_id; Html.a_class [ "panel-pre"; "panel-scroll" ];
        ]
      [ Html.txt "No log yet." ];
  ]

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () =
    Common.jquery_on "#navlog" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navlog" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        set_tab_is_active true)
  in
  ()

let onresize () : unit = ()
