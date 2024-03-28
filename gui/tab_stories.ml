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
let none_checkbox = Html.input ~a:[ Html.a_input_type `Checkbox ] ()

let weak_checkbox =
  Html.input ~a:[ Html.a_input_type `Checkbox; Html.a_checked () ] ()

let strong_checkbox = Html.input ~a:[ Html.a_input_type `Checkbox ] ()
let none_box = Tyxml_js.To_dom.of_input none_checkbox
let weak_box = Tyxml_js.To_dom.of_input weak_checkbox
let strong_box = Tyxml_js.To_dom.of_input strong_checkbox

let launch_button =
  Html.button
    ~a:[ Html.a_class [ "btn"; "btn-default" ]; Html.a_button_type `Submit ]
    [ Html.txt "Launch" ]

let story_list, list_control = React.S.create []

let story_list_html =
  ReactiveData.RList.map
    (fun (id, cm) ->
      Html.option
        ~a:[ Html.a_value (string_of_int id) ]
        (Html.txt
           (Format.asprintf "@[%i (%a)@]" id
              Kastor_client.print_compression_modes cm)))
    (ReactiveData.RList.from_signal story_list)

let select_stories = Tyxml_js.R.Html5.select story_list_html
let select_stories_dom = Tyxml_js.To_dom.of_select select_stories

let%html setup_form =
  {|<form class="form-inline">
    <div class="form-group">
    <label>Compression level</label>
    <div class="checkbox"><label>|}
    [ none_checkbox ]
    {| Causal</label></div>
    <div class="checkbox"><label>|}
    [ weak_checkbox ]
    {| Weakly</label></div>
    <div class="checkbox"><label>|}
    [ strong_checkbox ]
    {| Strongly</label></div>
    </div>
    <div class="form-group">|}
    [ launch_button ]
    {|</div>
    <div class="form-group">
    <label>Selected story</label>|}
    [ select_stories ]
    {|
    </div>
    </form>|}

let story_log, log_control = React.S.create []
let current_info, set_info = React.S.create ""

let story_log_html =
  ReactiveData.RList.map
    (fun line -> Html.txt (line ^ "\n"))
    (ReactiveData.RList.rev (ReactiveData.RList.from_signal story_log))

let log_div =
  Tyxml_js.R.Html5.div ~a:[ Html.a_class [ "panel-pre" ] ] story_log_html

let log_panel =
  [
    Ui_common.navtabs "storylognavtab"
      [
        "story_computation_log", None, ReactiveData.RList.empty;
        "story_info_log", None, ReactiveData.RList.empty;
      ];
    Ui_common.navcontent [ "panel-scroll" ]
      [
        "story_computation_log", [], [ log_div ];
        "story_info_log", [ "panel-pre" ], [ Tyxml_js.R.Html5.txt current_info ];
      ];
  ]

let graph_display_id = "story_graph_display"
let story_graph = Js_story.create_story_rendering graph_display_id

let content () =
  [
    Html.div
      ~a:[ Html.a_class [ "col-md-5"; "flex-content" ] ]
      (setup_form :: log_panel);
    Html.div
      ~a:
        [
          Html.a_id graph_display_id;
          Html.a_class [ "col-md-7"; "flex-content" ];
        ]
      [];
  ]

let lift_result = function
  | Result.Ok x -> Result_util.ok x
  | Result.Error e -> Api_common.result_error_msg e

let do_update_compression_level () =
  State_project.eval_with_project ~label:"Config compression" (fun manager ->
      let causal = Js.to_bool none_box##.checked in
      let weak = Js.to_bool weak_box##.checked in
      let strong = Js.to_bool strong_box##.checked in
      manager#config_story_computation { Api.causal; Api.weak; Api.strong }
      >|= lift_result)

let update_compression_level =
  Dom_html.handler (fun _ ->
      let _ = do_update_compression_level () in
      Js._false)

let set_a_story =
  let pred_id = ref max_int in
  fun () ->
    let va = Js.to_string select_stories_dom##.value in
    if va = "" then (
      let () = pred_id := max_int in
      Lwt.return (Result_util.ok ())
    ) else (
      let id = int_of_string va in
      if !pred_id <> id then (
        let () = pred_id := id in
        State_project.eval_with_project ~label:"Launch stories" (fun manager ->
            match Mods.IntMap.find_option id manager#story_list with
            | None -> Lwt.return (Result_util.ok ())
            | Some (_cm, d, v) ->
              let () =
                set_info
                  (Format.asprintf "@[ids: @[%a@]@ t=@[%a@]@ event=@[%a@]@]"
                     (Pp.list Pp.comma
                        (Pp.list Pp.comma (fun f d ->
                             Format.pp_print_int f
                               d.Trace.Simulation_info.story_id)))
                     d
                     (Pp.list Pp.comma
                        (Pp.list Pp.comma (fun f d ->
                             Format.pp_print_float f
                               d.Trace.Simulation_info.story_time)))
                     d
                     (Pp.list Pp.comma
                        (Pp.list Pp.comma (fun f d ->
                             Format.pp_print_int f
                               d.Trace.Simulation_info.story_event)))
                     d)
              in
              let () =
                story_graph##setData
                  (Js.string (Yojson.Basic.to_string (Graph_json.to_json v)))
              in
              Lwt.return (Result_util.ok ()))
      ) else
        Lwt.return (Result_util.ok ())
    )

let rec inspect_stories () =
  State_project.eval_with_project ~label:"Stories list" (fun manager ->
      let () =
        list_control
          (Mods.IntMap.fold
             (fun id (cm, _, _) acc -> (id, cm) :: acc)
             manager#story_list [])
      in
      let () = log_control manager#story_log in
      set_a_story ())
  >>= fun _ ->
  State_project.eval_with_project ~label:"Stories computing" (fun manager ->
      Lwt.return (Result_util.ok manager#is_computing))
  >>= Result_util.fold
        ~ok:(fun b ->
          if b && React.S.value tab_is_active then
            Js_of_ocaml_lwt.Lwt_js.sleep 3. >>= inspect_stories
          else
            Lwt.return_unit)
        ~error:(fun _ -> Lwt.return_unit)

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () =
    Common.jquery_on "#navstories" "hide.bs.tab" (fun _ ->
        let () = tab_was_active := false in
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navstories" "shown.bs.tab" (fun _ ->
        let () = tab_was_active := true in
        let () = set_tab_is_active true in
        let _ = do_update_compression_level () in
        Lwt.async inspect_stories)
  in
  let () = none_box##.onchange := update_compression_level in
  let () = weak_box##.onchange := update_compression_level in
  let () = strong_box##.onchange := update_compression_level in
  let () =
    select_stories_dom##.onchange
    := Dom_html.handler (fun _ ->
           let _ = set_a_story () in
           Js._false)
  in
  let () =
    (Tyxml_js.To_dom.of_button launch_button)##.onclick
    := Dom_html.handler (fun _ ->
           let _ =
             State_project.eval_with_project ~label:"Launch stories"
               (fun manager ->
                 if manager#story_is_computing then
                   Lwt.return (Result_util.ok ())
                 else
                   manager#simulation_raw_trace
                   >>= Api_common.result_bind_lwt ~ok:(fun trace ->
                           manager#raw_launch_story_computation trace
                           >|= lift_result))
           in
           let () = Lwt.async inspect_stories in
           Js._false)
  in
  ()

let onresize () : unit = ()
