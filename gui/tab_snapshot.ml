(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let tab_is_active, set_tab_is_active = React.S.create false

let current_snapshot, set_current_snapshot =
  React.S.create (None : (string * Data.snapshot) option)

type display_format = Kappa | Graph

let string_to_display_format = function
  | "Kappa" -> Some Kappa
  | "Graph" -> Some Graph
  | _ -> None

let display_format, set_display_format = React.S.create Kappa

let snapshot_count (state : Api_types_j.simulation_info option) : int =
  match state with
  | None -> 0
  | Some state ->
    state.Api_types_j.simulation_info_output
      .Api_types_j.simulation_output_snapshots

let navli () = Ui_common.badge (fun state -> snapshot_count state)
let select_id = "snapshot-select-id"
let display_id = "snapshot-map-display"

let configuration_template id additional_handlers : Widget_export.configuration
    =
  let json_handler =
    Widget_export.export_json ~serialize_json:(fun () ->
        match React.S.value current_snapshot with
        | None -> "null"
        | Some (_, s) -> Data.string_of_snapshot s)
  in
  let kappa_handler =
    {
      Widget_export.suffix = "ka";
      Widget_export.label = "kappa";
      Widget_export.export =
        (fun (filename : string) ->
          let data =
            Js.string
              (match React.S.value current_snapshot with
              | None -> ""
              | Some (_, s) -> Api_data.api_snapshot_kappa s)
          in
          Common.saveFile ~data ~mime:"application/json" ~filename);
    }
  in
  let dot_handler =
    {
      Widget_export.suffix = "dot";
      Widget_export.label = "dot";
      Widget_export.export =
        (fun (filename : string) ->
          let data =
            Js.string
              (match React.S.value current_snapshot with
              | None -> ""
              | Some (_, s) -> Api_data.api_snapshot_dot s)
          in
          Common.saveFile ~data ~mime:"text/vnd.graphviz" ~filename);
    }
  in
  let default_handlers = [ json_handler; kappa_handler; dot_handler ] in
  {
    Widget_export.id;
    Widget_export.handlers = default_handlers @ additional_handlers;
    Widget_export.show =
      React.S.map
        (fun model ->
          let simulation_info = State_simulation.model_simulation_info model in
          snapshot_count simulation_info > 0)
        State_simulation.model;
  }

(* Only allow the export of non-graphical data. *)
let configuration_kappa () : Widget_export.configuration =
  configuration_template "snapshot_kappa" []

(* The maps are rendered so allow the export of
   graphical data. *)
let configuration_graph () : Widget_export.configuration =
  configuration_template "snapshot_graph"
    [
      Widget_export.export_svg ~svg_div_id:display_id ();
      Widget_export.export_png ~svg_div_id:display_id ();
    ]

let format_select_id = "format_select_id"

let render_snapshot_graph (snapshot_js : Js_snapshot.snapshot Js.t)
    (snapshot : Data.snapshot) : unit =
  let () = Common.debug (Js.string (Data.string_of_snapshot snapshot)) in
  match React.S.value display_format with
  | Graph ->
    let json : string = Data.string_of_snapshot snapshot in
    let contact_map =
      Result_util.fold
        (React.S.value Tab_contact_map.contact_map_text)
        ~ok:(fun x -> x)
        ~error:(fun _ -> "null")
    in
    snapshot_js##setData ~contact_map:(Js.string contact_map) (Js.string json)
  | Kappa -> ()

let select_snapshot snapshot_js =
  let index =
    Js.Opt.bind
      (Ui_common.document##getElementById (Js.string select_id))
      (fun dom ->
        let snapshot_select_dom : Dom_html.inputElement Js.t =
          Js.Unsafe.coerce dom
        in
        let fileindex = Js.to_string snapshot_select_dom##.value in
        try Js.some (int_of_string fileindex) with _ -> Js.null)
  in
  let () = Common.debug index in
  let model = React.S.value State_simulation.model in
  let simulation_output = State_simulation.model_simulation_info model in
  match simulation_output with
  | None -> ()
  | Some state ->
    let index = Js.Opt.get index (fun _ -> 0) in
    if snapshot_count (Some state) > 0 then (
      let () =
        State_simulation.when_ready ~label:__LOC__ (fun manager ->
            manager#simulation_catalog_snapshot
            >>= Api_common.result_bind_lwt ~ok:(fun snapshot_ids ->
                    try
                      let snapshot_id : string = List.nth snapshot_ids index in
                      manager#simulation_detail_snapshot snapshot_id
                      >>= Api_common.result_bind_lwt
                            ~ok:(fun (snapshot : Data.snapshot) ->
                              let () =
                                set_current_snapshot
                                  (Some (snapshot_id, snapshot))
                              in
                              let () =
                                render_snapshot_graph snapshot_js snapshot
                              in
                              Lwt.return (Result_util.ok ()))
                    with
                    | Failure f -> Lwt.return (Api_common.result_error_msg f)
                    | Invalid_argument f ->
                      Lwt.return (Api_common.result_error_msg f)))
      in
      ()
    )

let select (snapshots : Api_types_j.snapshot_id list) =
  List.mapi
    (fun i snapshot_id ->
      Html.option
        ~a:
          ([ Html.a_value (string_of_int i) ]
          @
          if
            match React.S.value current_snapshot with
            | None -> false
            | Some (filename, _) -> filename = snapshot_id
          then
            [ Html.a_selected () ]
          else
            [])
        (Html.txt snapshot_id))
    snapshots

let snapshot_class :
    empty:(unit -> 'a) ->
    single:(unit -> 'a) ->
    multiple:(unit -> 'a) ->
    'a React.signal =
 fun ~empty ~single ~multiple ->
  React.S.map
    (fun model ->
      let simulation_info = State_simulation.model_simulation_info model in
      match snapshot_count simulation_info with
      | 0 -> empty ()
      | 1 -> single ()
      | _ -> multiple ())
    (React.S.on tab_is_active State_simulation.dummy_model
       State_simulation.model)

let snapshot_js : Js_snapshot.snapshot Js.t =
  Js_snapshot.create_snapshot display_id State_settings.agent_coloring

let dont_gc_me = ref []

let xml () =
  let list, handle = ReactiveData.RList.create [] in
  (* populate select *)
  let () =
    dont_gc_me :=
      [
        React.S.map
          (fun _ ->
            State_simulation.when_ready ~label:__LOC__ (fun manager ->
                manager#simulation_catalog_snapshot
                >>= Api_common.result_bind_lwt ~ok:(fun snapshot_ids ->
                        let () =
                          ReactiveData.RList.set handle (select snapshot_ids)
                        in
                        let () = select_snapshot snapshot_js in
                        Lwt.return (Result_util.ok ()))))
          (React.S.on tab_is_active State_simulation.dummy_model
             State_simulation.model);
      ]
  in
  let snapshot_label =
    Html.h4
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (snapshot_class
               ~empty:(fun () -> [ "hidden" ])
               ~single:(fun _ -> [ "oneliner"; "visible" ])
               ~multiple:(fun _ -> [ "hidden" ]));
        ]
      [
        Tyxml_js.R.Html.txt
          (React.S.map
             (fun snapshot ->
               match snapshot with
               | None -> ""
               | Some (snapshot_file, _) -> snapshot_file)
             current_snapshot);
      ]
  in
  let snapshot_select =
    Tyxml_js.R.Html.select
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (snapshot_class
               ~empty:(fun () -> [ "hidden" ])
               ~single:(fun _ -> [ "hidden" ])
               ~multiple:(fun _ -> [ "visible"; "form-control" ]));
          Html.a_id select_id;
        ]
      list
  in
  let snapshot_chooser = Html.div [ snapshot_label; snapshot_select ] in
  let toggle_controls ~kappa ~graph =
    Tyxml_js.R.Html.a_class
      (React.S.map
         (function
           | Kappa -> kappa
           | Graph -> graph)
         display_format)
  in
  let export_controls =
    [
      Html.div
        ~a:[ toggle_controls ~kappa:[ "visible" ] ~graph:[ "hidden" ] ]
        [ Widget_export.content (configuration_kappa ()) ];
      Html.div
        ~a:[ toggle_controls ~kappa:[ "hidden" ] ~graph:[ "visible" ] ]
        [ Widget_export.content (configuration_graph ()) ];
    ]
  in
  let kappa_snapshot_display =
    Html.div
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (React.S.map
               (fun display_format ->
                 "panel-scroll" :: "kappa-code" :: "flex-content"
                 ::
                 (match display_format with
                 | Kappa -> [ "visible" ]
                 | Graph -> [ "hidden" ]))
               display_format);
        ]
      [
        Tyxml_js.R.Html.txt
          (React.S.map
             (fun snapshot ->
               match snapshot with
               | None -> ""
               | Some (_, snapshot) -> Api_data.api_snapshot_kappa snapshot)
             current_snapshot);
      ]
  in
  let kappa_graph_display =
    Html.div
      ~a:
        [
          Tyxml_js.R.Html.a_class
            (React.S.map
               (fun display_format ->
                 "flex-content"
                 ::
                 (match display_format with
                 | Graph -> [ "visible" ]
                 | Kappa -> [ "hidden" ]))
               display_format);
        ]
      [%html
        {|
        <form  class="form-inline" id="snap-form">
        <label><input type="radio" name="mode" value="sumByMass" checked> Mass</label>
        <label><input type="radio" name="mode" value="sumByCount" > Count</label>
        <label><input type="radio" name="mode" value="sumBySize" > Size</label>
        <button id="recenterSnapButton" class="stateButton" type="button">Reset Zoom</button>
        <button id="rootButton" class="stateButton" type="button">Back to root</button>
        </form>
        <div class="flex-content" id="|}
          display_id {|"></div>|}]
  in
  let format_chooser =
    [%html
      {| <select class="form-control" id="|} format_select_id
        {|"><option value="Kappa" selected>kappa</option><option value="Graph">graph</option></select> |}]
  in
  [%html
    {|<div class="navcontent-view flex-content">
             <div class="row" style="margin : 5px;">
                <div class="col-sm-2 col-xs-4">
         |}
      [ format_chooser ]
      {|
               </div>
               <div class="col-sm-10 col-xs-8"> |}
      [ snapshot_chooser ]
      {| </div>
             </div>
                |}
      [ kappa_snapshot_display; kappa_graph_display ]
      {|
          </div>
          <div class="navcontent-controls">
          |}
      export_controls
      {|
          </div>
  |}]

let content () =
  [ Ui_common.toggle_element (fun state -> snapshot_count state > 0) (xml ()) ]

let onload () : unit =
  let snapshot_select_dom : Dom_html.inputElement Js.t =
    Ui_common.id_dom select_id
  in
  let format_select_dom : Dom_html.inputElement Js.t =
    Ui_common.id_dom format_select_id
  in
  let () =
    snapshot_select_dom##.onchange
    := Dom_html.handler (fun _ ->
           let () = Common.debug "onchange" in
           let () = select_snapshot snapshot_js in
           Js._true)
  in
  let update_format () =
    let format_text : string = Js.to_string format_select_dom##.value in
    match string_to_display_format format_text with
    | Some format ->
      let () = set_display_format format in
      (match React.S.value current_snapshot with
      | None -> ()
      | Some (_, snapshot) ->
        render_snapshot_graph
          (snapshot_js : Js_snapshot.snapshot Js.t)
          (snapshot : Data.snapshot))
    | None -> assert false
  in
  (* get initial value for display format *)
  let () = update_format () in
  (* update value for display format *)
  let () =
    format_select_dom##.onchange
    := Dom_html.handler (fun _ ->
           let () = update_format () in
           Js._true)
  in
  let () =
    Common.jquery_on "#navsnapshot" "hide.bs.tab" (fun _ ->
        set_tab_is_active false)
  in
  let () =
    Common.jquery_on "#navsnapshot" "shown.bs.tab" (fun _ ->
        let () = set_tab_is_active true in
        ())
  in
  let () = Widget_export.onload (configuration_kappa ()) in
  let () = Widget_export.onload (configuration_graph ()) in
  ()

let onresize () : unit = if React.S.value tab_is_active then snapshot_js##redraw
