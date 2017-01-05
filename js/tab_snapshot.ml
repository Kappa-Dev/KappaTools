(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
module Html = Tyxml_js.Html5

let current_snapshot, set_current_snapshot =
  React.S.create (None : Api_types_j.snapshot option)

type display_format = Kappa | Graph
let display_format_to_string =
  function
  | Kappa -> "Kappa"
  | Graph -> "Graph"
let string_to_display_format =
  function
  | "Kappa" -> Some Kappa
  | "Graph" -> Some Graph
  | _ -> None

let display_format, set_display_format = React.S.create Graph

let snapshot_count
    (state : Api_types_j.simulation_info option) :
  int =
  match state with
  | None -> 0
  | Some state ->
    state.Api_types_j.simulation_info_output.Api_types_j.simulation_output_snapshots

let navli (t : Ui_simulation.t) =
  Ui_common.badge
    t
    (fun state -> snapshot_count state)

let select_id = "snapshot-select-id"
let display_id = "snapshot-map-display"

let configuration (t : Ui_simulation.t) : Widget_export.configuration =
  let simulation_output = (Ui_simulation.simulation_output t) in
  { Widget_export.id = "snapshot"
  ; Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
          ~serialize_json:(fun () ->
              (match
		 (React.S.value current_snapshot : Api_types_j.snapshot option) with
              | None -> "null"
              | Some s -> Api_types_j.string_of_snapshot s
              )
            )
      ; { Widget_export.suffix = "ka"
        ; Widget_export.label = "kappa"
        ; Widget_export.export =
	    fun (filename : string) ->
              let data = match
		  (React.S.value current_snapshot : Api_types_j.snapshot option) with
              | None -> ""
              | Some s ->
                Api_data_v1.api_snapshot_kappa (Api_data_v1.api_snapshot s)
              in
              Common.saveFile
		~data:data
		~mime:"application/json"
		~filename:filename
        }
      ; { Widget_export.suffix = "dot"
        ; Widget_export.label = "dot"
        ; Widget_export.export =
	    fun (filename : string) ->
              let data = match
		  (React.S.value current_snapshot : Api_types_j.snapshot option) with
              | None -> ""
              | Some s -> Api_data_v1.api_snapshot_dot (Api_data_v1.api_snapshot s)
              in
              Common.saveFile
		~data:data
		~mime:"text/vnd.graphviz"
		~filename:filename
        }
      ];
    show = React.S.map
        (fun state -> snapshot_count state > 0)
        simulation_output

  }

let format_select_id = "format_select_id"

let content (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let list, handle = ReactiveData.RList.create [] in
  let select (snapshots : Api_types_j.snapshot_id list) =
    List.mapi
      (fun i snapshot_id ->
         Html.option
	   ~a:([ Html.a_value (string_of_int i)]
	   @
           if (match (React.S.value current_snapshot) with
	   | None -> false
           | Some s -> s.Api_types_j.snapshot_file = snapshot_id)
	   then [Html.a_selected ()]
           else [])
           (Html.pcdata
	      (Ui_common.option_label snapshot_id)))
              snapshots
  in
  (* populate select *)
  let _ = React.S.map
      (fun _ ->
         Ui_simulation.manager_operation
           t
           (fun
             manager
             project_id
             simulation_id ->
             (manager#simulation_info_snapshot
                project_id
                simulation_id
             ) >>=
             (Api_common.result_map
                ~ok:(fun _ (data : Api_types_t.snapshot_info) ->
                    let () = ReactiveData.RList.set handle (select data.Api_types_t.snapshot_ids) in
                    Lwt.return_unit)
                ~error:(fun _ errors  ->
                    let () = Ui_state.set_model_error __LOC__ errors in
                    Lwt.return_unit)
             )
           )
      )
      simulation_output
  in
  let snapshot_class :
    empty:(unit -> 'a) ->
    single:(unit -> 'a) ->
    multiple:(unit -> 'a) -> 'a React.signal =
    fun
      ~empty
      ~single
      ~multiple ->
    React.S.map
      (fun state ->
         match snapshot_count state with
         | 0 -> empty ()
         | 1 -> single ()
         | _ -> multiple ())
      simulation_output
  in
  let snapshot_label =
    Html.h4
      ~a:[ Tyxml_js.R.Html.a_class
             (snapshot_class
                ~empty:(fun () -> ["hidden"])
                ~single:(fun _ -> ["visible"])
                ~multiple:(fun _ -> ["hidden"]))
         ]
      [ Tyxml_js.R.Html.pcdata
          (React.S.map
             (fun snapshot ->
                match snapshot with
                | None -> ""
                | Some snapshot ->
                  (Ui_common.option_label snapshot.Api_types_j.snapshot_file))
             current_snapshot)
      ]
  in
  let snapshot_select =
    Tyxml_js.R.Html.select
      ~a:[ Tyxml_js.R.Html.a_class
             (snapshot_class
                ~empty:(fun () -> [ "hidden" ])
                ~single:(fun _ -> [ "hidden" ])
                ~multiple:(fun _ -> ["visible" ; "form-control"])) ;
           Html.a_id select_id ]
      list
  in
  let snapshot_chooser = Html.div [ snapshot_label ; snapshot_select ]
  in
  let export_controls =
    Widget_export.content (configuration t)
  in
  let kappa_snapshot_display =
      Html.div
          ~a:[ Tyxml_js.R.Html.a_class
                 (React.S.map
                    (fun display_format ->
                       match display_format with
                         | Kappa -> ["visible" ; "kappa-code" ]
                         | Graph -> ["hidden"])
                    display_format
                 )
             ]
      [ Tyxml_js.R.Html.pcdata
          (React.S.map
             (fun snapshot ->
                match snapshot with
                | None -> ""
                | Some snapshot ->
                  Api_data_v1.api_snapshot_kappa
                    (Api_data_v1.api_snapshot snapshot))
             current_snapshot)

      ]
  in
  let kappa_graph_display =
       Html.div
          ~a:[ Tyxml_js.R.Html.a_class
                 (React.S.map
                    (fun display_format ->
                       match display_format with
                         | Graph -> ["visible" ; "kappa-code" ]
                         | Kappa -> ["hidden"])
                    display_format
                 ) ;
               Html.a_id display_id
             ]
      [ Html.entity "nbsp" ]
  in
  let format_chooser =
    [%html
      {| <select class="form-control" id="|}
        format_select_id
        {|"><option value="Kappa">kappa</option><option value="Graph">graph</option></select> |} ]
  in
  [%html {|<div class="navcontent-view">
             <div class="row" style="margin : 5px;">
                <div class="col-sm-2">
                |}[ format_chooser ]{|
               </div>
               <div class="col-sm-10"> |}[ snapshot_chooser ]{| </div>
             </div>
             <div class="row">
                <div class="col-sm-12">
                |}[ kappa_snapshot_display ]{|
    	        </div>
	     </div>
             <div class="row">
                <div class="col-sm-12">
                |}[ kappa_graph_display ]{|
    	        </div>
	     </div>
          </div>
          <div class="navcontent-controls">
          |}[export_controls]{|
          </div>
  |}]

let navcontent (t : Ui_simulation.t) =
  [Ui_common.toggle_element
     t
     (fun state -> snapshot_count state > 0)
     (content t) ]

let render_snapshot_graph
    (snapshot_js : Js_contact.contact_map Js.t)
    (snapshot : Api_types_j.snapshot) : unit =
  let snapshot : Api_types_v1_j.snapshot =
    Api_data_v1.api_snapshot snapshot in
  let () =
    Common.debug
      (Js.string
         (Api_types_v1_j.string_of_snapshot snapshot))
  in
  let site_graph : Api_types_v1_j.site_graph =
    Api_data_v1.api_snapshot_site_graph snapshot in
  match React.S.value display_format with
  | Graph ->
    let json : string = Api_types_v1_j.string_of_site_graph site_graph
    in
    snapshot_js##setData
      (Js.string json)
      (Js.Opt.option (Ui_state.agent_count ()))
  | Kappa -> ()

let select_snapshot (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let snapshot_js : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id true in
  let index = Js.Opt.bind
      (Ui_common.document##getElementById (Js.string select_id))
      (fun dom ->
	 let snapshot_select_dom : Dom_html.inputElement Js.t =
           Js.Unsafe.coerce dom in
	 let fileindex = Js.to_string (snapshot_select_dom##.value) in
	 try Js.some (int_of_string fileindex) with
           _ -> Js.null
      )
  in
  let () = Common.debug index in
    match (React.S.value simulation_output) with
  | None -> ()
  | Some state ->
    let index = Js.Opt.get index (fun _ -> 0) in
    if snapshot_count (Some state) > 0 then
      let () =
        Ui_simulation.manager_operation
          t
          (fun
            manager
            project_id
            simulation_id ->
            (manager#simulation_info_snapshot
               project_id
               simulation_id
            ) >>=
            (Api_common.result_bind_lwt
               ~ok:(fun  (snapshot_info : Api_types_t.snapshot_info) ->
                   try
                     let snapshot_id : string =
                       List.nth snapshot_info.Api_types_t.snapshot_ids index in
                     (manager#simulation_detail_snapshot
                        project_id
                        simulation_id
                        snapshot_id
                     )
             with
             | Failure f ->
               Lwt.return
                 (Api_common.result_error_msg f)
             | Invalid_argument f ->
               Lwt.return
                 (Api_common.result_error_msg f)

                 )
            ) >>=
            (Api_common.result_map
               ~ok:(fun _ (snapshot : Api_types_j.snapshot) ->
                   let () = set_current_snapshot (Some snapshot) in
                   let () = render_snapshot_graph
                     snapshot_js
                     snapshot
                   in
                   Lwt.return_unit)
               ~error:(fun _ errors  ->
                   let () = set_current_snapshot None in
                   let () = Ui_state.set_model_error __LOC__ errors in
                   Lwt.return_unit)
            )
          )
      in
      ()



let onload (t : Ui_simulation.t) : unit =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let snapshot_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Ui_common.document##getElementById
             (Js.string select_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let format_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Ui_common.document##getElementById
             (Js.string format_select_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let () =
    snapshot_select_dom
    ##.
      onchange := Dom_html.handler
	(fun _ ->
         let () = Common.debug ("onchange") in
         let () = select_snapshot t in Js._true)
  in
  let update_format () =
    let format_text : string = (Js.to_string format_select_dom##.value) in
    match string_to_display_format format_text with
    | Some format ->
      let snapshot_js : Js_contact.contact_map Js.t =
         Js_contact.create_contact_map display_id true in
      let () =
        (match React.S.value current_snapshot with
         | None -> ()
         | Some snapshot -> render_snapshot_graph
                              (snapshot_js : Js_contact.contact_map Js.t)
                              (snapshot : Api_types_j.snapshot))
      in
        set_display_format format
    | None -> assert false
  in
  (* get initial value for display format *)
  let () = update_format () in
  (* update value for display format *)
  let () =
    format_select_dom
    ##.
      onchange := Dom_html.handler
	(fun _ ->
         let () = update_format () in
         Js._true)
  in
  let () =
    Common.jquery_on
      "#navsnapshot"
      "shown.bs.tab"
      (fun _ ->
         match (React.S.value simulation_output) with
           None -> ()
         | Some _state -> select_snapshot t)
  in
  let () = Widget_export.onload (configuration t) in
  ()

let onresize (_ : Ui_simulation.t) : unit = ()
