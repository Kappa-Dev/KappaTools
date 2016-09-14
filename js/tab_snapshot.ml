module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5
module UIState = Ui_state

let current_snapshot, set_current_snapshot =
  React.S.create (None : ApiTypes.snapshot option)

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

let state_snapshot state =
  match state with
  | None -> []
  | Some state -> state.ApiTypes.snapshots

let navli (t : Ui_simulation.t) =
  Ui_common.badge
    t
    (fun state -> List.length (state_snapshot state))

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
		 (React.S.value current_snapshot : ApiTypes.snapshot option) with
              | None -> "null"
              | Some s -> ApiTypes.string_of_snapshot s
              )
            )
      ; { Widget_export.suffix = "ka"
        ; Widget_export.label = "kappa"
        ; Widget_export.export =
	    fun (filename : string) ->
              let data = match
		  (React.S.value current_snapshot : ApiTypes.snapshot option) with
              | None -> ""
              | Some s -> Api_data.api_snapshot_kappa s
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
		  (React.S.value current_snapshot : ApiTypes.snapshot option) with
              | None -> ""
              | Some s -> Api_data.api_snapshot_dot s
              in
              Common.saveFile
		~data:data
		~mime:"text/vnd.graphviz"
		~filename:filename
        }
      ];
    show = React.S.map
        (fun state ->
           match state_snapshot state with
           | [] -> false
           | _ -> true
        )
        simulation_output

  }

let format_select_id = "format_select_id"

let content (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let list, handle = ReactiveData.RList.create [] in
  let select state =
    List.mapi
      (fun i snapshot ->
         Html.option
	   ~a:([ Html.a_value (string_of_int i)]
	   @
           if (match (React.S.value current_snapshot) with
	   | None -> false
           | Some s -> s.ApiTypes.snap_file = snapshot.ApiTypes.snap_file)
	   then [Html.a_selected ()]
           else [])
           (Html.pcdata
	      (Ui_common.option_label snapshot.ApiTypes.snap_file)))
              (state_snapshot state)
  in
  let _ = React.S.map
      (fun state -> ReactiveData.RList.set handle (select state))
      simulation_output
  in
  let snapshot_class :
    empty:(unit -> 'a) ->
    single:(ApiTypes.snapshot -> 'a) ->
    multiple:(ApiTypes.snapshot list -> 'a) -> 'a React.signal =
    fun
      ~empty
      ~single
      ~multiple ->
    React.S.map
      (fun state ->
         match state_snapshot state with
         | [] -> empty ()
         | h::[] -> single h
         | s -> multiple s)
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
             (fun state ->
                match state_snapshot state with
                | [] -> ""
                | snapshot::[] -> (Ui_common.option_label snapshot.ApiTypes.snap_file)
                | _ -> "")
             simulation_output)
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
                | Some snapshot -> Api_data.api_snapshot_kappa snapshot)
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
      [%html {| <select class="form-control" id="|} format_select_id {|"><option value="Kappa">kappa</option><option value="Graph">graph</option></select> |} ]
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
     state_snapshot
     (content t) ]

let render_snapshot_graph
    (snapshot_js : Js_contact.contact_map Js.t)
    (snapshot : ApiTypes.snapshot) : unit =
  let () =
    Common.debug
      (Js.string
         (ApiTypes_j.string_of_snapshot snapshot))
  in
  let site_graph : ApiTypes.site_graph =
    Api_data.api_snapshot_site_graph snapshot in
  match React.S.value display_format with
  | Graph ->
    let json : string = ApiTypes_j.string_of_site_graph site_graph
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
    if List.length state.ApiTypes.snapshots > 0 then
      let snapshot_selected : ApiTypes.snapshot =
        List.nth state.ApiTypes.snapshots index in
      let () = set_current_snapshot (Some snapshot_selected) in
      render_snapshot_graph
        snapshot_js
        snapshot_selected
    else
      set_current_snapshot None

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
                              (snapshot : ApiTypes.snapshot))
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
