module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5
module UIState = Ui_state

let current_snapshot, set_current_snapshot =
  React.S.create (None : ApiTypes.snapshot option)

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
  let simulation_null = (Ui_simulation.simulation_null t) in
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
        simulation_null

  }


let content (t : Ui_simulation.t) =
  let simulation_null = (Ui_simulation.simulation_null t) in
  let select =
    Tyxml_js.R.Html.select
      ~a:[ Html.a_class ["form-control"]
         ; Html.a_id select_id ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun state ->
              ReactiveData.RList.set
		handle
		(List.mapi
                   (fun i snapshot ->
                      Html.option
			~a:([ Html.a_value (string_of_int i)]
			    @
			    if (match (React.S.value current_snapshot) with
				| None ->
				  false
				| Some s ->
				  s.ApiTypes.snap_file = snapshot.ApiTypes.snap_file
                              )
			    then [Html.a_selected ()]
			    else [])
			(Html.pcdata
			   (Ui_common.option_label snapshot.ApiTypes.snap_file)))
                   (state_snapshot state)
		)
           )
           simulation_null in
       list
      )
  in
  let snapshot_select =
    Ui_common.toggle_element
      t
      state_snapshot
      [
	Tyxml_js.R.Html.div
	  ~a:[ Html.a_class ["list-group-item"] ]
	  (let list, handle = ReactiveData.RList.create [] in
	   let _ = React.S.map
               (fun state ->
		  ReactiveData.RList.set
		    handle
		    (match state_snapshot state with
		       head::[] ->
                       [Html.h4
			  [ Html.pcdata
                              (Ui_common.option_label head.ApiTypes.snap_file)]]
		     | _ -> [select]
		    )
               )
                    simulation_null
	   in
	   list
	  )
      ]
  in
  let export_controls =
    Widget_export.content (configuration t)
  in
  [%html {|<div>
             <div class="row">
               <div class="center-block display-header">
         |}[ snapshot_select ]{|
               </div>
             </div>
             <div class="row">
                <div class="col-sm-12" id="|}display_id{|">
							 </div>
							 </div>
							 |}[export_controls]{|
        </div>|}]


let navcontent (t : Ui_simulation.t) = [content t]

let update_snapshot
    (snapshot_js : Js_contact.contact_map Js.t)
    (snapshot : ApiTypes.snapshot) : unit =
  let () =
    Common.debug
      (Js.string
         (ApiTypes_j.string_of_snapshot snapshot))
  in
  let site_graph : ApiTypes.site_graph =
    Api_data.api_snapshot_site_graph snapshot in
  let () =
    Common.debug
      (Js.string
         (ApiTypes_j.string_of_site_graph site_graph))
  in
  let json : string =
    ApiTypes_j.string_of_site_graph site_graph
  in
  snapshot_js##setData
    (Js.string json)
    (Js.Opt.option (Ui_state.agent_count ()))

let select_snapshot (t : Ui_simulation.t) =
  let simulation_null = (Ui_simulation.simulation_null t) in
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
  match (React.S.value simulation_null) with
    None -> ()
  | Some state ->
    let index = Js.Opt.get index (fun _ -> 0) in
    if List.length state.ApiTypes.snapshots > 0 then
      let snapshot_selected : ApiTypes.snapshot =
        List.nth state.ApiTypes.snapshots index in
      let () = set_current_snapshot (Some snapshot_selected) in
      update_snapshot
        snapshot_js
        snapshot_selected
    else
      set_current_snapshot None

let onload (t : Ui_simulation.t) : unit =
  let simulation_null = (Ui_simulation.simulation_null t) in
  let snapshot_select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Ui_common.document##getElementById
             (Js.string select_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let () =
    snapshot_select_dom
    ##.
      onchange := Dom_html.handler
	(fun _ -> let () = select_snapshot t in Js._true)
  in
  let () =
    Common.jquery_on
      "#navsnapshot"
      "shown.bs.tab"
      (fun _ ->
         match (React.S.value simulation_null) with
           None -> ()
         | Some _state -> select_snapshot t)
  in
  let () = Widget_export.onload (configuration t) in
  let _ : unit React.signal = React.S.l1
      (fun state -> match state with
	   None -> ()
	 | Some _state -> select_snapshot t
      )
      simulation_null
  in
  ()
