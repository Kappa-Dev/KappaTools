module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5
module UIState = Ui_state
open ApiTypes

let select_id = "output-select-id"
let display_id = "contact-map-display"
let export_id = "contact-export"

let current_file, set_current_file =
  React.S.create (None : (string option * string list) option)

let upsert
    (update : 'value -> 'value-> 'value)
    (key : 'key)
    (value : 'value)
    (list : ('key * 'value) list) : ('key * 'value) list =
  if List.mem_assoc key list then
    (key,update value (List.assoc key list))::(List.remove_assoc key list)
  else
    (key,value)::list

let update_outputs
    (update : string option * string list) : unit =
  if (Some update) = (React.S.value current_file) then
    ()
  else
    set_current_file (Some update)

let state_outputs state : (string option * string list) list =
  match state with
    None -> []
  | Some state ->
    List.fold_left
      (fun files file_line ->
         (upsert
            (fun current previous -> current@previous)
            file_line.file_name
            [file_line.line]
            files)
      )
      []
      state.ApiTypes.files

let navli (t : Ui_simulation.t) =
  Ui_common.badge t
    (fun state -> List.length (state_outputs state))

let content (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let select =
    Tyxml_js.R.Html.select
      ~a:[ Html.a_class ["form-control"]
         ; Html.a_id select_id ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun state ->
              let files : (string option * string list) list =
		state_outputs state in
              let file : (string option * string list) option =
		React.S.value current_file in
              let () =
		match (files,file) with
		| (f::_,None) -> set_current_file (Some f)
		| (f::_,Some file) ->
		  (match
                     List.filter
                       (fun (name,_) -> name = fst file)
                       files
                   with
		   | [] -> set_current_file (Some f)
		   | m::_ ->
                     if snd file = snd m then ()
                     else set_current_file (Some m)
		  )
		| _ -> ()
              in (* update *)
              ReactiveData.RList.set
		handle
		(List.mapi
                   (fun i (key,_) ->
                      Html.option
			~a:([ Html.a_value (string_of_int i)]
			    @
			    if (match (React.S.value current_file) with
				| None -> false
				| Some (k,_) -> key = k
                              )
			    then [Html.a_selected ()]
			    else [])
			(Html.pcdata
			   (Ui_common.option_label
                              (match key with
                               | None -> ""
                               | Some name -> name)
			   )
			)
                   )
                   files
		)
           )
           simulation_output in
       list
      )
  in
  let file_select =
    Tyxml_js.R.Html.div
      ~a:[ Html.a_class ["list-group-item"] ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun state ->
              ReactiveData.RList.set
		handle
		(match state_outputs state with
		   (key,_)::[] ->
                   [Html.h4
                      [ Html.pcdata
                          (Ui_common.option_label
                             (match key with
                              | None -> ""
                              | Some file_name -> file_name
                             )
                          )]]
		 | _ -> [select]
		)
           )
           simulation_output
       in
       list
      )
  in
  let file_content =
    Tyxml_js.R.Html.div
      (let line_list, line_handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun (file : (string option * string list) option) ->
              match file with
              | None -> ()
              | Some (_,lines) ->
		ReactiveData.RList.set
		  line_handle
		  (List.map (fun line -> Html.p [ Html.pcdata line ]) lines)
           )
           current_file
       in
       line_list)
  in
  [%html {|<div>
	   <div class="row">
	   <div class="center-block display-header">
	   |}[file_select]{|
      </div>
   </div>
   <div class="row">
      <div class="col-sm-12">
      |}[file_content]{|
      </div>
   </div>
</div>|}]

let select_outputs (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let index = Js.Opt.bind
      (Ui_common.document##getElementById (Js.string select_id))
      (fun dom ->
	 let select_dom : Dom_html.inputElement Js.t =
           Js.Unsafe.coerce dom in
	 let fileindex = Js.to_string (select_dom##.value) in
	 try Js.some (int_of_string fileindex) with
           _ -> Js.null
      )
  in
  match state_outputs (React.S.value simulation_output) with
  | [] -> ()
  | l ->
    let index = Js.Opt.get index (fun _ -> 0) in
    if List.length l > index then
      update_outputs
        (List.nth l index)
    else
      ()

let navcontent (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  [ Html.div
      ~a:[Tyxml_js.R.Html.a_class
            (React.S.bind
               simulation_output
               (fun state -> React.S.const
                   (match state_outputs state with
                      [] -> ["hidden"]
                    | _::_ -> ["show"])
               )
            )]
      [content t]
  ]

let onload (t : Ui_simulation.t) =
  let select_dom : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Ui_common.document##getElementById
             (Js.string select_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let () = select_dom##.onchange := Dom_html.handler
	(fun _ ->
	   let () = select_outputs t
	   in Js._true)
  in

  ()
