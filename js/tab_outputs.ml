module Html = Tyxml_js.Html5
open Lwt.Infix


let select_id = "output-select-id"
let export_id = "output-export"

let current_file, set_current_file =
  React.S.create (None : Api_types_j.file_line_detail option)

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
  (t : Ui_simulation.t)
  (index : int) : unit =
  Ui_simulation.manager_operation
    t
    (fun
      manager
      project_id
      simulation_id ->
      (manager#simulation_info_file_line
         project_id
         simulation_id
      ) >>=
      (Api_common.result_bind_lwt
         ~ok:(fun (file_line_info : Api_types_j.file_line_info) ->
             try
               let file_line_info_id : Api_types_j.file_line_id =
                 List.nth file_line_info.Api_types_t.file_line_ids index in
               (manager#simulation_detail_file_line
                  project_id
                  simulation_id
                  file_line_info_id
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
         ~ok:(fun _ (file_line_detail : Api_types_j.file_line_detail) ->
             let () = set_current_file (Some file_line_detail) in
             Lwt.return_unit
           )
         ~error:(fun _ errors  ->
            let () = Ui_state.set_model_error __LOC__ errors in
            Lwt.return_unit)
      )
    )

let file_count
    (state : Api_types_j.simulation_info option) :
  int =
  match state with
    None -> 0
  | Some state ->
    state.Api_types_t.simulation_info_output.Api_types_t.simulation_output_file_lines
(* Careful this defaults to None which a valid file identifier.
   The idea is to always give a valid file identifier.
 *)
let get_file_line_id
    (file : Api_types_j.file_line_detail) : Api_types_j.file_line_id =
  match file with
  | h::_ -> h.Api_types_j.file_line_name
  | [] -> None

let navli (t : Ui_simulation.t) =
  Ui_common.badge t
    (fun state -> (file_count state))

let content (t : Ui_simulation.t) =
  let simulation_output = (Ui_simulation.simulation_output t) in
  let select =
    Tyxml_js.R.Html.select
      ~a:[ Html.a_class ["form-control"]
         ; Html.a_id select_id ]
      (let list, handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun _ ->
              Ui_simulation.manager_operation
                t
                (fun
                  manager
                  project_id
                  simulation_id ->
                  (manager#simulation_info_file_line
                     project_id
                     simulation_id
                  ) >>=
                  (Api_common.result_map
                     ~ok:(fun _ (file_line_info : Api_types_j.file_line_info) ->
                         let file_ids : Api_types_j.file_line_id list =
                           file_line_info.Api_types_j.file_line_ids in
                         let file : Api_types_j.file_line_detail option = React.S.value current_file in
		         let current_file_id : Api_types_j.file_line_id =
                           (match (file_ids,file) with
	                   | (f::_,None) -> f
		           | (_::_,Some file) -> get_file_line_id file
                           | _ -> None) in
                        let file_options =
                          List.mapi
                            (fun i key ->
                               Html.option
			         ~a:([ Html.a_value (string_of_int i)]@
                                     (if (key = current_file_id) then
                                         [Html.a_selected ()]
			              else  []))
 	                         (Html.pcdata
			            (Ui_common.option_label
                                        (match key with
                                         | None -> ""
                                         | Some name -> name)
			                )
			         )
                            )
                            file_ids
                         in
                         let () = ReactiveData.RList.set handle file_options in
                         Lwt.return_unit
                       )
                     ~error:(fun _ errors  ->
                         let () = Ui_state.set_model_error __LOC__ errors in
                         Lwt.return_unit)
                  )
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
           (fun _ ->
              Ui_simulation.manager_operation
                t
                (fun
                  manager
                  project_id
                  simulation_id ->
                  (manager#simulation_info_file_line
                     project_id
                     simulation_id
                  ) >>=
                  (Api_common.result_map
                     ~ok:(fun _ (file_line_info : Api_types_j.file_line_info) ->
                         let () = ReactiveData.RList.set
                             handle
                             (match file_line_info.Api_types_j.file_line_ids with
		                key::[] ->
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
                         in
                         Lwt.return_unit
                       )
                     ~error:(fun _ errors  ->
                         let () = Ui_state.set_model_error __LOC__ errors in
                         Lwt.return_unit)
                  )
                )
           )
           simulation_output in
       list
      )
  in
  let file_content =
    Tyxml_js.R.Html.div
      (let line_list, line_handle = ReactiveData.RList.create [] in
       let _ = React.S.map
           (fun (file : Api_types_j.file_line_detail option) ->
              match file with
              | None -> ()
              | Some lines->
		ReactiveData.RList.set
		  line_handle
                  (List.map
                     (fun line -> Html.p [ Html.pcdata line.Api_types_j.file_line_text ]) lines)
                  )
           current_file
       in
       line_list)
  in
  [ [%html {|<div class="navcontent-view">
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
             </div> |}] ]

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
  let index = Js.Opt.get index (fun _ -> 0) in
  if file_count (React.S.value simulation_output) > 0 then
    update_outputs t index
   else
     ()

let navcontent (t : Ui_simulation.t) =
  [ Ui_common.toggle_element t (fun t -> file_count t > 0) (content t) ]

let onload (t : Ui_simulation.t) =
  let () =
    Common.jquery_on
      (Format.sprintf "#%s" select_id)
      ("change")
      (fun _ ->
	 let () = select_outputs t in Js._true)
  in  ()
  (* TODO
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
     *)

let onresize (_ : Ui_simulation.t) : unit = ()
