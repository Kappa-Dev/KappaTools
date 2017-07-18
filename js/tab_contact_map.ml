(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

open Lwt.Infix

let navli () = []

let display_id = "contact-map-display"
let export_id = "contact-export"

let tab_is_active, set_tab_is_active = React.S.create true
let tab_was_active = ref true

let configuration : Widget_export.configuration =
  { Widget_export.id = export_id ;
    Widget_export.handlers =
      [ Widget_export.export_svg ~svg_div_id:display_id ()
      ; Widget_export.export_png ~svg_div_id:display_id ()
      ; Widget_export.export_json
          ~serialize_json:(fun () ->
              let model = React.S.value State_project.model in
                let contact_map = model.State_project.model_contact_map in
              (match contact_map with
               | None -> "null"
               | Some parse -> Api_types_j.string_of_contact_map parse
              )
            )
      ];
    show = React.S.map
        (fun model ->
           match model.State_project.model_contact_map with
             | None -> false
             | Some data -> Array.length data > 0
        )
        (React.S.on tab_is_active State_project.dummy_model State_project.model)
  }


let xml contact_json =
  let export_controls =
    Widget_export.content configuration
  in
  [%html {|<div>
                <div id="|}display_id{|">
                 |}[ Html.entity "nbsp" ]{|
	        </div>
	   |}[ export_controls ]{|
        </div>|}]

let content () =
  let contact,set_contact = ReactiveData.RList.create [] in
  let _ = React.S.l1
      (fun _ ->
         State_project.with_project
           ~label:__LOC__
           (fun (manager : Api.concrete_manager) ->
              (Lwt_result.map
                 (fun contact_json ->
                    let () = ReactiveData.RList.set set_contact
                        [ Html.div [xml contact_json]] in
                    ())
                 (manager#get_contact_map None)) >>=
              fun out -> Lwt.return (Api_common.result_lift out)
           )
      )
      (React.S.on tab_is_active
         State_project.dummy_model State_project.model) in
  [ Tyxml_js.R.Html5.div
      ~a:[Html.a_class ["panel-pre" ; "panel-scroll" ; "tab-log" ]]
      contact
  ]

let content () = [ Html.div [xml ()] ]

let update
    (data : Api_types_j.contact_map)
    (contactmap : Js_contact.contact_map Js.t) : unit =
  let () = Common.debug (Js.string "updating") in
  (* quick cheat to get the count of the agents *)
  let json : string =
    Api_types_j.string_of_site_graph data in
  let () = Common.debug (Js.string json) in
    contactmap##setData (Js.string json)

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Widget_export.onload configuration in
  let contactmap : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id false in
  let _ =
    React.S.map
      (fun model->
         match model.State_project.model_contact_map with
         | None -> (contactmap##clearData)
         | Some data ->
           if Array.length data > 0 then
             update data contactmap
           else
             contactmap##clearData)
      (React.S.on
         tab_is_active State_project.dummy_model State_project.model)
  in
    let () = Common.jquery_on
      "#navcontact"
      "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navcontact"
      "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()

let onresize () : unit = ()
