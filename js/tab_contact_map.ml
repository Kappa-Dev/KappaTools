(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

open Lwt.Infix

let display_id = "contact-bis-map-display"
let export_id = "contact-bis-export"

let navli () = []

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false

let accuracy, set_accuracy = React.S.create (Some Public_data.Low)

let contact_map_text,set_contact_map_text = React.S.create None

let configuration : Widget_export.configuration = {
  Widget_export.id = export_id ;
  Widget_export.handlers =
    [ Widget_export.export_svg ~svg_div_id:display_id ();
      Widget_export.export_png ~svg_div_id:display_id ();
      Widget_export.export_json
        ~serialize_json:(fun () ->
            Option_util.unsome "null" (React.S.value contact_map_text)
          )
    ];
  Widget_export.show = React.S.map
      ((<>) None)
      (React.S.on tab_is_active None contact_map_text)
}

let accuracy_chooser =
  let option_gen x =
    Html.option
      ~a:
        ((fun l -> if React.S.value accuracy = Some x
           then Html.a_selected () :: l else l)
           [ Html.a_value (Public_data.accuracy_to_string x) ])
      (Html.pcdata
         (Public_data.accuracy_to_string x)) in
  Html.select
    ~a:[Html.a_class [ "form-control" ]]
    (List.map option_gen Public_data.accuracy_levels)

let content () =
  let export_controls =
    Widget_export.content configuration in
  [[%html {|<div>
|} [accuracy_chooser] {|
                <div id="|}display_id{|">
                 |}[ Html.entity "nbsp" ]{|
	        </div>
	   |}[ export_controls ]{|
        </div>|}]]

let extract_contact_map = function
  | `Assoc [ "contact map", `Assoc [ "accuracy", acc; "map", contact ] ] -> acc,contact
  | `Assoc [ "contact map", `Assoc [ "map", contact; "accuracy", acc ] ] -> acc,contact
  | _ -> failwith "Wrong ugly contact_map extractor"

let _ = React.S.l1
    (fun _ ->
       React.S.l1
         (fun acc ->
            State_project.with_project
              ~label:__LOC__
              (fun (manager : Api.concrete_manager) ->
                 (Lwt_result.map
                    (fun contact_json ->
                       let _,map_json = extract_contact_map contact_json in
                       set_contact_map_text (Some (Yojson.Basic.to_string map_json)))
                    (manager#get_contact_map acc)) >>=
                 fun out -> Lwt.return (Api_common.result_lift out)
              )
         )
         accuracy
    )
    (React.S.on tab_is_active
       State_project.dummy_model State_project.model)

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Widget_export.onload configuration in
  let contactmap : Js_contact.contact_map Js.t =
    Js_contact.create_contact_map display_id in
  let _ =
    React.S.map
      (function
        | None -> (contactmap##clearData)
        | Some data ->
          contactmap##setData (Js.string data))
      (React.S.on
         tab_is_active None contact_map_text)
  in
  let () = (Tyxml_js.To_dom.of_select accuracy_chooser)##.onchange :=
      Dom_html.full_handler
        (fun va _ ->
           let va = Js.to_string va##.value in
           let () = set_accuracy (Public_data.accuracy_of_string va) in
           Js._true) in
  let () = Common.jquery_on
      "#navcontact_map" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navcontact_map" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
