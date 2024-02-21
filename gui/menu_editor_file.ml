(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let file_new_modal_id = "menu-editor-file-new-modal"
let file_new_input_id = "menu-editor-file-new-input"
let file_dropdown_menu_id = "menu-editor-file-dropdown-menu"
let file_new_li_id = "menu-editor-file-new-li"
let file_open_li_id = "menu-editor-file-open-li"
let file_open_selector_id = "menu-editor-open-selector-id"
let file_close_li_id = "menu-editor-file-close-li"
let file_export_li_id = "menu-editor-file-export-li"
let file_compile_checkbox = "menu-editor-file-compile-checkbox"

(* list filename annotation *)
let element_get_filename (element : Dom_html.element Js.t) :
    Js.js_string Js.t Js.opt =
  Common.element_data (element : Dom_html.element Js.t) "file-id"

let element_set_filename (name : string) =
  Html.Unsafe.string_attrib "data-file-id" name

let file_new_input =
  Html.input
    ~a:
      [
        Html.a_id file_new_input_id;
        Html.a_input_type `Text;
        Html.a_class [ "form-control" ];
        Html.a_placeholder "file name";
        Html.a_size 40;
      ]
    ()

let file_new_input_dom = Tyxml_js.To_dom.of_input file_new_input

let file_checkbox file_id is_checked =
  let checked_attribute =
    if is_checked then
      [ Html.a_checked () ]
    else
      []
  in
  Html.input
    ~a:
      ([
         Html.a_input_type `Checkbox;
         Html.a_class [ file_compile_checkbox ];
         element_set_filename file_id;
       ]
      @ checked_attribute)
    ()

let open_input =
  Html.input
    ~a:
      [
        Html.a_id file_open_selector_id;
        Html.a_class [ "hidden" ];
        Html.Unsafe.string_attrib "type" "file";
        Html.Unsafe.string_attrib "accept" ".ka";
      ]
    ()

let dropdown (model : State_file.model) =
  (* directories *)
  let hide_on_empty l =
    if Mods.IntMap.is_empty model.State_file.directory then
      []
    else
      l
  in
  let file_li =
    let current_file_pos =
      Option_util.map
        (fun { State_file.rank; _ } -> rank)
        model.State_file.current
    in
    List.map
      (fun (rank, { State_file.name; State_file.local }) ->
        let compile = local = None in
        let li_class =
          (if current_file_pos = Some rank then
             [ "active" ]
           else
             [])
          @ [ "ui-state-sortable" ]
        in
        Html.li
          ~a:[ Html.a_class li_class; element_set_filename name ]
          [
            Html.a
              ~a:[ element_set_filename name ]
              [
                Html.div
                  ~a:
                    [
                      Html.a_class [ "checkbox-control-div" ];
                      element_set_filename name;
                    ]
                  [
                    file_checkbox name compile;
                    Html.span
                      ~a:
                        [
                          Html.a_class [ "checkbox-control-label" ];
                          element_set_filename name;
                        ]
                      [ Html.cdata name ];
                  ];
              ];
          ])
      (Mods.IntMap.bindings model.State_file.directory)
  in
  let separator_li =
    hide_on_empty
      [
        Html.li
          ~a:
            [
              Html.Unsafe.string_attrib "role" "separator";
              Html.a_class
                [ "divider"; "ui-sort-disabled"; "ui-sort-bottom-anchor" ];
            ]
          [];
      ]
  in
  let new_li =
    [
      Html.li
        ~a:[ Html.a_class [ "ui-sort-disabled"; "ui-sort-bottom-anchor" ] ]
        [ Html.a ~a:[ Html.a_id file_new_li_id ] [ Html.cdata "New" ] ];
    ]
  in

  let open_li =
    [
      Html.li
        ~a:[ Html.a_class [ "ui-sort-disabled"; "ui-sort-bottom-anchor" ] ]
        [
          Html.a ~a:[ Html.a_id file_open_li_id ] [ Html.cdata "Open" ];
          open_input;
        ];
    ]
  in

  let close_li =
    hide_on_empty
      [
        Html.li
          ~a:[ Html.a_class [ "ui-sort-disabled"; "ui-sort-bottom-anchor" ] ]
          [ Html.a ~a:[ Html.a_id file_close_li_id ] [ Html.cdata "Close" ] ];
      ]
  in
  let export_li =
    hide_on_empty
      [
        Html.li
          ~a:[ Html.a_class [ "ui-sort-disabled"; "ui-sort-bottom-anchor" ] ]
          [ Html.a ~a:[ Html.a_id file_export_li_id ] [ Html.cdata "Export" ] ];
      ]
  in
  [] @ file_li @ separator_li @ new_li @ open_li @ close_li @ export_li

let content =
  let li_list =
    ReactiveData.RList.from_signal
      (React.S.map (fun model -> dropdown model) State_file.model)
  in
  [
    Html.button
      ~a:
        [
          Html.Unsafe.string_attrib "type" "button";
          Html.a_class [ "btn btn-default"; "dropdown-toggle" ];
          Html.Unsafe.string_attrib "data-toggle" "dropdown";
          Html.Unsafe.string_attrib "aria-haspopup" "true";
          Html.Unsafe.string_attrib "aria-expanded" "false";
          Tyxml_js.R.filter_attrib (Html.a_disabled ())
            (React.S.l2
               (fun model file ->
                 match model.State_project.model_current_id with
                 | None -> true
                 | Some _ ->
                   (match file.State_file.current with
                   | None -> false
                   | Some { State_file.out_of_sync; _ } -> out_of_sync))
               State_project.model State_file.model);
        ]
      [ Html.txt "File"; Html.span ~a:[ Html.a_class [ "caret" ] ] [] ];
    Tyxml_js.R.Html.ul
      ~a:[ Html.a_id file_dropdown_menu_id; Html.a_class [ "dropdown-menu" ] ]
      li_list;
    Ui_common.create_modal ~id:file_new_modal_id ~title_label:"New File"
      ~body:
        [ [%html {|<div class="input-group">|} [ file_new_input ] {|</div>|}] ]
      ~submit_label:"Create File"
      ~submit:
        (Dom_html.handler (fun _ ->
             let filename : string = Js.to_string file_new_input_dom##.value in
             let () = Menu_editor_file_controller.create_file filename in
             let () =
               Common.modal ~id:("#" ^ file_new_modal_id) ~action:"hide"
             in
             Js._false));
  ]

let order_files (element : Dom_html.element Js.t) =
  let filenames : string list =
    Common.children_value element "li[data-file-id]" (fun element ->
        let () = Common.debug element in
        Js.Opt.case
          (element_get_filename element)
          (fun () -> failwith "missing filename")
          Js.to_string)
  in
  let () = Menu_editor_file_controller.order_files filenames in
  ()

let file_select_handler _ _ : unit Lwt.t =
  let open_input_dom = Tyxml_js.To_dom.of_input open_input in
  let files = Js.Optdef.get open_input_dom##.files (fun () -> assert false) in
  let file = Js.Opt.get (files##item 0) (fun () -> assert false) in
  let file_id = Js.to_string file##.name in
  let () =
    Menu_editor_file_controller.create_file
      ~text:(Js_of_ocaml_lwt.File.readAsText file)
      file_id
  in
  let () = open_input_dom##.value := Js.string "" in
  Lwt.return_unit

let onload () =
  let open_input_dom = Tyxml_js.To_dom.of_input open_input in
  let () =
    Common.jquery_on ("#" ^ file_new_li_id) "click"
      (Dom_html.handler (fun _ ->
           let () = Common.modal ~id:("#" ^ file_new_modal_id) ~action:"show" in
           Js._false))
  in
  let () =
    Common.jquery_on ("#" ^ file_open_li_id) "click"
      (Dom_html.handler (fun _ ->
           (* click : unit Js.meth; *)
           let () = open_input_dom##click in
           Js._false))
  in
  let () =
    Common.jquery_on ("#" ^ file_close_li_id) "click"
      (Dom_html.handler (fun _ ->
           let () = Menu_editor_file_controller.close_file () in
           Js._false))
  in
  let () =
    Common.jquery_on ("#" ^ file_export_li_id) "click"
      (Dom_html.handler (fun _ ->
           let () = Menu_editor_file_controller.export_current_file () in
           Js._false))
  in
  let () =
    Common.jquery_on "span[data-file-id]" "click"
      (Dom_html.handler (fun (event : Dom_html.event Js.t) ->
           (* let () = Common.debug event in *)
           let target : Dom_html.element Js.t Js.opt = event##.target in
           let file_id : Js.js_string Js.t Js.opt =
             Js.Opt.bind target (fun (element : Dom_html.element Js.t) ->
                 element_get_filename element)
           in
           let () =
             Js.Opt.case file_id
               (fun _ -> ())
               (fun file_id ->
                 Menu_editor_file_controller.set_file (Js.to_string file_id))
           in
           Js._false))
  in
  let () =
    Common.create_sort file_dropdown_menu_id (fun event _ (* ui *) ->
        let target : Dom_html.element Js.t Js.opt = event##.target in
        Js.Opt.case target
          (fun _ -> ())
          (fun (element : Dom_html.element Js.t) ->
            let id : string = Js.to_string element##.id in
            if file_dropdown_menu_id = id then
              order_files element
            else
              Common.debug (Format.sprintf "unexpected id %s" id)))
  in
  let () =
    Common.jquery_on
      (Format.sprintf "input.%s" file_compile_checkbox)
      "change"
      (Dom_html.handler (fun event ->
           let target : Dom_html.element Js.t Js.opt = event##.target in
           let file_id : Js.js_string Js.t Js.opt =
             Js.Opt.bind target (fun (element : Dom_html.element Js.t) ->
                 element_get_filename element)
           in
           let is_checked : bool =
             Js.to_bool
               (Js.Opt.case target
                  (fun _ -> Js._false)
                  (fun (element : Dom_html.element Js.t) ->
                    (Js.Unsafe.coerce element : Dom_html.inputElement Js.t)##.checked))
           in
           let () =
             Js.Opt.case file_id
               (fun _ -> ())
               (fun file_id ->
                 let () = Common.debug file_id in
                 let () =
                   Menu_editor_file_controller.set_file_compile
                     (Js.to_string file_id) is_checked
                 in
                 ())
           in
           Js._false))
  in
  let () =
    Lwt.async (fun () ->
        Js_of_ocaml_lwt.Lwt_js_events.changes
          (Tyxml_js.To_dom.of_input open_input)
          file_select_handler)
  in
  ()
