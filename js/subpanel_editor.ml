(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Codemirror
module Html = Tyxml_js.Html5

let editor_full , set_editor_full = React.S.create (false : bool)
let is_paused , set_is_paused = React.S.create (false : bool)
let filename , set_filename = React.S.create (None : string option)

let file_label =
  Tyxml_js.R.Html.txt
    (React.S.bind
       State_file.model
       (fun model ->
          React.S.const
            (match model.State_file.model_current with
             | None -> ""
             | Some filename -> filename)
       ))

let toggle_button_id = "toggle_button"
let toggle_button =
  Html.a
    ~a:[ Html.a_id toggle_button_id
       ; Html.Unsafe.string_attrib "role" "button"
       ; Html.a_class ["btn";"btn-default";"pull-right"]
       ]
    [ Html.cdata "toggle" ]

let panel_heading_group_id = "panel_heading_group"
let panel_heading =
  let menu_editor_file_content :
    [> Html_types.div ] Tyxml_js.Html5.elt =
  Html.div
    ~a:[ Html.a_class [ "btn-group" ] ;
         Html.Unsafe.string_attrib "role" "group" ; ]
    (Menu_editor_file.content ())
  in
  let buttons =
    menu_editor_file_content ::
       [toggle_button] in
  [%html {|<div class="row">
             <div id="|}panel_heading_group_id{|" class="col-md-10 btn-group">|} buttons{|</div>
             <label class="col-md-2 oneliner filename">|}[ file_label ]{|</label>
            </div>|}]

let codemirror_id = "code-mirror"
let editor_panel_id = "editor-panel"

let content () =
  let textarea =
    Html.textarea
      ~a:[Html.a_id codemirror_id]
      (Html.txt "")
  in
  Html.div
    ~a:[Html.a_class ["flex-content";"panel";"panel-default"]]
    [ Html.div
        ~a:[Html.a_class ["panel-heading"]]
        [ panel_heading ] ;
      Html.div
        ~a:[ Tyxml_js.R.Html.a_class
               (React.S.map
                  (fun model ->
                     match model.State_file.model_current with
                     | None -> ["no-panel-body" ; "flex-content" ]
                     | Some _ -> ["panel-body" ; "flex-content" ])
                  State_file.model) ;
             Html.a_id editor_panel_id
           ]
         [ textarea  ] ;
    ]

let error_lint errors : Codemirror.lint Js.t Js.js_array Js.t =
  let position p =
    new%js Codemirror.position (p.Locality.line-1) (p.Locality.chr) in
  let hydrate (error  : Api_types_j.message) : lint Js.t option =
    match error.Api_types_j.message_range with
    | None -> None
    | Some range ->
      let model : State_file.model = React.S.value State_file.model in
      match model.State_file.model_current with
      | None -> None
      | Some file_id ->
        if range.Locality.file = file_id then
          Some (Codemirror.create_lint
                  ~message:error.Api_types_j.message_text
              (* This is a bit of a hack ... i am trying to keep
                   the code mirror code independent of the api code.
              *)
              ~severity:( match error.Api_types_j.message_severity with
                      | `Error -> Codemirror.Error
                      | `Warning -> Codemirror.Warning
                      | `Info -> Codemirror.Warning
                    )
              ~from:(position range.Locality.from_position)
              ~to_:(position range.Locality.to_position))
        else
          None
  in
  Js.array
    (Array.of_list
       (List.fold_left
          (fun acc value ->
             match hydrate value with
             | None -> acc
             | Some value -> value::acc)
          []
          errors))


let setup_lint _ _ _ =
  error_lint (React.S.value State_error.errors)

(* http://stackoverflow.com/questions/10575343/codemirror-is-it-possible-to-scroll-to-a-line-so-that-it-is-in-the-middle-of-w *)
let jump_to_line (codemirror : codemirror Js.t) (line : int) : unit =
  let position : position Js.t = new%js Codemirror.position line 0 in
  let mode : Js.js_string Js.t Js.opt = Js.some (Js.string "local") in
  let coords : Codemirror.dimension Js.t  = codemirror##charCoords position mode in
  let top : int = coords##.top in
  let element : Dom_html.element Js.t = codemirror##getScrollerElement in
  let middleHeight : int = element##.offsetHeight/2 in
  let scrollLine : int = top - middleHeight - 5 in
  let () = codemirror##scrollTo Js.null (Js.some scrollLine) in
  ()

let onload () : unit =
  let () = Menu_editor_file.onload () in
  let lint_config =
    Codemirror.create_lint_configuration () in
  let () = lint_config##.getAnnotations := setup_lint in
  let () = lint_config##.lintOnChange := Js._false in
  let configuration = Codemirror.default_configuration in
  let gutter_options =
    Js.string "breakpoints,CodeMirror-lint-markers,CodeMirror-linenumbers" in
  let gutter_option : Js.string_array Js.t =
    gutter_options##split(Js.string ",")
  in
  let textarea : Dom_html.element Js.t = Ui_common.id_dom "code-mirror" in
  let () =
    configuration##.lineNumbers := Js._true;
    configuration##.lineWrapping := Js._true;
    configuration##.styleActiveLine := Js._true;
    configuration##.autofocus := Js._true;
    configuration##.gutters := gutter_option;
    configuration##.lint := lint_config;
    configuration##.mode := (Js.string "Kappa")
  in
  let codemirror : codemirror Js.t =
    Codemirror.fromTextArea textarea configuration in
  let () = codemirror##setValue(Js.string "") in
  let _ = React.S.map (fun _ -> codemirror##performLint) State_error.errors in
  let _ = Subpanel_editor_controller.with_file
      (Api_common.result_map
         ~ok:(fun _ file ->
             let file_content = file.Api_types_j.file_content in
             let () = set_filename (Some file.Api_types_j.file_metadata.Api_types_j.file_metadata_id) in
             let () = codemirror##setValue(Js.string file_content) in
                 Lwt.return (Api_common.result_ok ()))
         ~error:(fun _ _ ->
             (* ignore if missing file *)
             Lwt.return (Api_common.result_ok ())))
  in
  let () = Codemirror.commands##.save :=
      (fun _ -> Menu_editor_file_controller.export_current_file ()) in
  let timeout : Dom_html.timeout_id option ref = ref None in
  let handler = fun codemirror change ->
    let () = set_is_paused true in
    let text : string = Js.to_string codemirror##getValue in
    let () = match !timeout with
        None -> ()
      | Some timeout ->
        Dom_html.window ## clearTimeout (timeout) in
    let delay : float =
      if (((Js.str_array (change##.text ))##.length) > 1)
         ||
         (State_error.has_errors ())
      then
        1.0 *. 1000.0
      else
        5.0 *. 1000.0
    in
    let handle_timeout () =
      let () = set_is_paused false in
      let () = Common.info (Js.string "handle_timeout") in
      let () = Common.info (Js.string text) in
      match React.S.value filename with
      | None -> ()
      | Some filename ->
        Subpanel_editor_controller.set_content
        ~filename
        ~filecontent:(Js.to_string codemirror##getValue)
    in
    let () = timeout := Some
          (Dom_html.window ## setTimeout
             (Js.wrap_callback
                (fun _ -> handle_timeout ())) delay) in
    ()
  in
  let () = codemirror##onChange(handler) in
  let toggle_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (Ui_common.document##getElementById (Js.string toggle_button_id))
         (fun () -> assert false)) in
  let () =
    toggle_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let editor_full = React.S.value editor_full in
           let () = set_editor_full (not editor_full) in
           Js._true) in
  let _ =
    React.S.map
      (fun model ->
         match model.State_file.model_current with
         | None -> Common.hide_codemirror ()
         | Some _ -> Common.show_codemirror ())
      State_file.model
  in
  let _ =
    React.S.map
      (fun (refresh : State_file.refresh option) ->
         match refresh with
         | None -> set_filename None
         | Some refresh ->
           let () = set_filename (Some refresh.State_file.filename) in
           let () = codemirror##setValue(Js.string refresh.State_file.content) in
           let () = match refresh.State_file.line with
             | None -> ()
             | Some line -> jump_to_line codemirror line in
           ()
      )
      State_file.refresh_file
  in
  ()

let onresize () = ()
