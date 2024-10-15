(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let option_label ?(max_size = 24) label =
  if String.length label > max_size then
    String.sub label 0 (max_size - 1) ^ "..."
  else
    label

let export_controls ~(export_select_id : string) ~(export_filename_id : string)
    ~(export_button_id : string) ~(export_data_label : string) =
  let export_formats : string list = [ export_data_label ] in
  let export_filename =
    Html.input
      ~a:
        [
          Html.a_id export_filename_id;
          Html.a_input_type `Text;
          Html.a_class [ "form-control" ];
          Html.a_placeholder "file name";
        ]
      ()
  in
  let export_button =
    Html.button
      ~a:
        [
          Html.a_id export_button_id;
          Html.Unsafe.string_attrib "role" "button";
          Html.a_class [ "btn"; "btn-default"; "pull-right" ];
        ]
      [ Html.cdata "export" ]
  in
  let export_formats_select =
    List.map
      (fun format ->
        [%html
          {|<option value="|} format {|">|} (Html.cdata format) {|</option>|}])
      export_formats
  in
  [%html
    {|<div class="row">
           <div class="col-sm-12">
           <div class="form-inline">
           <div class="form-group">
           <select class="form-control"
                   id="|}
      export_select_id
      {|"><option value="png">png</option><option value="svg">svg</option>|}
      export_formats_select
      {|</select>
                                                                                                                                      </div>
                                                                                                                                      <div class="form-group">
                                                                                                                                      <label class="checkbox-inline">
                                                                                                                                    |}
      [ export_filename ]
      {|
           </label>
        </div>
        <div class="form-group">
           <label class="checkbox-inline">
              |}
      [ export_button ]
      {|
           </label>
        </div>
     </div>
  </div>
</div>|}]

let document = Dom_html.window##.document

let arguments (key : string) : string list =
  List.map snd (List.filter (fun (k, _) -> key = k) Url.Current.arguments)

let version ?(test : 'a option = None) ~(prod : 'a) ~(dev : 'a) : 'a =
  let version : string list = arguments "version" in
  match test, version with
  | Some test, [ "test" ] -> test
  | _, [ "dev" ] -> dev
  | _ -> prod

let navli label force_class decorations =
  let default_attributes =
    [ Html.a_id ("nav" ^ label); Html.a_role [ "presentation" ] ]
  in
  let attributes =
    match force_class with
    | None -> default_attributes
    | Some l -> Tyxml_js.R.Html5.a_class l :: default_attributes
  in
  let text =
    ReactiveData.RList.concat
      (ReactiveData.RList.singleton (Html.cdata label))
      decorations
  in
  Html.li ~a:attributes
    [
      Tyxml_js.R.Html.a
        ~a:
          [
            Html.Unsafe.string_attrib "data-toggle" "tab";
            Html.a_role [ "tab" ];
            Html.Unsafe.string_attrib "aria-controls" label;
            Html.a_href ("#" ^ label);
          ]
        text;
    ]

let navtabs navtabs_id = function
  | [] | (_, Some _, _) :: _ -> Common.toss "ui_common.navtabs : missing tabs"
  | (ti, None, l) :: t ->
    Html.ul
      ~a:
        [
          Html.a_id navtabs_id;
          Html.a_class [ "nav"; "nav-tabs" ];
          Html.Unsafe.string_attrib "role" "tablist";
        ]
      (navli ti (Some (React.S.const [ "active" ])) l
      :: List.map (fun (t, a_class, li) -> navli t a_class li) t)

let onenavcontent label active classes content =
  Html.div
    ~a:
      [
        Html.a_id label;
        Html.a_class
          (if active then
             "flex-content" :: "tab-pane" :: "active" :: classes
           else
             "flex-content" :: "tab-pane" :: classes);
        Html.Unsafe.string_attrib "role" "tabpanel";
      ]
    content

let navcontent ?id classes = function
  | [] -> Common.toss "ui_common.navcontent : missing content"
  | (t, cl, c) :: l ->
    let id : [> `Id ] Html.attrib list =
      match id with
      | None -> []
      | Some id -> [ Html.a_id id ]
    in
    Html.div
      ~a:
        ([
           Html.a_class
             ([ "panel-content"; "tab-content"; "flex-content" ] @ classes);
         ]
        @ id)
      (onenavcontent t true cl c
      :: List.map (fun (t, cl, c) -> onenavcontent t false cl c) l)

let level ?debug ?info ?log ?warning ?error () : 'a list =
  let level : string list = arguments "level" in
  let extract key value =
    match value with
    | None -> []
    | Some value ->
      if List.mem key level then
        [ value ]
      else
        []
  in
  extract "debug" debug @ extract "info" info @ extract "log" log
  @ extract "warning" warning @ extract "error" error @ []

let features ?(default = []) (options : (string * 'a) list) : 'a list =
  let features : string list = arguments "feature" in
  let matches : 'a list =
    List.map snd
      (List.filter (fun (feature, _) -> List.mem feature features) options)
  in
  match matches with
  | [] -> default
  | _ :: _ -> matches

let input_change input_dom signal_handler =
  input_dom##.onchange :=
    Dom_html.handler (fun _ ->
        let () = signal_handler (Js.to_string input_dom##.value) in
        Js._true)

module type Menu = sig
  val content :
    unit ->
    [> `Button | `Div | `Ul | `A of [> `PCDATA | `Span ] ] Tyxml_js.Html5.elt
    list

  val onload : unit -> unit
end

module type Div = sig
  val id : string
  val content : unit -> Html_types.div_content_fun Tyxml_js.Html.elt list
  val onload : unit -> unit
end

module type Tab = sig
  val navli :
    unit ->
    Html_types.flow5_without_interactive Tyxml_js.Html5.elt ReactiveData.RList.t

  val content : unit -> Html_types.div_content_fun Tyxml_js.Html5.elt list
  val onload : unit -> unit
  val onresize : unit -> unit
end

module type SubTab = sig
  include Tab

  val parent_hide : unit -> unit
  val parent_shown : unit -> unit
end

module type Panel = sig
  val content : unit -> Html_types.div Tyxml_js.Html5.elt
  val onload : unit -> unit
  val onresize : unit -> unit
end

let id_dom (id : string) : 'a Js.t =
  Js.Unsafe.coerce
    (Js.Opt.get
       (document##getElementById (Js.string id))
       (fun () ->
         Common.toss
           (Format.sprintf "ui_common.id_dom : could not find id %s" id))
      : Dom_html.element Js.t)

let switch_class elt_id add_list remove_list =
  let dom_elt : 'a Js.t = id_dom elt_id |> Js.Unsafe.coerce in
  List.iter
    (fun (class_str : string) ->
      Js.Unsafe.meth_call dom_elt##.classList "add"
        [| Js.string class_str |> Js.Unsafe.coerce |])
    add_list;
  List.iter
    (fun (class_str : string) ->
      Js.Unsafe.meth_call dom_elt##.classList "remove"
        [| Js.string class_str |> Js.Unsafe.coerce |])
    remove_list

(* modals *)

let create_modal_text_input ~(id : string) ~(title_label : string)
    ~(body : [< Html_types.div_content_fun ] Html.elt Html.list_wrap)
    ~(submit_label : string) ~(submit : ('self Js.t, _ Js.t) Dom.event_listener)
    : [> Html_types.div ] Html.elt =
  let button =
    Html.button
      ~a:[ Html.a_button_type `Submit; Html.a_class [ "btn"; "btn-primary" ] ]
      [ Html.txt submit_label ]
  in
  let form =
    Html.form
      ~a:[ Html.a_class [ "modal-content" ] ]
      [
        Html.div
          ~a:[ Html.a_class [ "modal-header" ] ]
          [
            Html.button
              ~a:
                [
                  Html.Unsafe.string_attrib "type" "button";
                  Html.a_class [ "close" ];
                  Html.Unsafe.string_attrib "data-dismiss" "modal";
                  Html.Unsafe.string_attrib "aria-label" "Close";
                ]
              [
                Html.span
                  ~a:[ Html.Unsafe.string_attrib "aria-hidden" "true" ]
                  [ Html.entity "times" ];
              ];
            Html.h4 [ Html.cdata title_label ];
          ];
        Html.div ~a:[ Html.a_class [ "modal-body" ] ] body;
        Html.div
          ~a:[ Html.a_class [ "modal-footer" ] ]
          ([
             Html.button
               ~a:
                 [
                   Html.Unsafe.string_attrib "type" "button";
                   Html.a_class [ "btn"; "btn-default" ];
                   Html.Unsafe.string_attrib "data-dismiss" "modal";
                 ]
               [ Html.cdata "Cancel" ];
           ]
          @ [ button ]);
      ]
  in
  let () = (Tyxml_js.To_dom.of_form form)##.onsubmit := submit in
  Html.div
    ~a:
      [
        Html.a_class [ "modal"; "fade" ];
        Html.a_id id;
        Html.Unsafe.string_attrib "tabindex" "-1";
        Html.Unsafe.string_attrib "role" "dialog";
      ]
    [
      Html.div
        ~a:
          [
            Html.a_class [ "modal-dialog" ];
            Html.Unsafe.string_attrib "role" "document";
          ]
        [ form ];
    ]

(* Duplicated in js in common.js. TODO: merge the logic? *)
let create_modal_error ~(id : string) ~(is_critical : bool)
    ~(error_content : string) : [> Html_types.div ] Html.elt =
  let button_type : string =
    if is_critical then
      "btn-danger"
    else
      "btn-primary"
  in
  let button =
    Html.button
      ~a:
        [
          Html.a_button_type `Button;
          Html.a_class [ "btn"; button_type ];
          Html.Unsafe.string_attrib "data-dismiss" "modal";
        ]
      [ Html.txt "Return to app" ]
  in

  let title_label =
    if is_critical then
      "Critical error."
    else
      "Error."
  in

  let body =
    [
      Html.txt
        ("The Kappa app has encountered a "
        ^ (if is_critical then
             "critical "
           else
             "")
        ^ "error:");
      Html.pre [ Html.code [ Html.txt error_content ] ];
    ]
    @
    if is_critical then
      [
        Html.txt
          "Some parts of the app might not behave properly after this. In this \
           case, save your work and reload the app.";
      ]
    else
      []
  in

  let backdrop_attrib =
    if is_critical then
      Html.Unsafe.string_attrib "data-backdrop" "static"
    else
      Html.Unsafe.string_attrib "data-backdrop" "true"
  in

  let form =
    Html.form
      ~a:[ Html.a_class [ "modal-content" ] ]
      [
        Html.div
          ~a:[ Html.a_class [ "modal-header" ] ]
          [
            Html.button
              ~a:
                [
                  Html.Unsafe.string_attrib "type" "button";
                  Html.a_class [ "close" ];
                  Html.Unsafe.string_attrib "data-dismiss" "modal";
                  Html.Unsafe.string_attrib "aria-label" "Close";
                ]
              [
                Html.span
                  ~a:[ Html.Unsafe.string_attrib "aria-hidden" "true" ]
                  [ Html.entity "times" ];
              ];
            Html.h4 [ Html.cdata title_label ];
          ];
        Html.div ~a:[ Html.a_class [ "modal-body" ] ] body;
        Html.div ~a:[ Html.a_class [ "modal-footer" ] ] [ button ];
      ]
  in
  Html.div
    ~a:
      [
        Html.a_class [ "modal"; "fade" ];
        Html.a_id id;
        Html.Unsafe.string_attrib "tabindex" "-1";
        Html.Unsafe.string_attrib "role" "dialog";
        backdrop_attrib;
      ]
    [
      Html.div
        ~a:
          [
            Html.a_class [ "modal-dialog" ];
            Html.Unsafe.string_attrib "role" "document";
          ]
        [ form ];
    ]

let () = Random.self_init ()
let index_modal = ref 0

let new_modal_error_id () =
  incr index_modal;
  "modal_error_id-" ^ string_of_int !index_modal

let open_modal_error ~(is_critical : bool) ~(error_content : string) : unit =
  let id = new_modal_error_id () in

  let modal =
    Tyxml_js.To_dom.of_div (create_modal_error ~id ~is_critical ~error_content)
  in
  let main = id_dom "main" in
  Dom.appendChild main modal;
  Common.modal ~id:("#" ^ id) ~action:"show"
