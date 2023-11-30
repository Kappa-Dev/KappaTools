(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let toggle_element (projection : Api_types_j.simulation_info option -> bool)
    (content : [< Html_types.div_content_fun ] Html.elt Html.list_wrap) =
  Html.div
    ~a:
      [
        Tyxml_js.R.Html.a_class
          (React.S.bind State_simulation.model (fun model ->
               React.S.const
                 (if projection (State_simulation.t_simulation_info model) then
                    [ "show" ]
                  else
                    [ "hidden" ])));
      ]
    content

let option_label label =
  if String.length label > 10 then
    String.sub label 0 7 ^ "..."
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

let label_news tab_is_active counter =
  let last_value =
    ref
      (let simulation_info =
         State_simulation.t_simulation_info
           (React.S.value State_simulation.model)
       in
       counter simulation_info)
  in
  ReactiveData.RList.from_signal
    (React.S.l2
       (fun tab_active model ->
         if tab_active then
           []
         else (
           let simulation_info = State_simulation.t_simulation_info model in
           let v = counter simulation_info in
           if v <> !last_value && v > 0 then (
             let () = last_value := v in
             [
               Html.txt " ";
               Html.span
                 ~a:[ Html.a_class [ "label"; "label-default" ] ]
                 [ Html.txt "New" ];
             ]
           ) else
             []
         ))
       tab_is_active State_simulation.model)

let badge (counter : Api_types_j.simulation_info option -> int) =
  ReactiveData.RList.from_signal
    (React.S.map
       (fun model ->
         let simulation_info = State_simulation.t_simulation_info model in
         let count = counter simulation_info in
         if count > 0 then
           [
             Html.txt " ";
             Html.span
               ~a:[ Html.a_class [ "badge" ] ]
               [ Html.txt (string_of_int count) ];
           ]
         else
           [])
       State_simulation.model)

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

let navtabs nav_tab_id = function
  | [] | (_, Some _, _) :: _ -> Common.toss "ui_common.navtabs : missing tabs"
  | (ti, None, l) :: t ->
    Html.ul
      ~a:
        [
          Html.a_id nav_tab_id;
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

let level ?debug ?info ?notice ?warning ?error ?fatal () : 'a list =
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
  extract "debug" debug @ extract "info" info @ extract "notice" notice
  @ extract "warning" warning @ extract "error" error @ extract "fatal" fatal
  @ []

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

let create_modal ~(id : string) ~(title_label : string)
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
