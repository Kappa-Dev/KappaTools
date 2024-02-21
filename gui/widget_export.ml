(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

type handler = { suffix: string; label: string; export: string -> unit }

type configuration = {
  id: string;
  handlers: handler list;
  show: bool React.signal;
}

let export_format_id (configuration : configuration) : string =
  Format.sprintf "export_%s_select" configuration.id

let export_filename_id (configuration : configuration) : string =
  Format.sprintf "export_%s_filename" configuration.id

let export_button_id (configuration : configuration) : string =
  Format.sprintf "export_%s_button" configuration.id

let export_data_label (configuration : configuration) : string =
  Format.sprintf "export_%s_label" configuration.id

let export_form_id (configuration : configuration) : string =
  Format.sprintf "export_%s_form" configuration.id

let inline_content (configuration : configuration) =
  let export_filename =
    Html.input
      ~a:
        [
          Html.a_id (export_filename_id configuration);
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
          Html.a_id (export_button_id configuration);
          Html.Unsafe.string_attrib "role" "button";
          Html.a_class [ "btn"; "btn-default" ];
          Tyxml_js.R.filter_attrib (Html.a_disabled ())
            (React.S.map not configuration.show);
        ]
      [ Html.cdata "export" ]
  in
  let export_formats_select =
    List.map
      (fun handler ->
        [%html
          {|<option value="|} handler.label {|">|} (Html.cdata handler.label)
            {|</option>|}])
      configuration.handlers
  in
  [%html
    {|<div class="form-group">
             <select class="form-control" id="|}
      (export_format_id configuration)
      {|">
               |}
      export_formats_select
      {|
             </select>
             </div>
             <div class="form-group">
               <label for=|}
      (export_filename_id configuration)
      {|></label>
               |}
      [ export_filename ]
      {|
             </div>
             |}]
  @ [ export_button ]

let content configuration =
  Html.form
    ~a:
      [
        Html.a_id (export_form_id configuration); Html.a_class [ "form-inline" ];
      ]
    (inline_content configuration)

let onload (configuration : configuration) =
  let export_button : Dom_html.buttonElement Js.t =
    Ui_common.id_dom (export_button_id configuration)
  in
  let export_filename : Dom_html.inputElement Js.t =
    Ui_common.id_dom (export_filename_id configuration)
  in
  let export_format : Dom_html.selectElement Js.t =
    Ui_common.id_dom (export_format_id configuration)
  in
  let export_button_toggle () : unit =
    let is_disabled : bool Js.t =
      Js.bool
        ((not (React.S.value configuration.show))
        || export_filename##.value##trim##.length = 0)
    in
    let () = export_button##.disabled := is_disabled in
    ()
  in
  let () = export_button_toggle () in
  let () =
    export_filename##.oninput :=
      Dom_html.handler (fun _ ->
          let () = export_button_toggle () in
          Js._false)
  in
  let () =
    export_button##.onclick :=
      Dom_html.handler (fun _ ->
          let handler : handler =
            List.nth configuration.handlers export_format##.selectedIndex
          in
          let filename default : string =
            let root : string = Js.to_string export_filename##.value in
            if String.contains root '.' then
              root
            else
              root ^ "." ^ default
          in
          let () = handler.export (filename handler.suffix) in
          Js._false)
  in
  ()

let export_svg ?svg_style_id ~(svg_div_id : string) () : handler =
  {
    suffix = "svg";
    label = "svg";
    export =
      (fun filename ->
        Common.plotSVG ?plotStyleId:svg_style_id svg_div_id filename filename);
  }

let export_png ?svg_style_id ~(svg_div_id : string) () : handler =
  {
    suffix = "png";
    label = "png";
    export =
      (fun filename ->
        Common.plotPNG ?plotStyleId:svg_style_id svg_div_id filename filename);
  }

let export_json ~(serialize_json : unit -> string) : handler =
  {
    suffix = "json";
    label = "json";
    export =
      (fun filename ->
        let data = Js.string (serialize_json ()) in
        Common.saveFile ~data ~mime:"application/json" ~filename);
  }
