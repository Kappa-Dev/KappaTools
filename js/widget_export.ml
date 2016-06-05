module Html5 = Tyxml_js.Html5

type handler  =
  { suffix : string;
    label: string;
    export : string -> unit }

type configuration =
  { id : string ;
    handlers :  handler list;
    show : bool React.signal }

let export_format_id
    (configuration :  configuration) : string =
  Format.sprintf "export_%s_select" configuration.id

let export_filename_id
    (configuration :  configuration) : string =
  Format.sprintf "export_%s_filename" configuration.id

let export_button_id
    (configuration :  configuration) : string =
  Format.sprintf "export_%s_button" configuration.id

let export_data_label
    (configuration :  configuration) : string =
  Format.sprintf "export_%s_label" configuration.id

let content
    (configuration :  configuration) =
  let export_filename =
    Html5.input
      ~a:[ Html5.a_id (export_filename_id configuration)
         ; Html5.a_input_type `Text
         ; Html5.a_class ["form-control"]
         ; Html5.a_placeholder "file name" ]
      ()
  in
  let export_button =
    Html5.button
      ~a:[ Html5.a_id (export_button_id configuration)
         ; Html5.Unsafe.string_attrib "role" "button"
         ; Html5.a_class ["btn";"btn-default";"pull-right"]
         ]
      [ Html5.cdata "export" ]
  in
  let export_formats_select =
    List.map
      (fun handler ->
        <:html5<<option $list:Html5.a_value handler.label$>
           $str:handler.label$
        </option> >>)
      configuration.handlers
  in
  let xml_div =
    <:html5<<div class="col-sm-12">
     <div class="form-inline">
        <div class="form-group">
           <select class="form-control"
                   $list:Html5.a_id (export_format_id configuration)$>
              $list:export_formats_select$
           </select>
        </div>
        <div class="form-group">
           <label class="checkbox-inline">
              $export_filename$
           </label>
        </div>
        <div class="form-group">
           <label class="checkbox-inline">
              $export_button$
           </label>
        </div>
     </div>
  </div> >>
  in
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class
           (React.S.bind
              configuration.show
              (fun show ->
                React.S.const
                (if show then
                  ["row"]
                else
                  ["hidden"])
              )
           )
       ]
    [xml_div]

let onload
 (configuration :  configuration) =
  let export_button : Dom_html.buttonElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Display_common.document##getElementById
             (Js.string (export_button_id configuration))
          )
          (fun () -> assert false))
          : Dom_html.element Js.t) in
  let export_filename : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Display_common.document##getElementById
             (Js.string (export_filename_id configuration)))
          (fun () -> assert false))
          : Dom_html.element Js.t) in
  let export_format : Dom_html.selectElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (Display_common.document##getElementById
             (Js.string (export_format_id configuration)))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_button_toggle () : unit =
    let filename : string =
      Js.to_string (export_filename##value)
    in
    let is_disabled : bool Js.t =
      Js.bool
        (String.length (String.trim filename) == 0)
    in
    let () =
      export_button##disabled <- is_disabled
    in
    ()
  in
  let () =
    export_button_toggle ()
  in
  let () =
    export_filename##oninput <-
      Dom_html.handler
      (fun _ ->
        let () = export_button_toggle () in
        Js._true)
  in
  let () =
    export_button##onclick <-
      Dom_html.handler
      (fun _ ->
        let handler : handler =
          List.nth
            configuration.handlers
            (export_format##selectedIndex)
        in
        let filename default : string =
          let root : string =
            Js.to_string (export_filename##value)
          in
          if String.contains root '.' then
            root
          else
            root^"."^default
        in
        let () =
          handler.export (filename handler.suffix)
        in
        Js._true)
  in
  ()

let default_svg_style_id = "plot-svg-style"

let export_svg
    ?(svg_style_id = (Some default_svg_style_id))
    ~(svg_div_id : string)
    () : handler =
  { suffix = "svg"
  ; label = "svg"
  ; export =
      fun filename ->
        Common.plotSVG
          svg_div_id
          filename
          filename
          svg_style_id
  }

let export_png
    ?(svg_style_id = (Some default_svg_style_id))
    ~(svg_div_id : string)
    () : handler =
  { suffix = "png"
  ; label = "png"
  ; export =
      fun filename ->
        Common.plotPNG
          svg_div_id
          filename
          filename
          svg_style_id
  }

let export_json
    ~(serialize_json : unit -> string)
    : handler =
  { suffix = "json"
  ; label = "json"
  ; export =
      fun filename ->
        let data : string = serialize_json () in
        Common.saveFile
          ~data:data
          ~mime:"application/json"
          ~filename:filename
  }
