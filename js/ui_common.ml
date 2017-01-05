(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module ApiTypes = Api_types_v1_j
module Html = Tyxml_js.Html5
module UIState = Ui_state

let toggle_element
  (t : Ui_simulation.t)
  (projection : Api_types_j.simulation_info option -> bool)
  (content : [< Html_types.div_content_fun ] Html.elt Html.list_wrap) =
  Html.div
    ~a:[Tyxml_js.R.Html.a_class
          (React.S.bind
             (Ui_simulation.simulation_output t)
             (fun state -> React.S.const
                 (if projection state then
                    ["show"]
                  else
                    ["hidden"])
             )
          )]
    content


let option_label label =
  if String.length label > 10 then
    (String.sub label 0 7)^"..."
  else
    label


let export_controls
    ~(export_select_id : string)
    ~(export_filename_id : string)
    ~(export_button_id : string)
    ~(export_data_label : string) =
  let export_formats : string list =
    [export_data_label]
  in
  let export_filename =
    Html.input
      ~a:[ Html.a_id export_filename_id ;
           Html.a_input_type `Text;
           Html.a_class ["form-control"];
           Html.a_placeholder "file name"]
      ()
  in
  let export_button =
    Html.button
      ~a:[ Html.a_id export_button_id
         ; Html.Unsafe.string_attrib "role" "button"
         ; Html.a_class ["btn";"btn-default";"pull-right"]
         ]
      [ Html.cdata "export" ]
  in
  let export_formats_select =
    List.map
      (fun format ->
         [%html {|<option value="|}format{|">|}(Html.cdata format){|</option>|}])
      export_formats
  in
  [%html {|<div class="row">
           <div class="col-sm-12">
           <div class="form-inline">
           <div class="form-group">
           <select class="form-control"
                   id="|}export_select_id{|"><option value="png">png</option><option value="svg">svg</option>|}export_formats_select{|</select>
                                                                                                                                      </div>
                                                                                                                                      <div class="form-group">
                                                                                                                                      <label class="checkbox-inline">
                                                                                                                                    |}[export_filename]{|
           </label>
        </div>
        <div class="form-group">
           <label class="checkbox-inline">
              |}[export_button]{|
           </label>
        </div>
     </div>
  </div>
</div>|}]

let document = Dom_html.window##.document

let default_svg_style_id = "plot-svg-style"

let save_plot_ui
    export_data
    title
    export_button_id
    export_filename_id
    export_format_id
    svg_div_id
    ?(svg_style_id = (Some default_svg_style_id))
    dat_file_extension
  =
  let export_button : Dom_html.buttonElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (document##getElementById (Js.string export_button_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_filename : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (document##getElementById (Js.string export_filename_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_format : Dom_html.inputElement Js.t =
    Js.Unsafe.coerce
      ((Js.Opt.get
          (document##getElementById (Js.string export_format_id))
          (fun () -> assert false))
       : Dom_html.element Js.t) in
  let export_button_toggle () : unit =
    let filename : string =
      Js.to_string (export_filename##.value)
    in
    let is_disabled : bool Js.t =
      Js.bool
        (String.length (String.trim filename) == 0)
    in
    let () =
      export_button##.disabled := is_disabled
    in
    ()
  in
  let () =
    export_button_toggle ()
  in
  let () =
    export_filename##.oninput :=
      Dom_html.handler
        (fun _ ->
           let () = export_button_toggle () in
           Js._true)
  in
  let () =
    export_button##.onclick :=
      Dom_html.handler
        (fun _ ->
           let suffix : string =
             Js.to_string (export_format##.value)
           in
           let filename default : string =
             let root : string =
               Js.to_string (export_filename##.value)
             in
             if String.contains root '.' then
               root
             else
               root^"."^default
           in
           let () = match suffix with
               "svg" -> Common.plotSVG svg_div_id
                          title
                          (filename "svg")
                          svg_style_id
             | "png" ->
               Common.plotPNG
                 svg_div_id
                 title
                 (filename "png")
                 svg_style_id
             | "dat" -> export_data (filename dat_file_extension)
             | f -> Common.error ("Unknown format"^f)
           in
           Js._true)
  in
  ()
let badge
    (t : Ui_simulation.t)
    (counter : Api_types_j.simulation_info option -> int)
  =
  let badge_list, badge_handle = ReactiveData.RList.create [] in
  [ Tyxml_js.R.Html.span
      (let _ = React.S.map
           (fun state ->
              let count = counter state in
              if count > 0  then
                ReactiveData.RList.set
                  badge_handle
                  [ Html.pcdata " " ;
                    Html.span
                      ~a:[ Html.a_class ["badge"] ; ]
                      [ Html.pcdata (string_of_int count) ; ] ;
                  ]
              else
                 ReactiveData.RList.set badge_handle []
           )
           (Ui_simulation.simulation_output t) in
       badge_list
      )
  ]
let arguments (key : string) : string list =
    List.map
      snd
      (List.filter
	 (fun (k,_) -> key = k)
	 Url.Current.arguments)

let version
    ?(test:'a option = None)
    ~(prod:'a)
    ~(dev:'a)
  :'a =
  let version : string list = arguments "version" in
  match (test,version) with
  | (Some test,["test"]) -> test
  | (_,["dev"]) -> dev
  | _ -> prod

let navli label active decorations =
  let default_attributes =
    [ Html.a_id ("nav"^label)
    ; Html.Unsafe.string_attrib "role" "presentation" ]
  in
  let attributes =
    if active then
      (Html.a_class ["active"])::default_attributes
    else
      default_attributes
  in
  Html.li ~a:attributes
    [ Html.a ~a:[ Html.Unsafe.string_attrib "data-toggle" "tab"
                ; Html.Unsafe.string_attrib "role" "tab"
                ; Html.Unsafe.string_attrib "aria-controls" label
                ; Html.a_href ("#"^label) ]
        (List.append [ Html.cdata label ]  decorations)
    ]

let navtabs nav_tab_id = function
  | [] -> assert false
  | (ti,l) :: t ->
    Html.ul
      ~a:[ Html.a_id nav_tab_id
         ; Html.a_class ["nav";"nav-tabs"]
         ; Html.Unsafe.string_attrib "role" "tablist" ]
      (navli ti true l :: List.map (fun (t,li) -> navli t false li) t)

let onenavcontent label active content =
  Html.div
    ~a:[ Html.a_id label
       ; if active then
           Html.a_class ["tab-pane";"active"]
         else
           Html.a_class ["tab-pane"]
       ; Html.Unsafe.string_attrib "role" "tabpanel" ] content

let navcontent ?id (classes : string list) = function
  | [] -> assert false
  | (t,c) :: l ->
    let id : [> `Id ] Html.attrib list =
      match id with
      | None -> []
      | Some id -> [ Html.a_id id ]
    in
    Html.div
      ~a:([ Html.a_class
              (["panel-content";"tab-content"]@classes) ; ]@id)
      (onenavcontent t true c ::
       List.map (fun (t,c) -> onenavcontent t false c) l)

let level
    ?debug
    ?info
    ?notice
    ?warning
    ?error
    ?fatal
    () : 'a list =
  let level : string list = arguments "level" in
  let extract key value =
    match value with
    | None -> []
    | Some value ->
     if List.mem key level then
       [value]
     else
       []
  in
  (extract "debug" debug)@
  (extract "info" info)@
  (extract "notice" notice)@
  (extract "warning" warning)@
  (extract "error" error)@
  (extract "fatal" fatal)@
  []

let features
    ?(default=[])
    (options :(string * 'a) list) : 'a list =
  let features : string list = arguments "feature" in
  let matches : 'a list =
    List.map
      snd
      (List.filter
         (fun (feature,_) -> List.mem feature features)
         options)
  in
  match matches with
  | [] -> default
  | _::_ -> matches
