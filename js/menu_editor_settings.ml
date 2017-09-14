(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let settings_client_li_id = "settings-client-id-li"
let settings_synch_li_id = "settings-synch-li"
let settings_synch_div_id = "settings-synch-div-id"
let settings_synch_checkbox_id = "settings-synch-checkbox"
let settings_synch_label_id = "settings-synch-label"
let settings_synch_checkbox =
  Html.input
    ~a:[ Html.a_input_type `Checkbox;
         Html.a_id settings_synch_checkbox_id;
         Tyxml_js.R.filter_attrib (Html.a_checked ()) State_settings.synch]
    ()

let settings_client_id_modal_id = "settings-client-id-modal"
let settings_client_id_button_id = "settings-client-id-button"
let settings_client_id_input_id = "settings-client-id-input"

let settings_client_id_input =
  Html.input
    ~a:[ Html.a_id settings_client_id_input_id ;
         Html.a_input_type `Text ;
         Html.a_class [ "form-control" ];
         Html.a_placeholder "client id" ;
         Html.a_size 40;
       ] ()

let settings_client_id_input_dom =
  Tyxml_js.To_dom.of_input settings_client_id_input

let dropdown (model : State_runtime.model) =
  let current_id = State_runtime.spec_id  model.State_runtime.model_current in
  (List.map
     (fun (spec : State_runtime.spec) ->
        let spec_id = State_runtime.spec_id spec in
        let li_class = if current_id = spec_id  then [ "active" ] else [] in
        Html.li
          ~a:[ Html.a_class li_class ;
               Html.Unsafe.string_attrib "data-runtime-id" spec_id ; ]
          [ Html.a
              ~a:[ Html.Unsafe.string_attrib "data-runtime-id" spec_id ; ]
              [ Html.cdata (State_runtime.spec_label spec) ] ]
     )
     model.State_runtime.model_runtimes)
  @
  (match model.State_runtime.model_runtimes with
   | [] -> []
   | _::_ ->
     [ Html.li
         ~a:[ Html.Unsafe.string_attrib "role" "separator" ;
              Html.a_class [ "divider" ] ;
            ] [ ] ]
  )
  @
  [ Html.li
      [ Html.a ~a:[ Html.a_id settings_client_li_id ]
          [ Html.cdata "Client Id" ] ] ;
    Html.li
      [ Html.a ~a:[ Html.a_id settings_synch_li_id ]
          [ Html.div
              ~a:[ Html.a_id settings_synch_div_id ;
                   Html.a_class [ "checkbox-control-div" ] ]
              [ settings_synch_checkbox ;
                Html.span
                  ~a:[ Html.a_id settings_synch_label_id ;
                       Html.a_class [ "checkbox-control-label" ] ; ]
                  [ Html.cdata "Synch" ] ] ] ] ;

  ]


let content () =
  let li_list, li_handle = ReactiveData.RList.create [] in
  let _ =
    React.S.bind
      State_runtime.model
      (fun list_t ->
         let () =
           ReactiveData.RList.set
             li_handle
             (dropdown list_t)
         in
         React.S.const ())
  in
  [ Html.a
      ~a:[ Html.Unsafe.string_attrib "role" "button" ;
           Html.a_class [ "dropdown-toggle" ] ;
           Html.Unsafe.string_attrib "data-toggle" "dropdown" ;
           Html.Unsafe.string_attrib "aria-haspopup" "true" ;
           Html.Unsafe.string_attrib "aria-expanded" "false" ;
         ]
      [ Html.pcdata "Settings" ;
        Html.span ~a:[ Html.a_class ["caret"]] [ ]
      ] ;
    Tyxml_js.R.Html.ul
      ~a:[ Html.a_class [ "dropdown-menu" ] ]
      li_list ;
    Ui_common.create_modal
      ~id:settings_client_id_modal_id
      ~title_label:"Client Id"
      ~body:[[%html
              {|<div class="input-group">|}[settings_client_id_input]{|</div>|}] ;
            ]
      ~submit_label:"Update"
      ~submit:
        (Dom_html.handler
           (fun _ ->
              let settings_client_id : string = Js.to_string settings_client_id_input_dom##.value in
              let () = State_settings.set_client_id settings_client_id in
              let () =
                Common.modal
                  ~id:("#"^settings_client_id_modal_id)
                  ~action:"hide"
              in
              Js._false))
  ]

let onload () =
  (* client id update functionallity *)
  let () =
    Common.jquery_on
      ("#"^settings_client_li_id)
      "click"
      (Dom_html.handler
         (fun _ ->
            let () =
              Common.modal
                ~id:("#"^settings_client_id_modal_id)
                ~action:"show"
            in
            let () =
              settings_client_id_input_dom##.value :=
                Js.string
                  (State_settings.get_client_id ())
            in
            Js._false)) in
  let () =
    Common.jquery_on
      ("#"^settings_synch_checkbox_id)
      "change"
      (Dom_html.handler
         (fun _ ->
            let settings_synch_checkbox_dom =
              Tyxml_js.To_dom.of_input settings_synch_checkbox
            in
            let is_checked : bool =
              Js.to_bool (settings_synch_checkbox_dom##.checked)
            in
            let () = State_settings.set_synch is_checked in
            Js._false))
  in
  let () =
    Common.jquery_on
      ("#"^settings_synch_label_id)
      "click"
      (Dom_html.handler
         (fun _ ->
            let () = Common.async __LOC__ State_ui.sync in
            Js._false))
  in
  let () =
    Common.jquery_on
      ("li[data-runtime-id]")
      "click"
      (Dom_html.handler
         (fun (event : Dom_html.event Js.t)  ->
            let target : Dom_html.element Js.t Js.opt = event##.target in
            let runtime_id : Js.js_string Js.t Js.opt =
              Js.Opt.bind
                target
                (fun (element : Dom_html.element Js.t) ->
                   Common.element_data element "runtime-id")
            in
            let () =
              Js.Opt.case
                runtime_id
                (fun _ -> ())
                (fun runtime_id -> Panel_projects_controller.set_manager
                    (Js.to_string runtime_id))
            in
            Js._false))
  in
  ()
