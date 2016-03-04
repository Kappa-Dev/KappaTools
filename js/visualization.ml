module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5

open Js
open Lwt
open ApiTypes

let document = Dom_html.window##document
let plot_div_id = "plot-div"
let nav_tab_id = "navtabs"
let xml =
  let navli label active =
    let default_attributes = [ Html5.a_id ("nav"^label) ; Html5.Unsafe.string_attrib "role" "presentation" ] in
    let attributes = if active then (Html5.a_class ["active"])::default_attributes  else default_attributes in
    Html5.li ~a:attributes
             [ Html5.a ~a:[ Html5.Unsafe.string_attrib "data-toggle" "tab"
                          ; Html5.Unsafe.string_attrib "role" "tab"
                          ; Html5.Unsafe.string_attrib "aria-controls" label
                          ; Html5.a_href ("#"^label) ]
                       [ Html5.cdata label ] ] in
  let navcontent label active content = Html5.div ~a:[ Html5.a_id label
                                                     ; if active then
                                                         Html5.a_class ["tab-pane";"active"]
                                                       else
                                                         Html5.a_class ["tab-pane"]
                                                     ; Html5.Unsafe.string_attrib "role" "tabpanel" ] content in
  let navtabs = Html5.ul ~a:[Html5.a_id nav_tab_id
                            ; Html5.a_class ["nav";"nav-tabs"]
                            ; Html5.Unsafe.string_attrib "role" "tablist" ]
                         [ navli "graph" true ; navli "flux" false ; navli "log" false ] in
  let navcontent = Html5.div ~a:[ Html5.a_class ["panel-content";"tab-content"]]
                             [ navcontent "graph" true
                                          [ Html5.div ~a:[ Html5.a_id  plot_div_id ] [ Html5.cdata "graph" ] ]
                             ; navcontent "flux" false
                                          [ Html5.cdata "flux" ]
                             ; navcontent "log" false
                                          [ Html5.div
                                              ~a:[Html5.a_class ["panel-pre" ]]
                                              [ Tyxml_js.R.Html5.pcdata
                                                  (React.S.bind
                                                     UIState.model_runtime_state
                                                     (fun state -> React.S.const (match (state : ApiTypes.state option) with
                                                                                    Some state -> String.concat "" state.log_messages
                                                                                  | _ -> ""
                                                                                 ))) ]
                                          ]
                             ] in
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class (React.S.bind
                                    UIState.model_runtime_state
                                     (fun state -> React.S.const (match state with
                                                                    Some _ -> ["col-md-6";"show"]
                                                                  | None  -> ["col-md-6";"hidden"]
                                                                 ))
                                 )]
    [navtabs;navcontent]

let update_plot (plot : ApiTypes.plot option) : unit =
  match plot with
    None -> ()
  | Some plot ->
     let plot_div : Dom_html.element Js.t =
       Js.Opt.get (document##getElementById (Js.string plot_div_id))
                  (fun () -> assert false) in
     let width = plot_div##offsetWidth - 20 in
     let svg_store : Pp_svg.store = Api_data.plot_pg_store ~plot:plot
                                                           ~file:(React.S.value UIState.opened_filename)
                                                           ~title:"plot"
                                                           ~descr:(Printf.sprintf "{ model_max_time : %s , model_max_time : %d }"
                                                                                  (match React.S.value UIState.model_max_time with
                                                                                     None -> "None"
                                                                                   | Some model_max_time -> string_of_float model_max_time)
                                                                                  (React.S.value UIState.model_nb_plot))
     in
     let svg_text :string = Pp_svg.to_string ~width:width svg_store in
     plot_div##innerHTML <- Js.string svg_text

let onload () =
  let plot_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string plot_div_id))
               (fun () -> assert false) in
  React.S.l1 (fun state -> match state with
                             None -> ()
                           | Some state -> update_plot state.plot)
             UIState.model_runtime_state
