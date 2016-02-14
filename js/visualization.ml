module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let plot_div_id = "plot-div"

let navli label active =
  let default_attributes = [ Html5.a_id ("nav"^label) ; Html5.Unsafe.string_attrib "role" "presentation" ] in
  let attributes = if active then (Html5.a_class ["active"])::default_attributes  else default_attributes in
  Html5.li ~a:attributes
           [ Html5.a ~a:[ Html5.Unsafe.string_attrib "data-toggle" "tab"
                        ; Html5.Unsafe.string_attrib "role" "tab"
                        ; Html5.Unsafe.string_attrib "aria-controls" label
                        ; Html5.a_href ("#"^label) ]
                     [ Html5.cdata label ] ]

let navtabs = Html5.ul ~a:[Html5.a_class ["nav";"nav-tabs"]
                          ; Html5.Unsafe.string_attrib "role" "tablist" ]
                       [ navli "graph" true ; navli "flux" false ; navli "log" false ]

let navtabs_dom = Tyxml_js.To_dom.of_ul navtabs

let xml =
  let navcontent label active content = Html5.div ~a:[ Html5.a_id label
                                                     ; if active then
                                                         Html5.a_class ["tab-pane";"active"]
                                                       else
                                                         Html5.a_class ["tab-pane"]
                                                     ; Html5.Unsafe.string_attrib "role" "tabpanel" ] content in
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
                                                     Storage.model_runtime_state
                                                     (fun state -> React.S.const (match Storage.get_log_buffer state with
                                                                                    Some buffer -> buffer
                                                                                  | None -> ""
                                                                                 ))) ]
                                          ]
                             ] in
  Html5.div
    ~a:[Tyxml_js.R.Html5.a_class (React.S.bind
                                     Storage.model_runtime_state
                                     (fun state -> React.S.const (match Storage.get_plot (fun () -> 10) state with
                                                                    Some _ -> ["col-md-6";"show"]
                                                                  | None  -> ["col-md-6";"hidden"]
                                                                 ))
                                 )]
    [navtabs;navcontent]

let onload () =
  let size () =
    let width = navtabs_dom##offsetWidth in
    let margin = min 20 (2*navtabs_dom##offsetWidth/10) in
    width - margin in
  let plot_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string plot_div_id))
               (fun () -> assert false) in
  React.S.l1 (fun state -> match Storage.get_plot size state with
                             None -> ()
                           | Some x ->
                              plot_div##innerHTML <- Js.string x )
             Storage.model_runtime_state
