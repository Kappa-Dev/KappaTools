open Js
open Lwt

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let plot_div_id = "plot-div"
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
                                                     ; if active then Html5.a_class ["tab-pane";"active"] else Html5.a_class ["tab-pane"]
                                                     ; Html5.Unsafe.string_attrib "role" "tabpanel" ] content in
  let navtabs = Html5.ul ~a:[Html5.a_id "navtabs"
                            ; Html5.a_class ["nav";"nav-tabs"]
                            ; Html5.Unsafe.string_attrib "role" "tablist" ]
                         [ navli "graph" true ; navli "flux" false ; navli "log" false ] in
  let navcontent = Html5.div ~a:[ Html5.a_class ["tab-content"]]
                             [ navcontent "graph" true [ Html5.div ~a:[ Html5.a_id  plot_div_id ] [ Html5.cdata "graph" ] ]
                             ; navcontent "flux" false [ Html5.cdata "flux" ]
                             ; navcontent "log" false [ Html5.cdata "log" ] ] in
                              <:html5<<div class="col-md-6">
                                      <!-- Nav tabs -->
                                      $navtabs$
                                      <!-- Tab panes -->
                                      $navcontent$
                                   </div> >>
let onload () =
  let plot_div : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string plot_div_id))
               (fun () -> assert false) in
  React.S.l1 (function
                 None -> ()
               | Some x -> plot_div##innerHTML <- Js.string x) Storage.model_plot
