open Js
open Lwt

module Html5 = Tyxml_js.Html5
let visualization_column =
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
                                                     ; Html5.Unsafe.string_attrib "role" "tabpanel" ] [ Html5.cdata label ] in
  let navtabs = Html5.ul ~a:[Html5.a_id "navtabs"
                            ; Html5.a_class ["nav";"nav-tabs"]
                            ; Html5.Unsafe.string_attrib "role" "tablist" ]
                         [ navli "graph" true ; navli "flux" false ; navli "log" false ] in
  let navcontent = Html5.div ~a:[ Html5.a_class ["tab-content"]]
                             [ navcontent "graph" true [] ; navcontent "flux" false [] ; navcontent "log" false [] ] in
                              <:html5<<div class="col-md-6">
                                      <!-- Nav tabs -->
                                      $navtabs$
                                      <!-- Tab panes -->
                                      $navcontent$
                                   </div> >>
