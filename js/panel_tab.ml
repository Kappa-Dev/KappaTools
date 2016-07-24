module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5

open Js

let nav_tab_id = "navtabs"

let xml =
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
      ] in
  let navcontent label active content =
    Html.div
      ~a:[ Html.a_id label
         ; if active then
             Html.a_class ["tab-pane";"active"]
           else
             Html.a_class ["tab-pane"]
         ; Html.Unsafe.string_attrib "role" "tabpanel" ] content in
  let navtabs =
    Html.ul
      ~a:[ Html.a_id nav_tab_id
         ; Html.a_class ["nav";"nav-tabs"]
         ; Html.Unsafe.string_attrib "role" "tablist" ]
      [ navli "contact"   true  Tab_contact.navli
      ; navli "plot"      false Tab_plot.navli
      ; navli "flux"      false Tab_flux.navli
      ; navli "snapshot"  false Tab_snapshot.navli
      ; navli "outputs"   false Tab_outputs.navli
      ; navli "distances" false Tab_distances.navli
      ; navli "log"       false Tab_log.navli ]
  in
  let navcontent =
    Html.div
      ~a:[ Html.a_class ["panel-content";"tab-content"]]
      [ navcontent "contact"    true  Tab_contact.navcontent
      ; navcontent "plot"       false Tab_plot.navcontent
      ; navcontent "flux"       false Tab_flux.navcontent
      ; navcontent "snapshot"   false Tab_snapshot.navcontent
      ; navcontent "outputs"    false Tab_outputs.navcontent
      ; navcontent "distances"  false Tab_distances.navcontent
      ; navcontent "log"        false Tab_log.navcontent ] in
  Html.div
    ~a:[Html.a_class ["col-md-6"]]
    [navtabs;navcontent]

let onload () =
  let () = Tab_contact.onload () in
  let () = Tab_plot.onload () in
  let () = Tab_flux.onload () in
  let () = Tab_snapshot.onload () in
  let () = Tab_outputs.onload () in
  let () = Tab_distances.onload () in
  let () = Tab_log.onload () in
  ()
