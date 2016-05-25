module ApiTypes = ApiTypes_j
module Html5 = Tyxml_js.Html5

open Js

let nav_tab_id = "navtabs"

let xml =
  let navli label active decorations =
    let default_attributes =
      [ Html5.a_id ("nav"^label)
      ; Html5.Unsafe.string_attrib "role" "presentation" ]
    in
    let attributes =
      if active then
        (Html5.a_class ["active"])::default_attributes
      else
        default_attributes
    in
    Html5.li ~a:attributes
              [ Html5.a ~a:[ Html5.Unsafe.string_attrib "data-toggle" "tab"
                           ; Html5.Unsafe.string_attrib "role" "tab"
                           ; Html5.Unsafe.string_attrib "aria-controls" label
                           ; Html5.a_href ("#"^label) ]
                  (List.append [ Html5.cdata label ]  decorations)
              ] in
  let navcontent label active content =
    Html5.div
      ~a:[ Html5.a_id label
         ; if active then
             Html5.a_class ["tab-pane";"active"]
           else
             Html5.a_class ["tab-pane"]
         ; Html5.Unsafe.string_attrib "role" "tabpanel" ] content in
  let navtabs =
    Html5.ul
      ~a:[ Html5.a_id nav_tab_id
         ; Html5.a_class ["nav";"nav-tabs"]
         ; Html5.Unsafe.string_attrib "role" "tablist" ]
      [ navli "contact"   true  Tab_contactmap.navli
      ; navli "plot"      false Tab_plot.navli
      ; navli "flux"      false Tab_fluxmap.navli
      ; navli "snapshot"  false Tab_snapshot.navli
      ; navli "outputs"   false Tab_outputs.navli
      ; navli "distances" false Tab_distances.navli
      ; navli "log"       false Tab_log.navli ]
  in
  let navcontent =
    Html5.div
      ~a:[ Html5.a_class ["panel-content";"tab-content"]]
      [ navcontent "contact"    true  Tab_contactmap.navcontent
      ; navcontent "plot"       false Tab_plot.navcontent
      ; navcontent "flux"       false Tab_fluxmap.navcontent
      ; navcontent "snapshot"   false Tab_snapshot.navcontent
      ; navcontent "outputs"    false Tab_outputs.navcontent
      ; navcontent "distances"  false Tab_distances.navcontent
      ; navcontent "log"        false Tab_log.navcontent ] in
  Html5.div
    ~a:[Html5.a_class ["col-md-6"]]
    [navtabs;navcontent]

let onload () =
  let () = Tab_contactmap.onload () in
  let () = Tab_plot.onload () in
  let () = Tab_fluxmap.onload () in
  let () = Tab_snapshot.onload () in
  let () = Tab_outputs.onload () in
  let () = Tab_distances.onload () in
  let () = Tab_log.onload () in
  ()
