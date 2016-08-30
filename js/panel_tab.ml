module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5

let nav_tab_id = "navtabs"

let navtabs =
  Tyxml_js.To_dom.of_ul @@
  Ui_common.navtabs nav_tab_id
    [ "editor",    Tab_editor.navli
    ; "plot",      Tab_plot.navli
    ; "flux",      Tab_flux.navli
    ; "snapshot",  Tab_snapshot.navli
    ; "outputs",   Tab_outputs.navli
    ; "distances", Tab_distances.navli ]

let navcontents =
  Tyxml_js.To_dom.of_div @@
  Ui_common.navcontent
    [ "editor",    Tab_editor.navcontent
    ; "plot",      Tab_plot.navcontent
    ; "flux",      Tab_flux.navcontent
    ; "snapshot",  Tab_snapshot.navcontent
    ; "outputs",   Tab_outputs.navcontent
    ; "distances", Tab_distances.navcontent ]

let onload () =
  let () = Tab_editor.onload () in
  let () = Tab_plot.onload () in
  let () = Tab_flux.onload () in
  let () = Tab_snapshot.onload () in
  let () = Tab_outputs.onload () in
  let () = Tab_distances.onload () in
  ()

let onunload () =
  Tab_editor.onunload ()
