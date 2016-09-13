module ApiTypes = ApiTypes_j
module Html = Tyxml_js.Html5

let nav_tab_id = "navtabs"

let navtabs (t : Ui_simulation.t) =
  Tyxml_js.To_dom.of_ul @@
  Ui_common.navtabs nav_tab_id
    ([ "editor",    (Tab_editor.navli t)
     ; "plot",      (Tab_plot.navli t)
     ; "flux",      (Tab_flux.navli t)
     ; "snapshot",  (Tab_snapshot.navli t)
     ; "outputs",   (Tab_outputs.navli t) ]
     @
     (Ui_common.features [("distances", ("distances",Tab_distances.navli t))]))

let navcontents (t : Ui_simulation.t) =
  Tyxml_js.To_dom.of_div @@
  Ui_common.navcontent
    ([ "editor",    (Tab_editor.navcontent t)
    ; "plot",      (Tab_plot.navcontent t)
    ; "flux",      (Tab_flux.navcontent t)
    ; "snapshot",  (Tab_snapshot.navcontent t)
    ; "outputs",   (Tab_outputs.navcontent t) ]
    @
    (Ui_common.features [("distances",("distances", Tab_distances.navcontent t))]))

let controls t =
 Tyxml_js.To_dom.of_div (Tab_settings.xml t)

let onload (t : Ui_simulation.t) =
  let () = Tab_editor.onload t in
  let () = Tab_plot.onload t in
  let () = Tab_flux.onload t in
  let () = Tab_snapshot.onload t in
  let () = Tab_outputs.onload t in
  let () =
    List.iter
      (fun a -> a ())
      (Ui_common.features [("distances",fun () -> Tab_distances.onload t)])
  in
  let () = Tab_log.onload t in
  let () = Tab_settings.onload t in
  ()

let onunload () =
  Tab_editor.onunload ()
