(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let nav_tab_id = "navtabs"

let navtabs (t : Ui_simulation.t) =
  Tyxml_js.To_dom.of_ul @@
  Ui_common.navtabs nav_tab_id
    ([ "editor",    (Tab_editor.navli t)
     ; "plot",      (Tab_plot.navli t)
     ; "flux",      (Tab_flux.navli t)
     ; "snapshot",  (Tab_snapshot.navli t)
     ; "outputs",   (Tab_outputs.navli t) ])

let navcontents_id : string = "navcontents"
let navcontents (t : Ui_simulation.t) =
  Tyxml_js.To_dom.of_div @@
  Ui_common.navcontent
    ~id:navcontents_id
    []
    ([ "editor",    (Tab_editor.navcontent t)
    ; "plot",      (Tab_plot.navcontent t)
    ; "flux",      (Tab_flux.navcontent t)
    ; "snapshot",  (Tab_snapshot.navcontent t)
    ; "outputs",   (Tab_outputs.navcontent t) ])

let controls t =
 Tyxml_js.To_dom.of_div (Tab_settings.xml t)

let onload (t : Ui_simulation.t) =
  let () = Tab_editor.onload t in
  let () = Tab_plot.onload t in
  let () = Tab_flux.onload t in
  let () = Tab_snapshot.onload t in
  let () = Tab_outputs.onload t in
  let () = Tab_log.onload t in
  let () = Tab_settings.onload t in
  ()

let onunload () =
  Tab_editor.onunload ()

let onresize t =
  let () = Tab_editor.onresize t in
  let () = Tab_plot.onresize t in
  let () = Tab_flux.onresize t in
  let () = Tab_snapshot.onresize t in
  let () = Tab_outputs.onresize t in
  let () = Tab_log.onresize t in
  let () = Tab_settings.onresize t in
  ()
