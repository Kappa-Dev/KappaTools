(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let nav_tab_id = "navtabs"

let navtabs () =
  Tyxml_js.To_dom.of_ul @@
  Ui_common.navtabs nav_tab_id
    ([ "editor",    (Tab_editor.navli ())
     ; "log",       (Tab_log.navli ())
     ; "plot",      (Tab_plot.navli ())
     ; "flux",      (Tab_flux.navli ())
     ; "snapshot",  (Tab_snapshot.navli ())
     ; "outputs",   (Tab_outputs.navli ())
     ; "about",     (Tab_about.navli ())
     ])

let navcontents_id : string =
  "navcontents"

let navcontents () =
  Tyxml_js.To_dom.of_div @@
  Ui_common.navcontent
    ~id:navcontents_id
    []
    ([ "editor",   ["row"], (Tab_editor.content ())
     ; "log",      [],      (Tab_log.content ())
     ; "plot",     [],      (Tab_plot.content ())
     ; "flux",     [],      (Tab_flux.content ())
     ; "snapshot", [],      (Tab_snapshot.content ())
     ; "outputs",  [],      (Tab_outputs.content ())
     ; "about",    ["panel-scroll"],      (Tab_about.content ())
  ])

let onload () =
  let () = Tab_editor.onload () in
  let () = Tab_log.onload () in
  let () = Tab_plot.onload () in
  let () = Tab_flux.onload () in
  let () = Tab_snapshot.onload () in
  let () = Tab_outputs.onload () in
  let () = Tab_about.onload () in
  ()

let onresize () =
  let () = Tab_editor.onresize () in
  let () = Tab_log.onresize () in
  let () = Tab_plot.onresize () in
  let () = Tab_flux.onresize () in
  let () = Tab_snapshot.onresize () in
  let () = Tab_outputs.onresize () in
  let () = Tab_about.onresize () in
  ()
