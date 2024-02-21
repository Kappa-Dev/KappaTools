(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let nav_tab_id = "navtabs"

let navtabs () =
  let story_class =
    Some
      (React.S.map
         (fun s ->
           if s.State_project.model_parameters.State_project.store_trace then
             []
           else
             [ "disabled" ])
         State_project.model)
  in
  Tyxml_js.To_dom.of_ul
  @@ Ui_common.navtabs nav_tab_id
       [
         "editor", None, Tab_editor.navli ();
         "log", None, Tab_log.navli ();
         "plot", None, Tab_plot.navli ();
         "DIN", None, Tab_flux.navli ();
         "snapshot", None, Tab_snapshot.navli ();
         "outputs", None, Tab_outputs.navli ();
         "stories", story_class, Tab_stories.navli ();
         "about", None, Tab_about.navli ();
       ]

let navcontents_id : string = "navcontents"

let navcontents () =
  Tyxml_js.To_dom.of_div
  @@ Ui_common.navcontent ~id:navcontents_id []
       [
         "editor", [ "row" ], Tab_editor.content ();
         "log", [], Tab_log.content ();
         "plot", [], Tab_plot.content ();
         "DIN", [], Tab_flux.content ();
         "snapshot", [], Tab_snapshot.content ();
         "outputs", [], Tab_outputs.content ();
         "stories", [ "row" ], Tab_stories.content ();
         "about", [ "panel-scroll" ], Tab_about.content ();
       ]

let onload () =
  let () = Tab_editor.onload () in
  let () = Tab_log.onload () in
  let () = Tab_plot.onload () in
  let () = Tab_flux.onload () in
  let () = Tab_snapshot.onload () in
  let () = Tab_outputs.onload () in
  let () = Tab_stories.onload () in
  let () = Tab_about.onload () in
  ()

let onresize () =
  let () = Tab_editor.onresize () in
  let () = Tab_log.onresize () in
  let () = Tab_plot.onresize () in
  let () = Tab_flux.onresize () in
  let () = Tab_snapshot.onresize () in
  let () = Tab_outputs.onresize () in
  let () = Tab_stories.onresize () in
  let () = Tab_about.onresize () in
  ()
