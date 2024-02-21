(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let onload _ =
  let () = State_ui.onload () in
  let main = Ui_common.id_dom "main" in
  let () =
    Dom.appendChild main (Tyxml_js.To_dom.of_div (Panel_projects.content ()))
  in
  let () = Dom.appendChild main (Panel_tab.navtabs ()) in
  let () = Dom.appendChild main (Panel_tab.navcontents ()) in
  let () =
    Dom.appendChild main (Tyxml_js.To_dom.of_div (Panel_settings.content ()))
  in

  let () = Panel_projects.onload () in
  let () = Panel_tab.onload () in
  let () = Panel_settings.onload () in

  let _ =
    Dom_html.window##.onresize :=
      Dom_html.handler (fun _ ->
          let () = Panel_projects.onresize () in
          let () = Panel_tab.onresize () in
          let () = Panel_settings.onresize () in
          Js._true)
  in
  Js._true

let _ = Dom_html.window##.onload := Dom_html.handler onload
