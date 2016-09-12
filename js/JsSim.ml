module Html5 = Tyxml_js.Html5

let prod _ =
  let ui_simulation : Ui_simulation.t = Ui_simulation.create () in
  let main =
    Js.Opt.get (Ui_common.document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  let () = Dom.appendChild main (Panel_tab.navtabs ui_simulation) in
  let () = Dom.appendChild main (Panel_tab.navcontents ui_simulation) in
  let () = Dom.appendChild main (Panel_tab.controls ui_simulation) in
  let () = Panel_tab.onload ui_simulation
  in Js._true

let dev _ =
  let ui_simulation : Ui_simulation.t = Ui_simulation.create () in
  let main =
    Js.Opt.get (Ui_common.document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  let () = Dom.appendChild main (Panel_tab.navtabs ui_simulation) in
  let () = Dom.appendChild main (Panel_tab.navcontents ui_simulation) in
  let () = Dom.appendChild main (Panel_tab.controls ui_simulation) in
  let () = Panel_tab.onload ui_simulation
  in Js._true

let onunload _ =
  let () = Panel_tab.onunload () in
  Js._true

let _ = Dom_html.window##.onbeforeunload :=
    Dom_html.handler onunload

let _ = Dom_html.window##.onload :=
    Dom_html.handler
      (Ui_common.version
         ~prod:prod
         ?test:None
         ~dev:dev)
