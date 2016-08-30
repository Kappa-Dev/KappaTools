module Html5 = Tyxml_js.Html5

let prod _ =
  let main =
    Js.Opt.get (Ui_common.document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  let () = Dom.appendChild main Panel_tab.navtabs in
  let () = Dom.appendChild main Panel_tab.navcontents in
  let () = Panel_tab.onload ()
  in Js._true

let dev _ =
  let main =
    Js.Opt.get (Ui_common.document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  let () = Dom.appendChild main Panel_tab.navtabs in
  let () = Dom.appendChild main Panel_tab.navcontents in
  let () = Panel_tab.onload ()
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
