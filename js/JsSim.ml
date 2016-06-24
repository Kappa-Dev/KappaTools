open Js

open Panel_editor
open Panel_tab

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let main_container = <:html<<div class="row">
                                     $Panel_editor.xml$
                                     $Panel_tab.xml$
                             </div> >>

let onload _ =
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
               (fun () -> assert false) in
  let skeleton = Tyxml_js.To_dom.of_div main_container in
  let () = Dom.appendChild main skeleton in
  let _ = Panel_editor.onload ();
          Panel_tab.onload ()
  in Js._true

let _ = Dom_html.window##onload <- Dom_html.handler onload
