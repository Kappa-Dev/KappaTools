open Js

open Lwt
open Code
open Visualization
open Codemirror

module Html5 = Tyxml_js.Html5
let document = Dom_html.window##document
let main_container = <:html5<<div class="row">
                                     $Code.xml$
                                     $Visualization.xml$
                             </div> >>

let onload _ =
  let main =
    Js.Opt.get (document##getElementById (Js.string "main"))
               (fun () -> assert false) in
  let skeleton = Tyxml_js.To_dom.of_div main_container in
  let () = Dom.appendChild main skeleton in
  let _ = Code.onload ();
          Visualization.onload ()
  in
  Js._true

let _ = Dom_html.window##onload <- Dom_html.handler onload
